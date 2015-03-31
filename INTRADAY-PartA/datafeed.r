#Create an independent R process for reading realtimeBars  and
#sharing them via a mmap structure in the desired aggregation>=5secs.
#Also saves raw csv to file in the desired aggregation.
#
#	reqRealTimeBars()
#	==> realtimeBar() event.
#	cancelRealTimeBars()

# Any error happening in this tryCatch.. will be a FATAL error. We send a WARNING email with the error text
tryCatch({

require(mmap)
require(xts)
require(IBrokers)

source('datafeed-config.r')

source('IBgetShared.r')
source('IBappendXtsToHistory.r')

source('IBemailNotify.r')
source('IBsmsNotify.r')
# if true, notification emails will be sent
ibNotificationEmail <- TRUE
# if true, notification sms will be sent
ibNotificationSMS <- FALSE



# Create the contracts and prepare the request for realtimeBars.
contractList<-fileList<-shmemList<-agrcountList<-keepcsvhistoryList<-keepoutputhistoryList  <- list()
tickerList<- 1:length(IBcontractSpecs)
for (ii in 1:length(IBcontractSpecs)) {
   if (IBcontractSpecs[[ii]]$type=='currency') {
      contractList[[ii]]  <- twsCurrency( symbol=IBcontractSpecs[[ii]]$symbol ,currency=IBcontractSpecs[[ii]]$currency)  # Currency type contract
      fileList[[ii]]      <- paste(IBdatafeedRawDir,'/',IBcontractSpecs[[ii]]$symbol,IBcontractSpecs[[ii]]$currency,'-raw.csv',sep='')
      shmemList[[ii]]     <- paste(IBdatafeedShareDir,'/',IBcontractSpecs[[ii]]$symbol,IBcontractSpecs[[ii]]$currency,'-', IBcontractSpecs[[ii]]$exchange,'-shared.bin',sep='')
   } else if (IBcontractSpecs[[ii]]$type=='stock') {
      contractList[[ii]]  <- twsSTK( symbol=IBcontractSpecs[[ii]]$symbol ,    # Stock type contract
                                     exch=IBcontractSpecs[[ii]]$exchange ,
                                     primary=IBcontractSpecs[[ii]]$primary ,  #added 1 june 2011
                                     currency=IBcontractSpecs[[ii]]$currency )
      fileList[[ii]]      <- paste(IBdatafeedRawDir,'/',IBcontractSpecs[[ii]]$symbol,'-raw.csv',sep='')
      shmemList[[ii]]     <- paste(IBdatafeedShareDir,'/',IBcontractSpecs[[ii]]$symbol,'-', IBcontractSpecs[[ii]]$exchange,'-shared.bin',sep='')
  } else if (IBcontractSpecs[[ii]]$type=='future') {
      contractList[[ii]]  <- twsFuture( symbol=IBcontractSpecs[[ii]]$symbol ,    # future type contract
                                     exch=IBcontractSpecs[[ii]]$exchange ,
                                     expiry=IBcontractSpecs[[ii]]$expiry,
                                     primary=IBcontractSpecs[[ii]]$primary ,
                                     currency=IBcontractSpecs[[ii]]$currency )
      fileList[[ii]]      <- paste(IBdatafeedRawDir,'/',IBcontractSpecs[[ii]]$symbol,'-raw.csv',sep='')
      shmemList[[ii]]     <- paste(IBdatafeedShareDir,'/',IBcontractSpecs[[ii]]$symbol,'-', IBcontractSpecs[[ii]]$exchange,'-shared.bin',sep='')
   } else stop('Unsupported type in contract definition. Has to be stock or currency')

   if ((IBcontractSpecs[[ii]]$outputbarsizeseconds %% 5) != 0  |
        IBcontractSpecs[[ii]]$outputbarsizeseconds < 5  |
        IBcontractSpecs[[ii]]$outputbarsizeseconds >= (60*60*24) ) stop('Invalid outputbarsize. has to be greater than and a multiple of 5. cannot exceed 60*60*24')
   agrcountList[[ii]] <- as.integer( IBcontractSpecs[[ii]]$outputbarsizeseconds %/% 5 )  # multiple of 5sec that we aggregate
   keepcsvhistoryList[[ii]] <- IBcontractSpecs[[ii]]$keepcsvhistory
   if (!keepcsvhistoryList[[ii]]) unlink(fileList[[ii]])
   keepoutputhistoryList[[ii]] <- IBcontractSpecs[[ii]]$keepoutputhistory
   if (keepoutputhistoryList[[ii]]) {
      tmpxts<-IBgetShared( shmemList[[ii]] )
      if (!is.null(tmpxts)) {
         #Append previously recorded content of mmap shmemList[[ii]] as xts to (existing?) SYMBOL.rdata file in IBdatafeedHistoryDir
         #Check and warn if we have data overlaps, missing columns, etc.
         if (IBcontractSpecs[[ii]]$type=='currency') {
            print(paste('Appending or creating history for ',IBcontractSpecs[[ii]]$symbol,IBcontractSpecs[[ii]]$currency),sep='')
            IBappendXtsToHistory(
                  newSeriesxts = tmpxts,
                  rdatafile = paste(IBdatafeedHistoryDir,'/',IBcontractSpecs[[ii]]$symbol,IBcontractSpecs[[ii]]$currency,'.rdata',sep=''),
                  currentSymbol = paste(IBcontractSpecs[[ii]]$symbol,IBcontractSpecs[[ii]]$currency,sep='') )
         } else if (IBcontractSpecs[[ii]]$type=='stock' | IBcontractSpecs[[ii]]$type=='future') {
            print(paste('Appending or creating history for',IBcontractSpecs[[ii]]$symbol))
            IBappendXtsToHistory(
                  newSeriesxts = tmpxts,
                  rdatafile = paste(IBdatafeedHistoryDir,'/',IBcontractSpecs[[ii]]$symbol,'.rdata',sep=''),
                  currentSymbol = IBcontractSpecs[[ii]]$symbol )
         }
      }
      rm(tmpxts)
   }
}


# Eternal loop. this enables us to cold-restart the reqRealTimeBars() should it exit because of specific
# errors that require a restart of this command. reqRealTimeBars() is only
# allowed to exit because we use a modified twsCALLBACKdatafeed.
ibShareFilesWipe <- TRUE #At the beginning of the trading day, we wipe the shared mmap files, but when
                         #restarting from interruptions during the day, we don't!!!
while(TRUE) {

# Verify all contracts, to avoid FODIC problem
tmpContractList <- contractList
tmpTimeout <- unclass(Sys.time()) # timeout init
while(length(tmpContractList)>0) {
   # Do reqContractDetails() here to avoid the 'First Order of the Day Is Cancelled' FODIC problem
   # For Futures, here we pick the front month contract, in case that expiry='' or has an invalid value
   ibFODICworkaroundDone <- FALSE # we haven't yet done anthing to avoid the 'First Order of the Day Is Cancelled' (FODIC) problem.
   while(!ibFODICworkaroundDone) {
      if (unclass(Sys.time())-tmpTimeout > 2*60*60) stop('Timeout while doing reqContractDetails() in initial contract list verification loop') #fatal timeout after 2h.
      tmp<-list()
      tmp<-reqContractDetails(conn=ibConnection, Contract=tmpContractList[[1]], verbose=TRUE)
	  if (tmpContractList[[1]]$sectype=='FUT') {
		if (length(tmp)>0) {
			if ( length(tmp)>1  ) {
				#multiple contractDetails returned. Take the first. Assume it is the front month. modify master contract list
				warning('Multiple futures contracts returned. taking the first one. Asuming it is the front month contract')
				if ( !inherits(tmp[[1]]$contract , 'twsContract') ) stop('Multiple futures contractDetials returned. But something is wrong')
print(tmp[[1]]$contract)
				contractList[[ length(contractList)-length(tmpContractList)+1 ]] <- tmp[[1]]$contract
			}
			ibFODICworkaroundDone <- TRUE
		} else {
			Sys.sleep(15)
		}
	  } else {
		if (length(tmp)>0) { ibFODICworkaroundDone <- TRUE } else {
			Sys.sleep(15)
		}
      }
   }
   tmpContractList[[1]] <- NULL
   Sys.sleep(1)
}
print('All contracts verified - datafeed good to go')


#Define the main callback handler
source('IBeWrapper.RealTimeBars.SHARED.r')
myWrapper <- IBeWrapper.RealTimeBars.SHARED(Contracts=contractList, ShareFiles=shmemList, Aggregation=agrcountList, ShareFilesWipe=ibShareFilesWipe )

#use a single call to reqRealTimeBars(...) and specify the _list_ of contracts and the _list_ of files to write CSV data to.
reqRealTimeBars(conn=ibConnection,
    Contract = contractList,
    whatToShow="MIDPOINT",  # BID, ASK, TRADES, MIDPOINT
    barSize="5",
    useRTH=TRUE,
    playback = 1,
    tickerId = tickerList,
    file = fileList ,
    verbose=TRUE,
    eventWrapper=myWrapper ,
    CALLBACK=twsCALLBACKdatafeed
)
#This call only returns if we get a specific type of error. We don't want this to happen too often
#These types of error require the reqRealTimeBars() to be reissued, otherwise data will not flow
#despite it being available.
Sys.sleep(2*60) #Wait 2 minutes before we attempt a restart
ibShareFilesWipe <- FALSE #We want to preserve previous data, so no wiping
} #End of eternal loop


},

error = function(e) {
  #There was a FATAL error. We dispatch an alert and quit.
  print(e)
  if (ibNotificationEmail & exists('IBemailMsg',mode='function')) IBemailMsg(msgBODY=paste('FATAL: DATAFEED abort!',as.character(e)), msgHEAD=paste('FATAL: DATAFEED abort! Check Error Message'))
  if (ibNotificationSMS & exists('IBsmsMsg',mode='function')) IBsmsMsg(msgBODY=paste('FATAL: DATAFEED abort!! Check Error Message'))
  quit(save='no')
}

) #end of tryCatch



