
IBdoFODICcheck <- function(con,contract, numtries=1, getdetails=FALSE) {
   # try reqContractDetails() for FODIC mitigation
   ret <- FALSE
   tmp<-list()
   if (contract$sectype=='FUT') { #For Futures, we ignore expiry and will get a list of all possible contracts, and will pick the front one.
		contract$expiry <- ""
   }
   while (numtries>0 & !ret) {
      tmp<-list()
      tmp<-reqContractDetails(conn=con, Contract=contract, verbose=TRUE)
      #ibFODICworkaroundDisconnectTriggered <- ifelse(length(tmp)>0, FALSE, TRUE)
      if (length(tmp)>0) {
         ret <- TRUE
         print('FODIC check OK')
      } else numtries <- numtries - 1
   }
   if (!getdetails) return(ret) else return(tmp)
}


IBdoFODICperiodicReq <- function(conn, Contract) {
   # launch a single req.ContractDetails request. Dont wait for replies here!
   IBrokers:::.reqContractDetails(conn, Contract, reqId = "1")
}


IBgetLiquidTime <- function(details) {
   timezoneToday    <- as.character(details[[1]]$timeZoneId)  # 'MET'
   liquidHoursToday <- strsplit(as.character(details[[1]]$liquidHours),';')[[1]][1] # liquidHours ~ '20110602:0900-1730;20110603:0900-1730'
   #liquidHoursNextday <- strsplit(as.character(details[[1]]$liquidHours),';')[[1]][2]
   liquidDate       <- strsplit(liquidHoursToday,':')[[1]][1] # '20110602'
   liquidTime       <- strsplit(liquidHoursToday,':')[[1]][2] # '0900-1730' or 'CLOSED'
   if (liquidTime=='CLOSED') return(NULL) #stop('The exchange is closed today. Stopping!')
   liquidTime1       <- strsplit(liquidTime,'-')[[1]][1] # '0900'
   liquidTime2       <- strsplit(liquidTime,'-')[[1]][2] # '1730'
   liquidStart       <- paste(liquidDate,liquidTime1,sep=' ') # '20110602 0900'
   liquidEnd         <- paste(liquidDate,liquidTime2,sep=' ') # '20110602 1730'

   aTime <- strptime(liquidStart, "%Y%m%d %H%M", tz=timezoneToday)
   bTime <- strptime(liquidEnd, "%Y%m%d %H%M", tz=timezoneToday)
   #nowTime <- Sys.time()
   #if (aTime>nowTime | nowTime>bTime) stop('Current system time indicates that we are outside of liquid trading hours for this contract. Stopping!')
   return( list(openingtime=aTime, closingtime=bTime) )

}


