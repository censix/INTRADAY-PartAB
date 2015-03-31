
## For every new incoming Xsec bar, the data is appended to the existing xts structure AND to the existing CSV file on disk
## We use one file per Symbol. 'SYMBOL.csv'
## use xts struct to do the aggregation calculations and once a bar is completed, empty the xts structure and write one bar
## to the shared mmap'ed file  'SYMBOL-mktdata.bin'
#


#Contracts has to be a list(..) of twsContracts

IBeWrapper.RealTimeBars.SHARED <- function(Contracts=list(), ShareFiles=list(), Aggregation=list(), ShareFilesWipe=FALSE) {
  eW <- eWrapper(NULL)
  
  if (!(length(Contracts)>0)) stop('Contract list cannot be empty')
  n<-length(Contracts)
  #Initalize n different mmap xts structures type'double' on disk
  #Set number of rows and calculate xts size in bytes (on disk)
  numrows<-18000 # set to 18000 ~= 17280 * 5sec to cover 24 hours. The rest is safety padding
  tmpx <- xts(matrix(data=NA_real_, nrow=numrows, ncol=7), Sys.time()+1:numrows)
  sizeinbytes<-length(coredata(tmpx)) * nbytes(struct(double())) + length(.index(tmpx)) * nbytes(struct(double()))
  rm(tmpx)
  #Create disk files and mappings
  mmappings<-list()
  for (id in 1:n) {
    #filename: SYMBOL-EXCHANGE-mktdata.bin    
    tmpfname <- ShareFiles[[id]]
    if (!file.exists(tmpfname) | ShareFilesWipe) { writeBin(raw(sizeinbytes),tmpfname) }
    mmappings[[id]] <- mmap(tmpfname, struct(timestamp=double(),open=double(),high=double(),low=double(),close=double(),volume=double(),wap=double(),count=double() ))
    if (ShareFilesWipe) {
       # Initialize values - brute
       mmappings[[id]][,1] <- NA
       mmappings[[id]][,2] <- NA
       mmappings[[id]][,3] <- NA
       mmappings[[id]][,4] <- NA
       mmappings[[id]][,5] <- NA
       mmappings[[id]][,6] <- NA
       mmappings[[id]][,7] <- NA
       mmappings[[id]][,8] <- NA
    }
  }
  #store mappings in closure
  eW$assign.Data("mmappings",mmappings)

  #Initialize the closure structures that we will use for aggregation.
  eW$assign.Data("data",
                 rep(list(structure(.xts(matrix(rep(NA_real_,7),ncol=7), 0 ),
                                    .Dimnames=list(NULL,
                                                  c("Open","High",
                                                    "Low","Close",
                                                    "Volume","WAP","Count")))),n))

  eW$assign.Data("dataempty",
                 structure(.xts(matrix(rep(NA_real_,7),ncol=7), 0 ),
                                    .Dimnames=list(NULL,
                                                  c("Open","High",
                                                    "Low","Close",
                                                    "Volume","WAP","Count"))) )

  eW$assign.Data("datacumul",
                   rep(list(structure(.xts(matrix(rep(NA_real_,7),ncol=7), 0 ),
                                    .Dimnames=list(NULL,
                                                  c("Open","High",
                                                    "Low","Close",
                                                    "Volume","WAP","Count")))),n))

  eW$assign.Data("dataaggregation", Aggregation)
  
  #Define the callback function               
  eW$realtimeBars <- function(curMsg, msg, timestamp, file, ...)
  {
    id <- as.numeric(msg[2])
#print(paste(timestamp,id))    
    file <- file[[id]]
    cat(paste(msg[3],  # timestamp in POSIXct
              msg[4],  # Open
              msg[5],  # High
              msg[6],  # Low
              msg[7],  # Close
              msg[8],  # Volume
              msg[9],  # WAP
              msg[10], # Count
              sep=","), "\n", file=file, append=TRUE)
    #single row of xts data
    data <- eW$get.Data("data")
    attr(data[[id]], "index") <- as.numeric(msg[3])
    nr.data <- NROW(data[[id]])
    data[[id]][nr.data,1:7] <- as.numeric(msg[4:10])
    #cumulative xts data
    datacumul <- eW$get.Data("datacumul")
    datacumul[[id]] <- na.trim( rbind(datacumul[[id]], data[[id]] ) , sides='left', is.na='any' )
    nr.datacumul <- NROW(datacumul[[id]])

    dataaggregation <- eW$get.Data("dataaggregation")
    if (nr.datacumul > dataaggregation[[id]] ) stop('We have missed a RealTimeBar during aggregation !!! why ???')
    #Check timestamps in data[[id]] and see if we have enough for a full bar yet
    if (nr.datacumul == dataaggregation[[id]] ) {  # 6 * 5 = 30 second aggregate
      #aggregate to make one single bar
      tt<-datacumul[[id]][,1:7]; colnames(tt) <- c('open','high','low','close','volume','wap','count')
      #to.period is giving problems, so do aggregation manually
      #newbar <- to.period(x=tt , period='seconds', k=15, indexAt='endof') 
      #newbar <- xts(t(c( first(tt)[,'open'], max(tt[,'high']), min(tt[,'low']),last(tt)[,'close'], sum(tt[,'volume']) )), order.by=index(last(tt)) )
      newbar <- xts(t(c( as.numeric(first(tt)[,'open']), 
                         max(tt[,'high']), 
                         min(tt[,'low']),
                         as.numeric(last(tt)[,'close']), 
                         ifelse( as.numeric(tt[1,'volume']) >= 0 , sum(as.numeric(tt[,'volume'])), -1 ),
                         ifelse( as.numeric(tt[1,'wap']) >= 0 , sum(as.numeric(tt[,'wap'])*as.numeric(tt[,'volume']))/sum(as.numeric(tt[,'volume'])), -1 ), 
                         ifelse( as.numeric(tt[1,'count']) >= 0 , sum(as.numeric(tt[,'count'])), -1 )
                      )), order.by=index(last(tt)) )
      # 
      #and write the newbar to shared memory as struct type 'double' with 8 columns, 
      mmappings <- eW$get.Data("mmappings")
#      #get the ii index of the first free (NA) timestamp of m[], assuming that we do not have an extraction function??
#      #Faster: we could store this index in the closure somewhere and just increment it,
#      #  instead of searching for it every time. 
      tstmps<-mmappings[[id]][,1]
#print( is.na( tstmps[[1]][1] ) )  #first element 
      iinext<-match(NA, tstmps[[1]] )  #Only works if we don't have leading NAs or interrupting NAs
      if (is.na(iinext)) stop(paste('Fatal error. Shared memory/file buffer for id',id,'is full, or invalid NAs in data.'))
      mmappings[[id]][iinext,1] <- .index(newbar)
      mmappings[[id]][iinext,2] <- coredata(newbar)[1,1]
      mmappings[[id]][iinext,3] <- coredata(newbar)[1,2]
      mmappings[[id]][iinext,4] <- coredata(newbar)[1,3]
      mmappings[[id]][iinext,5] <- coredata(newbar)[1,4]
      mmappings[[id]][iinext,6] <- coredata(newbar)[1,5]
      mmappings[[id]][iinext,7] <- NA #coredata(newbar)[1,6]
      mmappings[[id]][iinext,8] <- NA #coredata(newbar)[1,7]
      #   then reset "datacumul" 
      datacumul[[id]] <- eW$get.Data("dataempty")
    }
    
    eW$assign.Data("data", data)
    eW$assign.Data("datacumul", datacumul)
    c(curMsg, msg)
  }

  
   #Define the error function 
   eW$errorMessage <- function(curMsg, msg, timestamp, file, twsconn, ...)
   {
      if(msg[3] == "1100") {
        twsconn$connected <- FALSE
        ## Do not disconnect, since the TWS may reconnect, with a 1101, 1102 and
        ## we still want to be around when that happens!!!
      }  
      if(msg[3] %in% c("1101","1102")) twsconn$connected <- TRUE
      
      cat(as.character(Sys.time()),"TWS Message:",msg,"\n")
      
# It looks like the 2105 (lost connection) Error and a subsequent 2106 (connection ok)
# requires us to re-issue the request for marketdata !!!
# 4-2--1-2105-HMDS data farm connection is broken:euhmds2-
# 4-2--1-2106-HMDS data farm connection is OK:euhmds2-
      if(msg[3] == "2105") {return('BREAKBREAK')} 

# An error that requires re-connection is Error 420. (Happens when we request data 
# from two different ip addresses)
# 4-2-78046390-420-Invalid Real-time Query:Trading TWS session is connected from a different IP address-
      if(msg[3] == "420") {return('BREAKBREAK')} 

   }

  return(eW)
  
}


## our custimized twsCALLBACK. the only change is that it allows us to exit from the otherwise
## infinite while(TRUE) loop.

twsCALLBACKdatafeed <- function(twsCon, eWrapper, timestamp, file, playback=1, ...)
{
  if(missing(eWrapper))
    eWrapper <- eWrapper()
  con <- twsCon[[1]]

  if(inherits(twsCon, 'twsPlayback')) {
    sys.time <- NULL
    while(TRUE) {
      if(!is.null(timestamp)) {
        # MktData
        last.time <- sys.time
        sys.time <- as.POSIXct(strptime(paste(readBin(con, character(), 2), collapse=' '), timestamp))
        if(!is.null(last.time)) {
          Sys.sleep((sys.time-last.time)*playback)
        }
        #curMsg <- readBin(con, character(), 1)
        curMsg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))
        if(length(curMsg) < 1)
          next
        processMsg(curMsg, con, eWrapper, format(sys.time, timestamp), file, ...)
      } else {
        # RealTimeBars
        curMsg <- readBin(con, character(), 1)
        if(length(curMsg) < 1)
          next
        processMsg(curMsg, con, eWrapper, timestamp, file, ...)
        if(curMsg == .twsIncomingMSG$REAL_TIME_BARS) Sys.sleep(5 * playback)
      }
    }
  } 
  else { 
    while(TRUE) {
      socketSelect(list(con), FALSE, NULL)
      curMsg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))
      res <- NULL
      if(!is.null(timestamp)) {
        res <- processMsg(curMsg, con, eWrapper, format(Sys.time(), timestamp), file, twsCon, ...)
      } else {
        res <- processMsg(curMsg, con, eWrapper, timestamp, file, twsCon, ...)
      }
      #S. Added this as a breaking mechanism
      if (!is.null(res)) { if (as.character(res)=='BREAKBREAK') break }
    }
  }
}



