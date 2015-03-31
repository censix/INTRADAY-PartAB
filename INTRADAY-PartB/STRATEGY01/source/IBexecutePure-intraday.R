
if (file.exists('FATALERROR.txt')) {
	#if (!file.exists('FATALERROR-test.log') & file.exists('test.log') ) file.rename('test.log', 'FATALERROR-test.log') #this needs to be done outside of R
	stop('Execution blocked by unresolved fatal error. Remove the FATALERROR.txt file and the forensic FATALERROR*.rdata files, then either continue with the pre-error *rdata files or re-initialize the portfolio(all trade history will be lost if you do this)')
}

require(blotter) #includes xts
require(mmap)
require(IBrokers)
if (file.exists('bugfixes.r')) source('bugfixes.r') #

source('source/intraday.config.r')
if (file.exists('intraday.config.local.r')) source('intraday.config.local.r') #Optional local config file for redefining the defaults in 'intraday.config.r'
source('source/IBorderBook-intraday.r')  #Provides order_book functionality
source('source/IBlookupOrderId-intraday.r') #Provides helpers to manage ibLookupOrderId
source('source/IBupdateSingleOrder-intraday.r')
source('source/IBerrorHandler-intraday.r')
source('source/IBeventHandlers-intraday.r')
source('source/IBgetOrdersTimespan-intraday.r')
source('source/IBplaceOrders-intraday.r')
source('source/IBdoFODICcheck.r')
source('source/IBroundToMinTick.r')
source('userStrategyEvents.r') #The actual strategy

# Very important. use delay=1e-3 NOT 1e-5, which goes beyond our current precision limit with 15 bit double's
# See http://r.789695.n4.nabble.com/POSIXct-value-display-incorrect-for-some-values-td4311446.html
options(digits.secs=3)
# NOTE:
#~  a<-Sys.time()
#~  b<-as.POSIXct(as.character(a))
#~  a
#~ [1] "2012-08-01 15:19:03.443 CEST"
#~  b
#~ [1] "2012-08-01 15:19:03.443 CEST"
#~  a==b
#~ [1] FALSE  #since b is the tryly truncated time.




# Restore environments
#Restore existing portfolio and the account into the .blotter environment.
.blotter<-new.env()
load("portf-acct.rdata", envir=.blotter)
tmpPortfolio <- getPortfolio(intraday.core.glPortfolio)
tmpSymbols = names(tmpPortfolio$symbols)
if (length(tmpSymbols)>1) stop(paste("We currently only support one symbol in the execution portfolio "))
#Restore currency and instrument definitions into the .instrument environment.
#.instrument<-new.env()
load("instr.rdata", envir=FinancialInstrument:::.instrument)
#Restore IB contract specifications into the global environment.
load("ib.rdata")
#Restore existing OrderBook into the .strategy environment.
.strategy<-new.env()
load("strat-portf-obook.rdata", envir=.strategy)



runEventloop <- function(...) {

IBsharedRealTimeFeed <- ibDatafeedShareFile
#IBsharedRealTimeFeed <- '/home/leaf/portfolios/datafeed/datafeed-shared/EURCAD--shared.bin' #Notice FWB is omitted
#IBsharedRealTimeFeed <- '/home/leaf/portfolios/datafeed/datafeed-shared/OD7S-FWB-shared.bin'


# For Debugging
lastdebugnotificationDataTimeout <- ''
lastdebugnotificationCalendar <- ''
PrintAllDebug <<- FALSE #needs global assignment. there must be a better way to do this.

# Globals
glPortfolio <- intraday.core.glPortfolio
glAccount   <- intraday.core.glAccount
glTodayCloseoutTime   <- intraday.core.glTodayCloseoutTime
# Globals - Core parameters - DO NOT CHANGE - These were carefully calibrated.
glTimeoutSocket <- intraday.core.glTimeoutSocket
glSleeptime     <- intraday.core.glSleeptime

# orderTimeoutEvent
timeoutFill <- intraday.core.timeoutFill #in seconds


# a timer on a specific status. if the status is not NULL for longer than definterval, the timer event is triggered
#etimer1<-list(definterval=100,defnotify=60,firsttime=NULL) # timer for IBCONNECTIONLOSSSTATUS timeout: 1:40 minutes. notification after 1 minute
etimer1 <- intraday.core.etimer1

# a periodic trigger for a specific action. interval in seconds
#eperiodic1 <- list(definterval=14*60,lasttime=NULL) # timer for periodic FODIC/keepalive checking. keepalive reqs will not be done if we have SHUTDOWN,etc. PENDING
eperiodic1 <- intraday.core.eperiodic1


# dataTimeoutEvent: has to have two levels, l1 and l2, defined in terms of frequencies:
#
#	level1 = c(num. of mktdata bars1 , timeinterval1)
#	level2 = c(num. of mktdata bars2 , timeinterval2)
#  Example: We have a barsize of 15sec, so we can expect 8 bars in 2 minutes (120sec) in a healthy stream.
#	We still trust the stream enough for trading as long as we get at least 4 bars in 120 secs.
#	If we get less than that, we block the strategy from issuing new orders but continue to exit
#	an existing position normally. If we receive less than 1 bar in 10 minutes (600sec) we want
#	to force an exit for exising positions. In order to implement this we can set
#		level1 = c(count=4,interval=120)
#		level2 = c(count=1,interval=600)
#	We have to make sure that l1$count/l1$interval > l2$count/l2$interval. Then we assign events as follows.
#	if (num.bars per level1$interval  < level1$count) someDataTimeoutEvent<-'l1'
#	if (num.bars per level2$interval  < level2$count) someDataTimeoutEvent<-'l2'
#
#'defcount' is the *minimum* number of events required in 'definterval'
#'firstidx' is the current index of the oldest event that is still equal or younger than 'definterval' seconds
#'n'        is the current number of events that took place in the past 'definterval' seconds
#
#ecounterl1<-list(defcount=4,definterval=120,firstidx=0,n=0,label='level1')   # 4 bars in 2 minutes, or more
#ecounterl2<-list(defcount=1,definterval=300,firstidx=0,n=0,label='level2')   # 1 bar  in 5 minutes, or more
ecounterl1 <- intraday.core.ecounterl1
ecounterl2 <- intraday.core.ecounterl2

# for our purposes we define two ecounters and we require that l1 be of higher frequency than l2.
if (ecounterl1$defcount / ecounterl1$definterval <=  ecounterl2$defcount / ecounterl2$definterval) stop('Datatimeout event definitions not consistent.')

## some key initializations needed for using the mmap datafeed
#IBsharedRealTimeFeed <- '/home/leaf/portfolios/datafeed/datafeed-shared/EURCAD--shared.bin' #Notice FWB is omitted
extract2XTS<- function(x) na.trim( .xts( as.matrix(cbind(x$open,x$high, x$low, x$close, x$volume, x$wap, x$count)), x$timestamp), sides='right', is.na='all')
IBmm <- mmap(IBsharedRealTimeFeed, struct(timestamp=double(),open=double(),high=double(),low=double(),close=double(),volume=double(),wap=double(),count=double() ), extractFUN=extract2XTS)
IBmmNextIdx <- 0 #indices start at 1

## initializations needed for the main event processing loop
processing <- TRUE
juststarting <- TRUE #Will be set to FALSE after we have received the very first row of data after startup
SHUTDOWNSTATUS <<- NULL # NULL means everything is ok. SHUTDOWNSTATUS can only change values in the order below, top to bottom. WAIT_ entries can be skipped.
					 # It is a one-way street to shutdown.
                     #  'DO_CANCEL_ALL_THEN_CLOSE_ALL_THEN_STOP'
                     #  'WAIT_CANCEL_ALL_THEN_CLOSE_ALL_THEN_STOP'
                     #  'DO_CLOSE_ALL_THEN_STOP'
                     #  'WAIT_CLOSE_ALL_THEN_STOP'
                     #  'DO_STOP'
                     # need to use global assignment because variable can be changed by IB event processing functions
BLOCKALLNEWUNTILFOT <<- NULL # BlockNewOrdersUntilFillorTimeout. need to use global assignment because variable can be changed by IB event processing functions
BLOCKENTRYNEW <<- NULL #if!=NULL Block new orders if they are ENTRIES Possible values: NULL,'BLOCKING'
ORDERTIMEOUTSTATUS <<- NULL
IBCONNECTIONLOSSSTATUS <<- NULL  # NULL means the connection to the IB tws (or gw) and the upstream server is active and working fine.
                  # other values:
                  #   'DO_RECONNECT'   the connection has been broken and we still need to try our first re-connection
                  #   'WAIT_RECONNECT' we have made a re-connection attempt an are now waiting for it to succeed.
                  # A connection loss can be of two types. a) loss of con. to the upstream server. b) loss of con. to the local tws or gw.
                  # In case a) we can only wait for it to come back. In case b) we can try a reconnect. both of these can timeout.

if (!IBdoFODICcheck(ibConnection,ibContract, 3)) return(FALSE)  #Exit naturally if we can't verify the ibContract before processing begins
# This becomes relevant on special holidays when a market that normally closes at 17:30h already closes at 14:00, i.e. christmas.
tmpcontractdetails <- IBdoFODICcheck(ibConnection,ibContract, 1, getdetails=TRUE)
if (length(tmpcontractdetails)==0) return(FALSE)  #Exit naturally if we can't get the contract details
tmpmarkettimes <- IBgetLiquidTime(tmpcontractdetails)
if (is.null(tmpmarkettimes)) return(FALSE) #Exit naturally if the market is CLOSED today
#Exit 15 min. before market close or at glTodayCloseoutTime, whatever comes first.
glTodayCloseoutTime <- min(glTodayCloseoutTime, tmpmarkettimes$closingtime-60*15 )
print(paste('glTodayCloseoutTime:',glTodayCloseoutTime))
# For Futures contracts, we automatically roll over to the front month contract (first one in the list returned by reqContractDetails)
if (ibContract$sectype=='FUT') {
	print(paste('Futures auto-roll: Saved contract from previous session:',ibContract))
	ibContract <- tmpcontractdetails[[1]]$contract
	print(paste('Futures auto-roll: Today we are trading this contract:',ibContract))
}

while(processing) {  #wait for mktdata, IBevents and timeouts. these come from different sources. #To exit this naturally use 'break'.
	nowtimePOSIXct <- as.POSIXct( strftime(Sys.time(), "%Y-%m-%d %H:%M:%OS3") )  #System time, truncating subseconds to mili-seconds. 1e-3
	nowtime <- unclass(nowtimePOSIXct)  #System time in seconds
print(paste('Heartbeat:',nowtimePOSIXct))


	########################################################################
	# Event generation
	########################################################################
	someIBEvent <- someMKTDataEvent <- someOrderTimeoutEvent <- someDataTimeoutEvent <- someCalendarEvent<- someIBConnectionLossEvent <- someIBConnectionTimeoutEvent <- NULL

	####
	## someMKTDataEvent <- ... generated from looking at index of last element in mmap structure
	####
	# Check if we have new data and find the last index
	prevIBmmNextIdx <- IBmmNextIdx
	if (prevIBmmNextIdx>0) { if (nrow(IBmm[prevIBmmNextIdx,])>0) prevMKTtime <- time( IBmm[prevIBmmNextIdx,] ) else prevMKTtime <- NULL } else prevMKTtime <- NULL
	while(nrow(IBmm[IBmmNextIdx+1,])>0) IBmmNextIdx <- IBmmNextIdx+1
	if (IBmmNextIdx > prevIBmmNextIdx) { #We have new mktdata
print('Received MKTData')
		lastMKTtime <- time( IBmm[IBmmNextIdx,] )
		numMKTdata <- IBmmNextIdx - prevIBmmNextIdx # >1
		someMKTDataEvent <- list(startidx=prevIBmmNextIdx+1,endidx=IBmmNextIdx, num=numMKTdata, starttime=time(IBmm[prevIBmmNextIdx+1,]), endtime=lastMKTtime)
		juststarting <- FALSE
	}

	####
	## someDataTimeoutEvent <- ... generated from counting number of mktdata bars received in last x seconds interval. in levels. see above
	####
	if (!is.null(someMKTDataEvent)) { #add new mktdata events
		ecounterl1$n <- ecounterl1$n + someMKTDataEvent$num
		if (ecounterl1$firstidx==0) ecounterl1$firstidx <- someMKTDataEvent$startidx #first mktdata in definterval
	}
	## remove events that have fallen outside the interval
	if (ecounterl1$firstidx > 0 ) {
	   while( nowtime - unclass(time( IBmm[ecounterl1$firstidx,] )) > ecounterl1$definterval ) {
		   ecounterl1$firstidx <- ecounterl1$firstidx + 1
		   ecounterl1$n <- max(ecounterl1$n - 1, 0)
		   if (ecounterl1$firstidx>IBmmNextIdx) {ecounterl1$firstidx <- 0; break}  #No more events in definterval, break while
	   }
	}
	## trigger timeout event
	if (!juststarting & ecounterl1$n < ecounterl1$defcount) someDataTimeoutEvent <- ecounterl1	else someDataTimeoutEvent <- NULL

	## Now the same for l2
	if (!is.null(someMKTDataEvent)) { #add new mktdata events
		ecounterl2$n <- ecounterl2$n + someMKTDataEvent$num
		if (ecounterl2$firstidx==0) ecounterl2$firstidx <- someMKTDataEvent$startidx #first mktdata in definterval
	}
	## remove events that have fallen outside the interval
	if (ecounterl2$firstidx > 0) {
	   while(  nowtime - unclass(time( IBmm[ecounterl2$firstidx,] )) > ecounterl2$definterval ) {
		   ecounterl2$firstidx <- ecounterl2$firstidx + 1
		   ecounterl2$n <- max(ecounterl2$n - 1, 0)
		   if (ecounterl2$firstidx>IBmmNextIdx) {ecounterl2$firstidx <- 0;break}  #No more events	in definterval, break while
	   }
	}
	## trigger timeout event
	if (!juststarting & ecounterl2$n < ecounterl2$defcount) someDataTimeoutEvent <- ecounterl2 # l2 overrides l1 (per design)!!


	####
	## someOrderTimeoutEvent <- ... generated from comparing order_book timestamps and System time
	####
	unfilledordersOB <- IBgetOrdersTimespan(portfolio = glPortfolio, symbol = ibContract$symbol,
	  status = 'open', timespan = '::', ordertype = NULL, side = NULL)
	if (nrow(unfilledordersOB)>0) {
	  cancelordersOBii<- which( nowtime - unclass(index(unfilledordersOB)) > timeoutFill  )
	  if (length(cancelordersOBii)>0) {
		timestampsToCancel <- as.list( index(unfilledordersOB[cancelordersOBii]) )
		someOrderTimeoutEvent <- timestampsToCancel
print(paste(length(timestampsToCancel),'orders have timed out'))
	  } else someOrderTimeoutEvent <- NULL
	}


	####
	## someIBEvent <- readFromIBSocket( ,SocketTimeout= ...smaller than 0.5*barsize...)
	####
	con=ibConnection[[1]]
	sel<-socketSelect(list(con), write=FALSE, timeout=glTimeoutSocket)
	if (!is.null(sel)) {
		#curMsg <- readBin(con,character(),1)
      curMsg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))
		if (length(curMsg) > 0) someIBEvent <- curMsg
	}

	####
	## someCalendarEvent <- generated at a specific Date/time
	####
	if ( nowtimePOSIXct >= glTodayCloseoutTime) someCalendarEvent <- 'daily_closeout_event' # i.e. at 17:15

	########################
	# someIBConnectionLossEvent  <- generated when connection to IB tws (or gw) or upstream connection is lost.
	########################
    # upstream connection: generated in IBerrorHandler further down
    # local connection : It looks like this is not needed



	########################################################################
	# Event processing
	########################################################################




	########################
	# Marketdata events - processing
	########################
	if (!is.null(someMKTDataEvent) & is.null(BLOCKALLNEWUNTILFOT) & is.null(SHUTDOWNSTATUS) & is.null(IBCONNECTIONLOSSSTATUS)) {

newmktdata <- IBmm[someMKTDataEvent$startidx:someMKTDataEvent$endidx,]
newmktdata <- newmktdata[,1:4]
colnames(newmktdata)<-c("Open","High","Low","Close")
print('Processing Received MKTData')
#print(tail(newmktdata))
      before.numorders <- nrow(IBgetOrderBook(glPortfolio)[[glPortfolio]][[ibContract$symbol]])
      # We allow the user strategy to generate new orders. The user function MUST ensure that if BLOCKENTRYNEW!=NULL only exit orders will be generated, no entry oders
      onMarketData(portfolio = glPortfolio, symbol = ibContract$symbol, IBmm=IBmm, mktdataidx=someMKTDataEvent$endidx, timestamp = nowtimePOSIXct, BLOCKENTRYNEW=BLOCKENTRYNEW ) #Call to user function for generating orders
      after.numorders <- nrow(IBgetOrderBook(glPortfolio)[[glPortfolio]][[ibContract$symbol]])
	  if (after.numorders > before.numorders) {
		 IBplaceOrders(
			conn=ibConnection,
			Contract=ibContract,
			portfolio='PF1',
			symbol=ibContract$symbol,
			timestamp=NULL,    #nowtimePOSIXct,
			timestampidxlist=(before.numorders+1):(after.numorders) ,
			timeoutSocket=glTimeoutSocket
		 )
		 BLOCKALLNEWUNTILFOT <<- 'BlockNewOrdersUntilFillorTimeout'   # global assignment. there must be a better way to do this.
	  }
	}

	########################
	# Order Timeout events  - processing
	########################
	if (!is.null(someOrderTimeoutEvent) & is.null(ORDERTIMEOUTSTATUS) & is.null(SHUTDOWNSTATUS) & is.null(IBCONNECTIONLOSSSTATUS)) {
		#The event itself contains the timestamps to cancel. ORDERTIMEOUTSTATUS contains the corresp. orderIds AFTER we have submitted the cancellations.
		 orderIdsToCancel <- IBLookupOrderId.getOrderIds(ibLookupOrderId, someOrderTimeoutEvent, ibConnection$clientId, glPortfolio, ibContract$symbol)
		 if (length(orderIdsToCancel)==0) stop('While cancelling orders: orderId-lookup failed for timestamp. Invalid or data mismatch!')
		 print('Cancelling timed-out orders:')
		 print(orderIdsToCancel)
		 for(oid in orderIdsToCancel) { cancelOrder(twsconn=ibConnection, orderId=as.numeric(oid)) }
		 ORDERTIMEOUTSTATUS <<- orderIdsToCancel
	}

	########################
	# Data Timeout events  - processing
	########################
	if (!is.null(someDataTimeoutEvent)) {

if (lastdebugnotificationDataTimeout!=paste(someDataTimeoutEvent,collapse='-')) {
lastdebugnotificationDataTimeout<-paste(someDataTimeoutEvent,collapse='-')
print('Received Data timeout event:')
print(lastdebugnotificationDataTimeout)
}

		if (someDataTimeoutEvent$label=='level1' ) {
		   #Issue warning:
		   BLOCKENTRYNEW <<- 'BLOCKING' #Continue trading EXITs only.
		}
		if (someDataTimeoutEvent$label=='level2' ) {
			# take shutdown decision: no return possible
			if (is.null(SHUTDOWNSTATUS)) {
			   SHUTDOWNSTATUS <<- 'DO_CANCEL_ALL_THEN_CLOSE_ALL_THEN_STOP'
			}
		}



	} else { #No data timeout, data is flowing and good for trading
		if (!is.null(BLOCKENTRYNEW)) BLOCKENTRYNEW <<- NULL #Allow continuation of trading
	}




	########################
	# IB events  - processing
	########################
	if (!is.null(someIBEvent)) {
          con=ibConnection[[1]]
#		#orderFill:	{
#			#....
#			if (no_more_open_orders_in_order_book) {
#				BLOCKALLNEWUNTILFOT <- NULL
#           .....
#			}
#		}
#		#orderCancellation: {
#			#....
#			if (no_more_open_orders_in_order_book) BLOCKALLNEWUNTILFOT <- NULL
#			# Issue a BIG alert if canellations occurr while we have a forced closeout!
#		}
        if (someIBEvent == .twsIncomingMSG$OPEN_ORDER) {
          #contents <- readBin(con, character(), 84)
          contents <- .Internal(readBin(con, "character", 84L, NA_integer_, TRUE, FALSE))
          if (is.null(eventOpenOrder)) {
            cat(someIBEvent, paste(contents), "\n")
          } else eventOpenOrder(msg=someIBEvent, contents=contents, portfolio=glPortfolio, symbol=ibContract$symbol, stp=nowtimePOSIXct, ...)
        }
        if (someIBEvent == .twsIncomingMSG$ORDER_STATUS) {
          #contents <- readBin(con, character(), 11)
          contents <- .Internal(readBin(con, "character", 11L, NA_integer_, TRUE, FALSE))
          if (is.null(eventOrderStatus)) {
            cat(someIBEvent, paste(contents), "\n")
          } else eventOrderStatus(msg=someIBEvent, contents=contents, portfolio=glPortfolio, symbol=ibContract$symbol, stp=nowtimePOSIXct, ...)
        }
        if (someIBEvent == .twsIncomingMSG$EXECUTION_DATA) {
          #contents <- readBin(con, character(), 21)
          contents <- .Internal(readBin(con, "character", 24L, NA_integer_, TRUE, FALSE)) #changed from 21L to 24L
          if (is.null(eventExecutionData)) {
            cat(someIBEvent, paste(contents), "\n")
          } else eventExecutionData(msg=someIBEvent, contents=contents, portfolio=glPortfolio, symbol=ibContract$symbol, stp=nowtimePOSIXct, ...)
        }
        if (someIBEvent == .twsIncomingMSG$CONTRACT_DATA) {
          contents <- .Internal(readBin(con, "character", 28L, NA_integer_, TRUE, FALSE))
          cat(someIBEvent, paste(contents), "\n")
        }
#		#errorCodes:
        if (someIBEvent == .twsIncomingMSG$ERR_MSG) {
          #We don't want these additional error messages to cause an abort
          sorenignore=c(201,  #Order rejected - Reason:
                        202,   #Order cancelled - Reason:
                        200,   #No security definition has been found: This happens wrongly sometimes just after beginning and before end of trading day
                        135,    #Order does not exists:  - Ignoring this is harmless if we were trying to do an order cancellation for an 'open' order
                        404    #Securities need to be located first, for shorting
                       )
          defaultignore=c(165,  #Historical market Data Service query message.
                          300,  #Can't find EId with ticker Id:
                          366,  #No historical data query found for ticker id:
                          399,  #Order message error
                          2104, #A market data farm is connected.
                          2106, #A historical data farm is connected.
                          2107  #A historical data farm connection has become inactive but should be available upon demand.
                       )
          tmp <- IBCONNECTIONLOSSSTATUS
          if (!IBerrorHandler(con, verbose=2, OK = c(sorenignore, defaultignore), portfolio=glPortfolio, symbol=ibContract$symbol, clientId=as.character(ibConnection$clientId), stp=nowtimePOSIXct)) {
              cat(someIBEvent,"\n")
              twsDisconnect(ibConnection)
              stop("Unable to process IB Error event!")
          }
          if (is.null(tmp) & !is.null(IBCONNECTIONLOSSSTATUS)) { someIBConnectionLossEvent <- 'upstreamconnectionloss'; IBCONNECTIONLOSSSTATUS <<- NULL } #hack to generate event
        }
	}



	########################
	# IB Connection Loss event  - processing
	########################
	if (!is.null(someIBConnectionLossEvent)) {
		print(someIBConnectionLossEvent)
		switch(someIBConnectionLossEvent,
			upstreamconnectionloss={
				IBCONNECTIONLOSSSTATUS <<- 'WAIT_RECONNECT'
			},
			twsconnectionloss={ #Not used at the moment, since event the loss of conn. with local tws triggers a 1100 error, eading to a 'upstreamconnectionloss' event
				IBCONNECTIONLOSSSTATUS <<- 'DO_RECONNECT'
				# some action PENDING
			}
		)
		if (is.null(etimer1$firsttime)) etimer1$firsttime <- nowtime
	}


	########################
	# Calendar events  - processing
	########################
	if (!is.null(someCalendarEvent)) {

if (lastdebugnotificationCalendar!=paste(someCalendarEvent,collapse='-')) {
lastdebugnotificationCalendar<-paste(someCalendarEvent,collapse='-')
print('Received Calendar event:')
print(lastdebugnotificationCalendar)
}
		if (someCalendarEvent=='daily_closeout_event' & is.null(SHUTDOWNSTATUS)) {
         SHUTDOWNSTATUS <<- 'DO_CANCEL_ALL_THEN_CLOSE_ALL_THEN_STOP'
		}

	}


	########################
	# TEST events  - generation and processing
	# comment /disable this entire section for production use !!!!!!!
	########################

	# 1) after entry placement, before fill - shutdown - - TEST OK
	#if (!is.null(BLOCKALLNEWUNTILFOT)) {
	#if (getPosQty(Portfolio = glPortfolio, Symbol = ibContract$symbol, Date = Sys.time())==0 & BLOCKALLNEWUNTILFOT == 'BlockNewOrdersUntilFillorTimeout' &  is.null(SHUTDOWNSTATUS)) {
	#   SHUTDOWNSTATUS <<- 'DO_CANCEL_ALL_THEN_CLOSE_ALL_THEN_STOP'
	#}
	#}

	# 2) after entry placement, after fill - shutdown - TEST OK
	#if (getPosQty(Portfolio = glPortfolio, Symbol = ibContract$symbol, Date = Sys.time())!=0  & is.null(BLOCKALLNEWUNTILFOT) &  is.null(SHUTDOWNSTATUS)) {
	#   SHUTDOWNSTATUS <<- 'DO_CANCEL_ALL_THEN_CLOSE_ALL_THEN_STOP'
	#}

	# 3) after exit placement, before fill - shutdown TEST ?? condition not correct. never get triggered ??
#~ 	if (getPosQty(Portfolio = glPortfolio, Symbol = ibContract$symbol, Date = Sys.time())!=0  & !is.null(BLOCKALLNEWUNTILFOT) &  is.null(SHUTDOWNSTATUS)) {
#~        allopenordersOB <- IBgetOrdersTimespan(portfolio = glPortfolio, symbol = ibContract$symbol,
#~                status = 'open', timespan = '::', ordertype = NULL,  side = NULL)
#~        allclosedordersOB <- IBgetOrdersTimespan(portfolio = glPortfolio, symbol = ibContract$symbol,
#~                status = 'closed', timespan = '::', ordertype = NULL,  side = NULL)
#~ 	   if (nrow(allopenordersOB)==1 & nrow(allclosedordersOB)==1) SHUTDOWNSTATUS <<- 'DO_CANCEL_ALL_THEN_CLOSE_ALL_THEN_STOP'
#~ 	}

	# 4) after exit placement, after fill - shutdown - TEST OK
	#if (getPosQty(Portfolio = glPortfolio, Symbol = ibContract$symbol, Date = Sys.time())==0  & is.null(BLOCKALLNEWUNTILFOT) &  is.null(SHUTDOWNSTATUS)) {
    #   allclosedordersOB <- IBgetOrdersTimespan(portfolio = glPortfolio, symbol = ibContract$symbol,
    #           status = 'closed', timespan = '::', ordertype = NULL,  side = NULL)
	#   if (nrow(allclosedordersOB)>=2) SHUTDOWNSTATUS <<- 'DO_CANCEL_ALL_THEN_CLOSE_ALL_THEN_STOP'
	#}


	########################
	# Shutdown sequence
	########################
   if (!is.null(SHUTDOWNSTATUS)) {
	  print(SHUTDOWNSTATUS)
	  if (!is.null(IBCONNECTIONLOSSSTATUS)) { #loss of connection while in shutdown. Suspend shutdown process, to avoid generating new connection errors
         print('Connection loss during shutdown .... shutdown sequence suspended.')
	  } else {
         if (substr(SHUTDOWNSTATUS,1,3)=='DO_') {
            switch(SHUTDOWNSTATUS,
               DO_CANCEL_ALL_THEN_CLOSE_ALL_THEN_STOP={
				      unfilledordersOB <- IBgetOrdersTimespan(portfolio = glPortfolio, symbol = ibContract$symbol, status = 'open', timespan = '::')
				      if (nrow(unfilledordersOB)>0) {  # we have unfilled orders that we have to cancel before closing the position.
				        timestampsToCancel <- as.list( index(unfilledordersOB) )
					     orderIdsToCancel <- IBLookupOrderId.getOrderIds(ibLookupOrderId, timestampsToCancel, ibConnection$clientId, glPortfolio, ibContract$symbol)
				        if (length(orderIdsToCancel)==0) stop('While cancelling orders: orderId-lookup failed for timestamp. Invalid or data mismatch!')
				        if (length(orderIdsToCancel)!=nrow(unfilledordersOB)) message('lookup failed for timestamp. fewer ids than timestamps. continuing')
				        print('Cancelling unfilled orders:')
				        print(orderIdsToCancel)
				        for(oid in orderIdsToCancel) { cancelOrder(twsconn=ibConnection, orderId=as.numeric(oid)) }
				        #
				        SHUTDOWNSTATUS <<- 'WAIT_CANCEL_ALL_THEN_CLOSE_ALL_THEN_STOP'
				      } else {
				        SHUTDOWNSTATUS <<- 'DO_CLOSE_ALL_THEN_STOP'
				      }
               },
               DO_CLOSE_ALL_THEN_STOP={
					   curqty <- getPosQty(Portfolio = glPortfolio, Symbol = ibContract$symbol, Date = Sys.time())
					   if (curqty!=0) {
					   #write close all order for all current positions
							tmpLastTxnFee <- as.numeric(last(getPortfolio(glPortfolio)$symbols[[ibContract$symbol]]$txn$Txn.Fees))  #Guess Txn.Fee by using the previous one
						    IBaddOrder(portfolio = glPortfolio, symbol = ibContract$symbol,
							    timestamp = nowtimePOSIXct, qty = -curqty, price = 1.0,
							    ordertype = 'market', side = 'long', threshold = NULL,
							    status = "open", replace = FALSE, delay = 25e-3, #notice we may not be the only one to have placed orders in this cycle. so delay big. 10 should be enough
							    tmult = FALSE, ... = ..., TxnFees = tmpLastTxnFee)
					   #send: placeOrder(s)
						    IBplaceOrders(
							   conn=ibConnection,
							   Contract=ibContract,
							   portfolio=glPortfolio,
							   symbol=ibContract$symbol,
							   timestamp=nowtimePOSIXct + 25e-3,
							   singletimestamp=TRUE,
							   timeoutSocket=glTimeoutSocket
						    )
						    SHUTDOWNSTATUS <<- 'WAIT_CLOSE_ALL_THEN_STOP' # Close position! Block All new orders. Stop the event loop.
					   } else {
						   # no open orders need to be canceled and we have no position.
						   SHUTDOWNSTATUS <<- 'DO_STOP'
					   }
               },
               DO_STOP={
                  processing <- FALSE  #shutdown
               },
            )
         }
     }

   }

	########################
	# FODIC/keepalive periodic check
	########################
	if (is.null(eperiodic1$lasttime)) {
	   eperiodic1$lasttime <- nowtime
	} else {
	   if ( is.null(IBCONNECTIONLOSSSTATUS) & is.null(SHUTDOWNSTATUS) & (nowtime - eperiodic1$lasttime > eperiodic1$definterval) ) {
	      IBdoFODICperiodicReq(conn=ibConnection, Contract=ibContract)
	      eperiodic1$lasttime <- nowtime
	   }
	}


	########################
	# someIBConnectionTimeoutEvent  <- generated when connection to IB tws or gw has been inactive for some time.
	########################
	if (!is.null(IBCONNECTIONLOSSSTATUS)) {
		if ( nowtime - etimer1$firsttime > etimer1$defnotify ) print(paste('Notification: IB CONNECTION Loss. Clean SHUTDOWN not possible. Will trigger a SYSTEM ABORT in', etimer1$firsttime + etimer1$definterval - nowtime,'seconds.'))
		if ( nowtime - etimer1$firsttime > etimer1$definterval ) someIBConnectionTimeoutEvent <- TRUE
    } else {
		if (!is.null(etimer1$firsttime)) etimer1$firsttime <- NULL
    }

	########################
	# IBConnectionTimeout events  - processing
	########################
   if (!is.null(someIBConnectionTimeoutEvent) & !is.null(IBCONNECTIONLOSSSTATUS)) {
	   #twsDisconnect(ibConnection) #cleanup
      processing <- FALSE
      curqty <- getPosQty(Portfolio = glPortfolio, Symbol = ibContract$symbol, Date = Sys.time())
      if (curqty!=0) {
		   #RED ALERT notification - immediate manual action required to close existing positions
		   stop('FATAL loss of IB connectivity. We still have OPEN positions. Immediate manual action is required to close existing positions!!')
      } else {
		#GREY ALERT notification
		break   #exit processing loop immediately
      }
   }


	Sys.sleep(glSleeptime)  #Dont go much below 0.1 seconds since it will dramatically increase processor load

}  #while(processing)

}  #runEventloop <- function(...)


# Now we are ready to run the event loop.
tryCatch({
   runEventloop()
   eventloopNormalTermination <- TRUE
   },

   error = function(e) {
     print(e)
     #There was a FATAL error. We send some kind of Notification if possible and create a blocking file.
     #When FATAL errors ocurr there is the possibility of remaining open positions!!! If so we have to notify the user IMMEDIATELY!!
     curqty <- getPosQty(Portfolio = intraday.core.glPortfolio, Symbol = ibContract$symbol, Date = Sys.time()) #Note: We can only use vars that were defined outsite the event loop.
	 if (curqty!=0) {
		print('RED ALERT - RED ALERT - We still have open positions !!!!')
		print('RED ALERT - RED ALERT - We still have open positions !!!!')
		print('RED ALERT - RED ALERT - We still have open positions !!!!')
		print(paste(curqty, ibContract$symbol))
		print('Login manually IMMEDIATELY to close it.')
	 }
     #if (ibNotificationEmail & exists('IBemailMsg',mode='function')) IBemailMsg(msgBODY=paste('FATAL System Abort!',ibContract$symbol,as.character(e)), msgHEAD=paste('FATAL System Abort!',ibContract$symbol))
     #if (ibNotificationSMS & exists('IBsmsMsg',mode='function')) IBsmsMsg(msgBODY=paste('FATAL System Abort!',ibContract$symbol))
     #NEEd TO UNCOMMENT THIS LINE FOR REAL OPERATION# cat(1,file='FATALERROR.txt')   #this file needs to be manually removed in order to enable trading again
     cat(1,file='FATALERROR.txt')   #this file needs to be manually removed in order to enable trading again
   },

   finally={
     twsDisconnect(ibConnection) #cleanup
     if (!exists('eventloopNormalTermination')) filenamePrefix <- 'FATALERROR-'  else filenamePrefix<-''
     #Store/save existing OrderBook from the .strategy environment.
     save("order_book.PF1", file=paste(filenamePrefix,"strat-portf-obook.rdata",sep=''), envir=.strategy )
     #Store/save existing portfolio and the account from the .blotter environment.
     save(list=c("portfolio.PF1","account.AC1"), file=paste(filenamePrefix,"portf-acct.rdata",sep=''), envir=.blotter )
     #Store/save IB contract specifications (constant) and ibLookupOrderId (variable) from global environment.
     save(list=c('ibContract', 'ibDatafeedShareFile', 'ibLookupOrderId', '.Last.orderId' ), file=paste(filenamePrefix,"ib.rdata",sep='') )
     #Store/save entire .strategy environment for inspection purposes. Contains strategy specific vars and models
     save(list=ls(envir=.strategy), file=paste(filenamePrefix,"stratenv.rdata",sep=''), envir=.strategy )
   }
)

