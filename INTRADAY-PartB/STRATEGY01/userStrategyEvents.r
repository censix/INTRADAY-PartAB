
# These event functions are called by the main processing loop. This is the place were the strategy should be implemented
# Manipulation of global variables is not allowed here !
# During SHUTDOWN and other core system operations, these events are NOT called since they could interfere with an orderly shutdown process, connection loss recovery, etc.
# The allowed range for the 'delay' argument in IBaddOrder() is  1-20 * base. So with the current base of 1e-3, the range would be 1e-3 to 20e-3
# All higher delays are used by the system : partialFillCancel retry and SHUTDOWN

intraday.strategy.N <- 20
intraday.strategy.lookbackwindow <- intraday.strategy.N + 1  #number of mktdata rows required. > 0

require(TTR) #for the BBands indicator and runMax,runMin

indBBands <- function(x,columns=c('High','Low','Close'),bbN, bbSD, labels=c('dn','mavg','up',"pctB"), lagk=1) {
  #tmp<-xts( rollapply(Cl(x), width=bbandN-1, mean ,align='right') ) #Gives problems
  tmp<-lag(BBands(HLC=x[,columns], n=bbN, sd=bbSD ), lagk)
  tmp<- cbind(x,tmp)[,-(1:length(colnames(x)))]
  colnames(tmp)<-labels
  return(tmp)
}

indMax <- function(x,columns=c('High'), N, labels=c('max'), lagk=1) {
  tmp<-lag(runMax(x=x[,columns], n=N ), lagk)
  tmp<- cbind(x,tmp)[,-(1:length(colnames(x)))]
  colnames(tmp)<-labels
  return(tmp)
}

indMin <- function(x,columns=c('Low'), N, labels=c('min'), lagk=1) {
  tmp<-lag(runMin(x=x[,columns], n=N ), lagk)
  tmp<- cbind(x,tmp)[,-(1:length(colnames(x)))]
  colnames(tmp)<-labels
  return(tmp)
}


# Is called after we have received new marketdata, except during shutdown, etc.
onMarketData <- function(portfolio, symbol, IBmm, mktdataidx, timestamp, BLOCKENTRYNEW, ...) {
    dotdotdot <- list(...)
	if (mktdataidx < intraday.strategy.lookbackwindow ) return(FALSE)  #not engough data yet

	# get the tail of length intraday.strategy.lookbackwindow  of the most recent mktdata.
	mktdata <- IBmm[(mktdataidx-intraday.strategy.lookbackwindow+1):mktdataidx,]
   mktdata <- mktdata[,1:4]
   colnames(mktdata)<-c("Open","High","Low","Close")
#print(mktdata)

	# calc Indicators and Signals by adding columns to mktdata. needs to be FAST!!!
#   mktdata <- indFunc1(mktdata,...)
#   mktdata <- indFunc2(mktdata,...)
	mktdata <- cbind(mktdata, indBBands(x=mktdata, bbN=intraday.strategy.N, bbSD=1.1))
	mktdata <- cbind(mktdata, indMax(x=mktdata, N=intraday.strategy.N))
	mktdata <- cbind(mktdata, indMin(x=mktdata, N=intraday.strategy.N))
print(last(mktdata))

   # get some vars out of the storage. i.e. tradelength, trailing stops, etc.
   var1<-getit('var1')
   var2<-getit('var2')

	#make order placement decision, using mktdata, indicators, var1, var2, etc.
	curqty <- getPosQty(Portfolio = portfolio, Symbol = symbol, Date = Sys.time())
	lastmkt <- last(mktdata)
	curprice <- as.numeric(lastmkt[,'Close'])
	#
	if (curqty==0 & is.null(BLOCKENTRYNEW)) {  #Entry - only allowed if not blocking
		if (curprice > as.numeric(lastmkt[,'max']) ) {
			IBaddOrder(portfolio = portfolio, symbol = symbol,
			 timestamp = timestamp, qty = 1, price = 1.0,
			 ordertype = 'market', side = 'long', threshold = NULL,
			 status = "open", replace = FALSE, delay = 1e-3,
			 tmult = FALSE, ... = ..., TxnFees = 0)
print(lastmkt)
		}
		if (curprice < as.numeric(lastmkt[,'min']) ) {
			IBaddOrder(portfolio = portfolio, symbol = symbol,
			 timestamp = timestamp, qty = -1, price = 1.0,
			 ordertype = 'market', side = 'short', threshold = NULL,
			 status = "open", replace = FALSE, delay = 1e-3,
			 tmult = FALSE, ... = ..., TxnFees = 0)
print(lastmkt)
		}
	} else {  #Exit
		if (curqty>0 & curprice<=as.numeric(lastmkt[,'mavg']) ) {
			IBaddOrder(portfolio = portfolio, symbol = symbol,
			 timestamp = timestamp, qty = -1, price = 1.0,
			 ordertype = 'market', side = 'long', threshold = NULL,
			 status = "open", replace = FALSE, delay = 1e-3,
			 tmult = FALSE, ... = ..., TxnFees = 0)
print(lastmkt)
		}
		if (curqty<0 & curprice>=as.numeric(lastmkt[,'mavg']) ) {
			IBaddOrder(portfolio = portfolio, symbol = symbol,
			 timestamp = timestamp, qty = 1, price = 1.0,
			 ordertype = 'market', side = 'short', threshold = NULL,
			 status = "open", replace = FALSE, delay = 1e-3,
			 tmult = FALSE, ... = ..., TxnFees = 0)
print(lastmkt)
		}
	}


   # put possibly modified vars back into storage.
   setit('var1',var1)
   setit('var2',var2)
   # return value does not matter
}



# Is called after an order has been filled, except during shutdown, etc.
onOrderFill <- function(portfolio,symbol,orderId) {  #no return value needed
}



# Is called after an order has been cancelled, except during shutdown, etc.
onOrderCancel <- function(portfolio,symbol,orderId,isPartial) {  #no return value needed
   #if isPartial==TRUE a partially filled order was canceled. The system has created a new order for the remainder!! NO need to do that here.
   #Retries have to be launched here. Or do nothing an wait for the next signal.
}
