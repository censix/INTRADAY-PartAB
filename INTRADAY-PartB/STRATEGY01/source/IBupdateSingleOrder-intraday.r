
IBupdateSingleOrder<-function(portfolio, symbol, ibclientid, iborderid, status, newstatus, statustimestamp, getit=FALSE) {

    # NOTE: when the TWS/GW receives an orderId N, then ALL i<N will be causing a 'duplicate id Error' !!!
    #       In order to reset N, go to: Configure:API:Reset API order ID sequence
if (PrintAllDebug) print('in IBupdateSingleOrder')
if (PrintAllDebug) print(ibLookupOrderId)
    #lookup order_book_timestamp for orderId -  ibLookupOrderId and ibClientId are global
#~     tmp <- ifelse(!is.null(ibLookupOrderId) ,
#~                ibLookupOrderId[ ibLookupOrderId[,1]==as.character(iborderid) &
#~                                 ibLookupOrderId[,2]==as.character(ibclientid) &
#~                                 ibLookupOrderId[,3]==portfolio &
#~                                 ibLookupOrderId[,4]==symbol, 5] ,
#~                NULL)
    tmp <- IBLookupOrderId.getOBts(ibLookupOrderId, list(iborderid,ibclientid,portfolio,symbol))
 if (PrintAllDebug) print(tmp)
    if (is.null(tmp)) stop('lookup failed for orderId. invalid or data mismatch!')
    tmptimestamp<-tmp  #tmp itself is now a POSIXct timestamp
    #tmptimestamp<-as.POSIXct( strftime(Sys.time(), "%Y-%m-%d %H:%M:%OS3") )
if (PrintAllDebug) print(tmptimestamp)
    allsymbolsOrderbook <- IBgetOrderBook(portfolio)
    thisOrderbook <- allsymbolsOrderbook[[portfolio]][[symbol]]
    #singleorder<- thisOrderbook[tmptimestamp]
    singleorder<- last(thisOrderbook[tmptimestamp])  #changed 11 Aug 9:34
if (PrintAllDebug) print(paste('**',singleorder))
    #if (nrow(singleorder) == 1) {
    if (nrow(singleorder) == 1 & ifelse(is.null(status),TRUE,singleorder[,"Order.Status"] == status)) {
      if (!is.null(newstatus)) allsymbolsOrderbook[[portfolio]][[symbol]][tmptimestamp, "Order.Status"] <- newstatus
      allsymbolsOrderbook[[portfolio]][[symbol]][tmptimestamp, "Order.StatusTime"] <- as.character(statustimestamp)
      assign(paste("order_book", portfolio, sep = "."), allsymbolsOrderbook, envir = .strategy)
      if (getit) return(allsymbolsOrderbook[[portfolio]][[symbol]][tmptimestamp,]) else return(TRUE)
    } else {
		print(as.numeric(time(last(thisOrderbook))),digits=20)
		print(tmp)
		return(FALSE)
    }
}




#~ IBupdateSingleOrder<-function(portfolio, symbol, ibclientid, iborderid, status, newstatus, statustimestamp, getit=FALSE) {
#~ 
#~     # NOTE: when the TWS/GW receives an orderId N, then ALL i<N will be causing a 'duplicate id Error' !!!
#~     #       In order to reset N, go to: Configure:API:Reset API order ID sequence
#~ if (PrintAllDebug) print('in IBupdateSingleOrder')
#~ if (PrintAllDebug) print(ibLookupOrderId)
#~     #lookup order_book_timestamp for orderId -  ibLookupOrderId and ibClientId are global
#~     tmp <- ifelse(!is.null(ibLookupOrderId) ,
#~                ibLookupOrderId[ ibLookupOrderId[,1]==as.character(iborderid) &
#~                                 ibLookupOrderId[,2]==as.character(ibclientid) &
#~                                 ibLookupOrderId[,3]==portfolio &
#~                                 ibLookupOrderId[,4]==symbol, 5] ,
#~                NULL)
#~  if (PrintAllDebug) print(tmp)
#~     if (is.null(tmp)) stop('lookup failed for orderId. invalid or data mismatch!')
#~     if (length(tmp)!=1) stop('lookup failed for orderId. invalid or data mismatch!')
#~     #tmptimestamp<-as.POSIXct(tmp) ## is this causing the problems ??? no. see below
#~     #tmptimestamp<-as.POSIXct( strftime(Sys.time(), "%Y-%m-%d %H:%M:%OS3") )
#~     tmptimestamp <- strptime(tmp,format='%Y-%m-%d %H:%M:%OS')  #converts string to POSIXlt/ct, preserving microseconds properly
#~ if (PrintAllDebug) print(tmptimestamp)
#~     allsymbolsOrderbook <- IBgetOrderBook(portfolio)
#~     thisOrderbook <- allsymbolsOrderbook[[portfolio]][[symbol]]
#~     #singleorder<- thisOrderbook[tmptimestamp]
#~     singleorder<- last(thisOrderbook[tmptimestamp])  #changed 11 Aug 9:34
#~ if (PrintAllDebug) print(paste('**',singleorder))
#~     #if (nrow(singleorder) == 1) {
#~     if (nrow(singleorder) == 1 & ifelse(is.null(status),TRUE,singleorder[,"Order.Status"] == status)) {
#~       if (!is.null(newstatus)) allsymbolsOrderbook[[portfolio]][[symbol]][tmptimestamp, "Order.Status"] <- newstatus
#~       allsymbolsOrderbook[[portfolio]][[symbol]][tmptimestamp, "Order.StatusTime"] <- as.character(statustimestamp)
#~       assign(paste("order_book", portfolio, sep = "."), allsymbolsOrderbook, envir = .strategy)
#~       if (getit) return(allsymbolsOrderbook[[portfolio]][[symbol]][tmptimestamp,]) else return(TRUE)
#~     } else {
#~ 		print(as.numeric(time(last(thisOrderbook))),digits=20)
#~ 		print(tmp)
#~ 		return(FALSE)
#~     }
#~}

#in order_book:
	# 2012-08-01 13:35:35.665199 "-10"     "1"         "market"   "long" ....
#but in ibLookupOrderId:
	# "2012-08-01 13:35:35.6652"

#  POSIXct('2012-08-01 13:35:35.665199') gives us
#           2012-08-01 13:35:35.6652    ??? :(((

# http://r.789695.n4.nabble.com/POSIXct-value-display-incorrect-for-some-values-td4311446.html

#~ This is basically FAQ 7.31.  WIth floating point number, you have
#~ about 15 digits of significance, so if you look at the value:
#~
#~ >> as.numeric(as.POSIXct('2010-06-03 9:03:58.324'))
#~ > [1] 1275581038.3239998817
#~
#~ when you get down to the milliseconds, this is about as much accuracy
#~ as you will get based on using POSIXct with dates around this
#~ century..  If you need more accuracy, then maybe you only use the
#~ hours and may have to have separate values for the date and the time
#~ based on the accuracy of floating point.


# Conclusion for Sori: (32 bit system)
# set this globally:  options(digits.secs=3)
# and for the 'delay' value in IBaddOrder, use 1e-3.

# options(digits.secs=3)

#~ digits.secs:
#~     controls the maximum number of digits to print when formatting time values in seconds. Valid values are 0...6 with default 0. See strftime.
