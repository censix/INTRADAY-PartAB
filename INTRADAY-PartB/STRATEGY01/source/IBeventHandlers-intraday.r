eventOpenOrder<-function(msg=someIBEvent, contents=contents, portfolio=glPortfolio, symbol='', stp=nowtimePOSIXct, ...) {

#IBrokers : export these function into the global environment
twsOrderState <- IBrokers:::twsOrderState
twsExecution <- IBrokers:::twsExecution


#(msg == .twsIncomingMSG$OPEN_ORDER)

  eoo <- list(
         # need to add contractId to twsContract...
              contract   = twsContract(
                             conId   = contents[3],
                             symbol  = contents[4],
                             sectype = contents[5],
                             expiry  = contents[6],
                             strike  = contents[7],
                             right   = contents[8],
                             exch    = contents[9],
                             currency= contents[10],
                             local   = contents[11],
                             combo_legs_desc = contents[66],
                             # the following are required to correctly specify a contract
                             primary = NULL,
                             include_expired = NULL,
                             comboleg = NULL,
                             multiplier = NULL
                           ),

              order      = twsOrder(
                             orderId = contents[2],   ###A integer
                             action  = contents[12],
                             totalQuantity = contents[13],
                             orderType     = contents[14],
                             lmtPrice      = contents[15],
                             auxPrice      = contents[16],
                             tif           = contents[17],
                             ocaGroup      = contents[18],
                             account       = contents[19],
                             openClose     = contents[20],
                             origin        = contents[21],
                             orderRef      = contents[22],
                             clientId      = contents[23],
                             permId        = contents[24],
                             outsideRTH    = contents[25],
                             hidden        = contents[26],
                             discretionaryAmt = contents[27],
                             goodAfterTime = contents[28],
                             # skip deprecated amount contents[29]
                             faGroup       = contents[30],
                             faMethod      = contents[31],
                             faPercentage  = contents[32],
                             faProfile     = contents[33],
                             goodTillDate  = contents[34],
                             rule80A       = contents[35],
                             percentOffset = contents[36],
                             settlingFirm  = contents[37],
                             shortSaleSlot = contents[38],
                             designatedLocation = contents[39],
                             auctionStrategy = contents[40],
                             startingPrice = contents[41],
                             stockRefPrice = contents[42],
                             delta         = contents[43],
                             stockRangeLower = contents[44],
                             stockRangeUpper = contents[45],
                             displaySize   = contents[46],
                             blockOrder    = contents[47],
                             sweepToFill   = contents[48],
                             allOrNone     = contents[49],
                             minQty        = contents[50],
                             ocaType       = contents[51],
                             eTradeOnly    = contents[52],
                             firmQuoteOnly = contents[53],
                             nbboPriceCap  = contents[54],
                             parentId      = contents[55],
                             triggerMethod = contents[56],
                             volatility    = contents[57],
                             volatilityType = contents[58],
                             deltaNeutralOrderType = contents[59],
                             deltaNeutralAuxPrice  = contents[60],
                             continuousUpdate = contents[61],
                             referencePriceType = contents[62],
                             trailStopPrice     = contents[63],
                             basisPoints        = contents[64],
                             basisPointsType    = contents[65],
                             # part of contract #66
                             scaleInitLevelSize = contents[67],
                             scaleSubsLevelSize = contents[68],
                             scalePriceIncrement = contents[69],
                             clearingAccount = contents[70],
                             clearingIntent  = contents[71],
                             notHeld         = contents[72],
                             # this contingent on UnderComp Not Yet Available in IBrokers [74+]
                             # algoStrategy [75+]
                             whatIf          = contents[75]
                           ),

              orderstate = twsOrderState(
                             status      = contents[76],  ###B string
                             initMargin  = contents[77],
                             maintMargin = contents[78],
                             equityWithLoan = contents[79],
                             commission  = contents[80],
                             minCommission = contents[81],
                             maxCommission = contents[82],
                             commissionCurrency = contents[83],
                             warningText = contents[84]
                           )
         )
  eoo <- structure(eoo, class='eventOpenOrder')
  cat("TWS OpenOrder:",
      paste("orderId=",eoo$order$orderId,sep=""),
      paste("conId=",eoo$contract$conId,sep=""),
      paste("symbol=",eoo$contract$symbol,sep=""),
      paste("status=",eoo$orderstate$status,sep=""),"\n")
  #eoo

  #   get corrresponding order_book entry
  #   set statustimestamp

}



eventExecutionData<-function(msg=someIBEvent, contents=contents, portfolio=glPortfolio, symbol='', stp=nowtimePOSIXct, ...) {

#IBrokers : export these function into the global environment
twsOrderState <- IBrokers:::twsOrderState
twsExecution <- IBrokers:::twsExecution

#(msg == .twsIncomingMSG$EXECUTION_DATA)

       version = contents[1]
       reqId   = contents[2]
       orderId   = contents[3]
       eed <- list(
              contract   = twsContract(
                             conId   = contents[4],
                             symbol  = contents[5],
                             sectype = contents[6],
                             expiry  = contents[7],
                             strike  = contents[8],
                             right   = contents[9],
                             exch    = contents[10],
                             currency= contents[11],
                             local   = contents[12],
                             # the following are required to correctly specify a contract
                             combo_legs_desc = NULL,
                             primary = NULL,
                             include_expired = NULL,
                             comboleg = NULL,
                             multiplier = NULL
                           ),
              execution  = twsExecution(orderId = orderId,
                                        execId  = contents[13],
                                        time    = contents[14],
                                        acctNumber = contents[15],
                                        exchange   = contents[16],
                                        side       = contents[17],
                                        shares     = contents[18],
                                        price      = contents[19],
                                        permId     = contents[20],
                                        clientId   = contents[21],
                                        liquidation= contents[22],
                                        cumQty     = contents[23],
                                        avgPrice   = contents[24]
                                       )
              )
  eed <- structure(eed, class="eventExecutionData")
  cat("TWS Execution:",
      paste("orderId=",eed$execution$orderId,sep=""),
      paste("time=",strptime(eed$execution$time,"%Y%m%d  %H:%M:%S"),sep=""),
      paste("side=",eed$execution$side,sep=""),
      paste("shares=",eed$execution$shares,sep=""),
      paste("symbol=",eed$contract$symbol,sep=""),
      paste("conId=",eed$contract$conId,sep=""),
      paste("price=",eed$execution$price,sep=""),"\n")
  #eed
  # NOTE: we rely on the orderStatusEvent 'Filled' to change the orderStatus now. This is because of possible partial fills.
  #
  res<-IBupdateSingleOrder(portfolio=portfolio, symbol=symbol, ibclientid=eed$execution$clientId ,iborderid=eed$execution$orderId, status=NULL, newstatus=NULL, statustimestamp=stp, getit=TRUE)
  #Finally record execution in portfolio!!
  if (!is.null(res) & !identical(res,FALSE)) {
print('Recording Execution in Portfolio')
#if (PrintAllDebug) print(res)
#if (PrintAllDebug) print(paste('TxnDate',stp))
#if (PrintAllDebug) print(paste('TxnQty',as.numeric(res[,"Order.Qty"])))
#if (PrintAllDebug) print(paste('TxnPrice',as.numeric(eed$execution$price)))
#if (PrintAllDebug) print(paste('TxnFees',as.numeric(res[,"Txn.Fees"])))
    addTxn(Portfolio = portfolio, Symbol = symbol,
      TxnDate = stp,
      TxnQty = ifelse(eed$execution$side=='BOT',1,-1) * as.numeric(eed$execution$shares), #as.numeric(res[,"Order.Qty"]) ,
      TxnPrice = as.numeric(eed$execution$price),
      ... = ... ,
      TxnFees = as.numeric(res[,"Txn.Fees"])  #Use the Txn.Fees from the order_book!
    )

print('DEBUG----portfolio and symbol ------')
print(portfolio)
print(symbol)
print('DEBUG----actually recorded transaction------')
print(last(getPortfolio(portfolio)$symbols[[symbol]]$txn ))

  } else stop('in IBorderFill: received EXECUTION_DATA but no such order in order_book!!!')


}






eventOrderStatus <- function(msg=someIBEvent, contents=contents, portfolio=glPortfolio, symbol='', stp=nowtimePOSIXct, ...) {

#IBrokers : export these function into the global environment
twsOrderState <- IBrokers:::twsOrderState
twsExecution <- IBrokers:::twsExecution

#(msg == .twsIncomingMSG$ORDER_STATUS)
   res<-NULL
   eos <- list(orderId = contents[2],  ###A integer
               status  = contents[3],  ###B string
               filled  = contents[4],  #integer
               remaining = contents[5],#integer
               averageFillPrice = contents[6],
               permId = contents[7],
               parentId = contents[8],
               lastFillPrice = contents[9],
               clientId = contents[10],
               whyHeld  = contents[11]
  )
  eos <- structure(eos,class="eventOrderStatus")
  cat("TWS OrderStatus:",
      paste("orderId=",eos$orderId,sep=""),
      paste("status=",eos$status,sep=""),
      paste("filled=",eos$filled,sep=""),
      paste("remaining=",eos$remaining,sep=""),
      paste("averageFillPrice=",eos$averageFillPrice,sep=""),"\n")
  #Make a timestamp that respects the possible offset between marketdata timestamp and local Sys.time()
  #stp<-as.POSIXct(Sys.time() - as.numeric(offsetlocal2market))
  switch(eos$status,

      Submitted={
         res<-IBupdateSingleOrder(portfolio=portfolio, symbol=symbol, ibclientid=eos$clientId ,iborderid=eos$orderId, status=NULL, newstatus=NULL, statustimestamp=stp)
      },

      Cancelled=, ApiCancelled=, Inactive={
		 isPartial <- FALSE
         res<-IBupdateSingleOrder(portfolio=portfolio, symbol=symbol, ibclientid=eos$clientId ,iborderid=eos$orderId, status=NULL, newstatus='canceled', statustimestamp=stp, getit=TRUE)
		 if (as.numeric(eos$filled)!=0 & as.numeric(eos$remaining)!=0 & is.null(SHUTDOWNSTATUS)) { #Cancelling a partially filled order. We create a new identical order for the remaining quanitity !
			isPartial <- TRUE
			if (intraday.core.policy.PartialFillCancellation != 'RetryRemainder') stop('This policy for partial Fill cancellation has not been implemented.')
print('Cancelled a partially filled order. Creating a new order for the remainder')
		    #Create a new order for the remainder. should be the exact copy of the original order, except for the quantity. Make sure sign() is the same as original !!!
		    tmpThreshold <- as.numeric(res$Order.Threshold)
		    if (is.na(tmpThreshold)) tmpThreshold <- NULL
			IBaddOrder(portfolio = portfolio, symbol = symbol,
				timestamp = stp, qty = sign(as.numeric(res$Order.Qty))*as.numeric(eos$remaining) , price = as.numeric(res$Order.Price),
				ordertype = as.character(res$Order.Type), side = as.character(res$Order.Side), threshold = tmpThreshold ,
				status = "open", replace = FALSE, delay = 22e-3, #notice we may not be the only one to have placed orders in this cycle. so delay big.
				tmult = FALSE, ... = ..., TxnFees = as.numeric(res$Txn.Fees))  #we assume that all orders are submitted as tmult=FALSE !!!???!!!
		    #send: placeOrder(s)
			IBplaceOrders(
			   conn=ibConnection,
			   Contract=ibContract,
			   portfolio=portfolio,
			   symbol=symbol,
			   timestamp=stp + 22e-3,  #needs to be the same as IBaddOrder()
			   singletimestamp=TRUE,
			   timeoutSocket=glTimeoutSocket
			)
		 }
		 #
         if (!is.null(ORDERTIMEOUTSTATUS)) {
			   idx<-match(eos$orderId, ORDERTIMEOUTSTATUS)
			   if (!is.na(idx)) ORDERTIMEOUTSTATUS <<- ORDERTIMEOUTSTATUS[-idx]
			   if (length(ORDERTIMEOUTSTATUS)==0) ORDERTIMEOUTSTATUS <<- NULL
         }
         if (!is.null(BLOCKALLNEWUNTILFOT)) { #see if we can unblock by confirming that no open orders remain...
           allopenordersOB <- IBgetOrdersTimespan(portfolio = portfolio, symbol = symbol,
               status = 'open', timespan = '::', ordertype = NULL,  side = NULL)  # timespan = paste('::',as.character(stp),sep='')
           if (nrow(allopenordersOB)==0) BLOCKALLNEWUNTILFOT <<- NULL  #global assignment!! there must be a better way to do this.
         }
         if (!is.null(SHUTDOWNSTATUS)) {
           allopenordersOB <- IBgetOrdersTimespan(portfolio = portfolio, symbol = symbol,
               status = 'open', timespan = '::', ordertype = NULL,  side = NULL)  # timespan = paste('::',as.character(stp),sep='')
           if (nrow(allopenordersOB)==0 & SHUTDOWNSTATUS=='WAIT_CANCEL_ALL_THEN_CLOSE_ALL_THEN_STOP')   SHUTDOWNSTATUS <<-  'DO_CLOSE_ALL_THEN_STOP'
		     if (nrow(allopenordersOB)==0 & SHUTDOWNSTATUS=='WAIT_CLOSE_ALL_THEN_STOP' & getPosQty(Portfolio=portfolio, Symbol=symbol, Date=Sys.time()) == 0)   SHUTDOWNSTATUS <<- 'DO_STOP'
         } else {
            onOrderCancel(portfolio = portfolio, symbol = symbol, orderId = eos$orderId, isPartial = isPartial) #Call to user function
         }
      },

      Filled={
		#
		if (as.numeric(eos$remaining)==0) res<-IBupdateSingleOrder(portfolio=portfolio, symbol=symbol, ibclientid=eos$clientId ,iborderid=eos$orderId, status=NULL, newstatus='closed', statustimestamp=stp, getit=FALSE)
		#
		if (!is.null(res) & !identical(res,FALSE)) {
print(BLOCKALLNEWUNTILFOT)
print(SHUTDOWNSTATUS)
         if (!is.null(ORDERTIMEOUTSTATUS)) {
			   idx<-match(eos$orderId, ORDERTIMEOUTSTATUS)
			   if (!is.na(idx)) ORDERTIMEOUTSTATUS <<- ORDERTIMEOUTSTATUS[-idx]
			   if (length(ORDERTIMEOUTSTATUS)==0) ORDERTIMEOUTSTATUS <<- NULL
         }
			if (!is.null(BLOCKALLNEWUNTILFOT)) { #see if we can unblock by confirming that no open orders remain...
				allopenordersOB <- IBgetOrdersTimespan(portfolio = portfolio, symbol = symbol,
					status = 'open', timespan = '::', ordertype = NULL,  side = NULL)  # timespan = paste('::',as.character(stp),sep='')
				if (nrow(allopenordersOB)==0) BLOCKALLNEWUNTILFOT <<- NULL  #global assignment!! there must be a better way to do this.
			}
			if (!is.null(SHUTDOWNSTATUS)) {
				allopenordersOB <- IBgetOrdersTimespan(portfolio = portfolio, symbol = symbol,
					status = 'open', timespan = '::', ordertype = NULL,  side = NULL)  # timespan = paste('::',as.character(stp),sep='')
				if (nrow(allopenordersOB)==0 & SHUTDOWNSTATUS=='WAIT_CANCEL_ALL_THEN_CLOSE_ALL_THEN_STOP')   SHUTDOWNSTATUS <<-  'DO_CLOSE_ALL_THEN_STOP'
				if (nrow(allopenordersOB)==0 & SHUTDOWNSTATUS=='WAIT_CLOSE_ALL_THEN_STOP' & getPosQty(Portfolio=portfolio, Symbol=symbol, Date=Sys.time()) == 0)   SHUTDOWNSTATUS <<- 'DO_STOP'
			} else {
			   onOrderFill(portfolio = portfolio, symbol = symbol, orderId = eos$orderId) #Call to user function
			}
		} else stop('in IBorderFill: received ORDER_STATUS Filled but no such order in order_book!!!')

      }
  )
  if (!is.null(res) & identical(res,FALSE)) stop(paste('in IBorderFill: Could not update order_book for orderId ',eos$orderId))
}


