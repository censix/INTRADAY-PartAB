IBplaceOrders <- function(
         conn,
         Contract,
         portfolio,
         symbol,
         timestamp,
         timestampidxlist=NULL,
         singletimestamp=FALSE,
         timeoutSocket
         ) {

#print(IBgetOrderBook('PF1'))
#print(paste('*****',as.character(timestamp)))
  if (!is.null(timestampidxlist)) {
	tmporderbook <- IBgetOrderBook(portfolio)
	newordersOB <- tmporderbook[[portfolio]][[symbol]][timestampidxlist,]
  } else {
	  if (singletimestamp) {
	  newordersOB <- IBgetOrdersTimespan(portfolio = portfolio, symbol = symbol,
		  status = 'open', timespan = timestamp , ordertype = NULL,
		  side = NULL)
	  } else {
	  newordersOB <- IBgetOrdersTimespan(portfolio = portfolio, symbol = symbol,
		  status = 'open', timespan = paste(as.character(timestamp),'::',sep=''), ordertype = NULL,
		  side = NULL)
	  }
  }

  if (nrow(newordersOB)>0) {   #Start. we have new orders to place
print(paste('placing',nrow(newordersOB),'orders'))
     for(ii in 1:nrow(newordersOB)) {  #Start of order placement loop
       tmpneworder <- newordersOB[ii,]
       #Create the appropriate object of type 'twsOrder' for each new order
       #Order <- ....
print(tmpneworder)
         #    Order.Qty Order.Price Order.Type Order.Side Order.Threshold Order.Status  Order.StatusTime Order.Set Txn.Fees
         #[1,] "0"       NA          "init"     "long"     "0"             "closed"     "2000-06-14"     "1"       "0"
         #[2,] "2"       "6434.52"   "market"   "long"     NA              "closed"     "2010-10-14"     NA        "-4"
         #     "-3"      "6663.24"   "market"   "long"     NA              "closed"     "2010-11-17"     NA        "-4"

       #allsymbolsOrderbook <- getOrderBook(portfolio)
       #thisOrderbook <- allsymbolsOrderbook[[portfolio]][[symbol]]
       if (!exists('ibLookupOrderId')) stop('Could not find ibLookupOrderId')
if (PrintAllDebug) print(ibLookupOrderId)
       #This requires a reset to the API orderId sequence before every run, so not feasible
       #.Last.orderId <- ifelse( !is.null(ibLookupOrderId) , as.numeric(ibLookupOrderId[nrow(ibLookupOrderId),1]), 0 )
       #This will work fine if we are the only API client that is placing new orders. If there are more, then we have to
       #get rid of the 'if' and do a plain: ".Last.orderId <- reqIds(conn)" every time !
       #.Last.orderId <- ifelse( !is.null(ibLookupOrderId) , max(as.integer(ibLookupOrderId[,1])), as.integer(reqIds(conn)) ) #creates unwanted connection
       #.Last.orderId <- ifelse( !is.null(ibLookupOrderId) , max( ibLookupOrderId$maxorderid, as.integer(conn$nextValidId)-1L ), as.integer(conn$nextValidId)-1L )
       .Last.orderId <- ifelse( !is.null(ibLookupOrderId) , max( ibLookupOrderId$maxorderid, as.integer(reqIds(conn))-1L ), as.integer(reqIds(conn))-1L ) #api976       
if (PrintAllDebug) print('.Last.orderId')
if (PrintAllDebug) print(.Last.orderId)
       tws.orderId<- .Last.orderId + 1 #This only works in conjunction with ibLookuoOrderId
       # NOTE: when the TWS/GW receives an orderId N, then ALL i<N will be causing a 'duplicate id Error' !!!
       #       In order to reset N, go to: Configure:API:Reset API order ID sequence
if (PrintAllDebug) print('tws.orderId')
if (PrintAllDebug) print(tws.orderId)

       #Is the limit logic all right?
       if (as.numeric(tmpneworder$Order.Qty) > 0) {
         tws.action<-'BUY'
         tws.totalQuantity<- as.character(tmpneworder$Order.Qty)
       } else if (as.numeric(tmpneworder$Order.Qty) < 0) {
         tws.action<-'SELL'
         tws.totalQuantity<- as.character(-1L*as.integer(tmpneworder$Order.Qty))
       } else {
         print(tmpneworder$Order.Qty)
         print(tmpneworder$Order.Side)
         stop('IBplaceOrders: This Order.Qty and Order.Side combination is not supported')
       }
       

       # Defaults
       tws.tif <-           'DAY'
       tws.trailStopPrice <-""
       tws.parentId <-      "0"
       tws.percentOffset <- ""
       tws.ocaGroup <-      ""
       tws.ocaType <-       "0"
#       tws.goodAfterTime <- ""  #format is "YYYYMMDD HH:MM:SS" - Changed since some exchanges have stopped accepting DAY orders that are placed before market open. 23.May 2012
       ## The 'goodAfterTime' argument is not supported for generic combos (joder!!)
       #tws.goodAfterTime <- paste(format(Sys.time(), format="%Y%m%d"),'09:01:01',sep=' ')  #"YYYYMMDD HH:MM:SS"  #PENDING - make this time an ibConstant, dependent on the exchange.
       # <mainOT>_<subOT>  . <subOT> can be empty
       tmpOT <- strsplit(tmpneworder$Order.Type,split='_',fixed=TRUE)[[1]]  # "limit.ocaexit_92EXIT093325" -> limit.ocaexit + 92EXIT093325 
       mainOT <- tmpOT[1]   # "limit.ocaexit"
       subOT  <- tmpOT[-1]  # "92EXIT093325" or ''
       #
       switch( mainOT,
         market ={
                 tws.orderType<-"MKT"
                 },
         limit ={
                tws.orderType<-"LMT"
                },
         stoplimit ={
                tws.orderType<-"STPLMT"
                },
         stoptrailing ={
                tws.orderType<-"TRAIL"
                },
#         stoptrailingattached1 ={   #When we specif a parentId, IB ignores the OCAGroup, so we cannot combine this with oca ....grrrmpgfg
#                tws.orderType<-"TRAIL"
#                tws.parentId <- tmpPreviousOrderId #CAUTION: we attach this order to the previous one in the orderbook
#                },
#         stoptrailinggtcocaexit ={
#                tws.orderType<-"TRAIL"
#                tws.tif <- 'GTC'
#                tws.ocaGroup <- paste("EXITGROUP",conn$clientId,symbol,sep='')
#                },
         limit.ocaexit ={
                tws.orderType<-"LMT"
                if (length(subOT)>0) tws.ocaGroup <- subOT else stop('ordertype subOT expected') 
                },
         stop.ocaexit ={
                tws.orderType<-"STP"
                if (length(subOT)>0) tws.ocaGroup <- subOT else stop('ordertype subOT expected') 
                },
         market.ocaexit ={  
                tws.orderType<-"MKT"
                if (length(subOT)>0) tws.ocaGroup <- subOT else stop('ordertype subOT expected') 
                },
         {print(mainOT)
          stop('IBplaceOrders: This main Order.Type is not supported')
         }
       )

#       #
       if ((as.character(mainOT) %in% c("limit", "limit.ocaexit")) && as.numeric(tmpneworder$Order.Price) > 0 ) {
         tws.lmtPrice <- as.character(tmpneworder$Order.Price)
       } else if (as.character(mainOT) %in% c("market", "market.ocaexit")) {
         tws.lmtPrice <- "0.0"
       } else {
         stop('IBplaceOrders: Order.Price and Order.Type combination is not supported')
       }
#       #

       Order <- twsOrder(orderId=tws.orderId,
            action=tws.action, #action="BUY",
            totalQuantity=tws.totalQuantity, #totalQuantity="10",
            orderType=tws.orderType, #orderType="LMT",
            lmtPrice=tws.lmtPrice,  #lmtPrice="0.0",
            auxPrice="0.0",
            tif="DAY", #tif="",
            outsideRTH="0", #FALSE
            openClose="O",
            origin=.twsOrderID$CUSTOMER,
            ocaGroup=tws.ocaGroup,
            account="",
            orderRef="",
            transmit=TRUE,
            parentId="0",
            blockOrder="0",
            sweepToFill="0",
            displaySize="0",
            triggerMethod="0",
            hidden="0",
            discretionaryAmt="0.0",
            goodAfterTime="",
            goodTillDate="",
            faGroup="", faMethod="", faPercentage="", faProfile="",
            shortSaleSlot="0",
            designatedLocation=.twsOrderID$EMPTY_STR,
            ocaType=tws.ocaType,
            rule80A="",
            settlingFirm="",
            clearingAccount="", clearingIntent="",
            allOrNone="0",
            minQty="",
            percentOffset="",
            eTradeOnly="0",
            firmQuoteOnly="0",
            nbboPriceCap="",
            auctionStrategy="0",
            startingPrice="",
            stockRefPrice="",
            delta="",
            stockRangeLower="",
            stockRangeUpper="",
            overridePercentageConstraints="0",
            volatility="",
            volatilityType="",
            deltaNeutralOrderType="",
            deltaNeutralAuxPrice="",
            continuousUpdate="0",
            referencePriceType="",
            trailStopPrice="",
            basisPoints="",
            basisPointsType="",
            scaleInitLevelSize="",
            scaleSubsLevelSize="",
            scalePriceIncrement="",
            notHeld=FALSE,
            algoStrategy="",
            algoParams=NULL,
            whatIf=FALSE,
            clientId=conn$clientId,  #clientId="",
            permId=""
       )

       if(!inherits(Order, 'twsOrder'))
         stop('requires twsOrder object for Order arg')
#print(Order)
       #waiting for connection to become available for writing, with timeout:
       con=conn[[1]]
       sel<-socketSelect(list(con), write=TRUE, timeout=timeoutSocket  )
       if (is.null(sel)) stop("IBplaceOrder: We have WRITE timeout while placing orders")
       #
       placeOrder(twsconn=conn,Contract=Contract,Order=Order) #also updates .Last.orderId
       #
       #print(.Last.orderId)
       #add the new orderId to the ibLookupOrderId matrix
       # Columns:   orderId      clientId     portfolio  symbol  order_book_timestamp
       tmp<-list( tws.orderId, conn$clientId, portfolio, symbol, index(tmpneworder) )
if (PrintAllDebug) print(tmp)
       if(!is.null(ibLookupOrderId)) {
			ibLookupOrderId <- IBLookupOrderId.add(ibLookupOrderId ,tmp)
	   } else {
			ibLookupOrderId <- IBLookupOrderId.add(IBLookupOrderId.init() ,tmp)
			}
       assign("ibLookupOrderId", ibLookupOrderId, .GlobalEnv) #assign in global context
if (PrintAllDebug) print('ibLookupOrderId=')
if (PrintAllDebug) print(ibLookupOrderId)
       # the result and all other status updates will be returned through ORDER_STATUS event
     } #End of order placement loop

  } #End. we have new orders to place


}

