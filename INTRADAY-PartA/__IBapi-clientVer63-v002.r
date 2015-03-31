

require(IBrokers)
###
#  WARNING: This is not a complete implementation of the IB API Version 63,
#  rather, we have implemented those things that need to be in place
#  in order to use "hedgeType", "hedgeParam" orders for pairs trading.
#  30 Dec 2014
###

## Define the type of IB API that we are implementing here !!!
.IBAPI.VERSION <- 'clientV63'   #client Version 63
## Define the type of IB API that we are implementing here !!!


.twsIncomingMSG <- IBrokers:::.twsIncomingMSG
# NEW in client v63
.twsIncomingMSG$MARKET_DATA_TYPE          = '58'
.twsIncomingMSG$COMMISSION_REPORT         = '59'
.twsIncomingMSG$POSITION_DATA             = '61'
.twsIncomingMSG$POSITION_END              = '62'
.twsIncomingMSG$ACCOUNT_SUMMARY           = '63'
.twsIncomingMSG$ACCOUNT_SUMMARY_END       = '64'
.twsIncomingMSG$VERIFY_MESSAGE_API        = '65'
.twsIncomingMSG$VERIFY_COMPLETED          = '66'
.twsIncomingMSG$DISPLAY_GROUP_LIST        = '67'
.twsIncomingMSG$DISPLAY_GROUP_UPDATED     = '68'



reqIds <- function (conn, numIds = 1)
{

    #.reqIds(conn, numIds)
    if (!is.twsConnection(conn))
        stop("requires twsConnection object")
    con <- conn[[1]]
    VERSION <- "1"
    writeBin(.twsOutgoingMSG$REQ_IDS, con)
    writeBin(VERSION, con)
    writeBin(as.character(numIds), con)


    con <- conn[[1]]
    #e_next_id <- eWrapper()
    e_next_id <- eWrapper(NULL)
    e_next_id$nextValidId <- function(curMsg, msg, timestamp,
        file, ...) {
        msg[2]
    }
    while (TRUE) {
        socketSelect(list(con), FALSE, 0.1)
        curMsg <- readBin(con, character(), 1)
        nextValidID <- processMsg(curMsg, con, eWrapper = e_next_id,
            timestamp = NULL, file = "")
        if (curMsg == .twsIncomingMSG$NEXT_VALID_ID)
            break
    }
    return(nextValidID)
}




twsConnect <-
function (clientId = 1, host = "localhost", port = 7496, verbose = TRUE,
    timeout = 5, filename = NULL, blocking = .Platform$OS.type ==
        "windows")
{


	startApi <- function (conn, clientId)
		{
			if (!is.twsConnection(conn))
				stop("requires twsConnection object")
			con <- conn[[1]]
			VERSION <- "1"
			START_API <- "71"
			writeBin(START_API, con)
			writeBin(VERSION, con)
			writeBin(as.character(clientId), con)
		}



    if (is.null(getOption("digits.secs")))
        options(digits.secs = 6)
    if (is.character(clientId))
        filename <- clientId
    if (is.null(filename)) {
        start.time <- Sys.time()
        s <- socketConnection(host = host, port = port, open = "ab",
            blocking = blocking)
        on.exit(close(s))
        if (!isOpen(s)) {
            close(s)
            stop(paste("couldn't connect to TWS on port", port))
        }
        CLIENT_VERSION <- "63"
        #writeBin(c(CLIENT_VERSION, as.character(clientId)), s) #omit clientId
		writeBin(c(CLIENT_VERSION), s)
        #
        eW <- eWrapper(NULL)
        eW$.Data <- environment()
        SERVER_VERSION <- NEXT_VALID_ID <- CONNECTION_TIME <- NULL
		# Server Version and connection time
        while (TRUE) {
            if (!is.null(CONNECTION_TIME))
                break
            if (!socketSelect(list(s), FALSE, 0.1))
                next
            curMsg <- readBin(s, character(), 1)
			#cat(curMsg,'\n')

            if (is.null(SERVER_VERSION)) {
                SERVER_VERSION <- curMsg[1]
                CONNECTION_TIME <- readBin(s, character(), 1)
                next
            }
        }

        on.exit()
        twsconn <- new.env()
        twsconn$conn <- s
        twsconn$clientId <- clientId
        #twsconn$nextValidId <- NEXT_VALID_ID
        twsconn$port <- port
        twsconn$server.version <- SERVER_VERSION
        twsconn$connected.at <- CONNECTION_TIME
        twsconn$connected <- NULL
        class(twsconn) <- c("twsconn", "environment")
        
        # set the clientId (needs an API call now)
        startApi(twsconn, clientId)  
        # Get the NEXT_VALID_ID and set it in the global Env.
        twsconn$nextValidId <- NEXT_VALID_ID <- as.integer(reqIds(twsconn,1))
        assign(".NEXT_VALID_ID", NEXT_VALID_ID, .GlobalEnv)
        #
        return(twsconn)
    }
    else {
        fh <- file(filename, open = "r")
        dat <- scan(fh, what = character(), quiet = TRUE)
        close(fh)
        tmp <- tempfile()
        fh <- file(tmp, open = "ab")
        writeBin(dat, fh)
        close(fh)
        s <- file(tmp, open = "rb")
        twsconn <- new.env()
        twsconn$conn <- s
        twsconn$clientId <- NULL
        twsconn$nextValidId <- NULL
        twsconn$port <- NULL
        twsconn$server.version <- NULL
        twsconn$connected.at <- filename
        class(twsconn) <- c("twsplay", "twsconn", "environment")
        return(twsconn)
    }
}


#~ tws<- twsConnect(clientId=33, host="localhost",
#~                               port=5001, verbose=TRUE,
#~                               timeout=15, filename=NULL)
#~
#~ str(tws)


ibgConnect <- function (clientId = 1, host = "localhost", port = 4001, verbose = TRUE, 
    timeout = 5, filename = NULL, blocking = .Platform$OS.type == 
        "windows") 
{
    twsConnect(clientId, host, port, verbose, timeout, filename)
}





# Enable the hedgeType and hedgeParam  fields and possibly optOutSmartRouting.
twsOrder <- function (
	orderId, action = "BUY", totalQuantity = "0", orderType = "LMT",
    lmtPrice = "0.0", auxPrice = "", tif = "", outsideRTH = "0",
    openClose = "O", origin = .twsOrderID$CUSTOMER, ocaGroup = "",
    account = "", orderRef = "", transmit = TRUE, parentId = "0",
    blockOrder = "0", sweepToFill = "0", displaySize = "0", triggerMethod = "0",
    hidden = "0", discretionaryAmt = "0", goodAfterTime = "",
    goodTillDate = "", faGroup = "", faMethod = "", faPercentage = "",
    faProfile = "", shortSaleSlot = "0", designatedLocation = .twsOrderID$EMPTY_STR,
    ocaType = "0", rule80A = "", settlingFirm = "", clearingAccount = "",
    clearingIntent = "", allOrNone = "0", minQty = "", percentOffset = "",
    eTradeOnly = "1", firmQuoteOnly = "1", nbboPriceCap = "",
    auctionStrategy = "0", startingPrice = "", stockRefPrice = "",
    delta = "", stockRangeLower = "", stockRangeUpper = "", overridePercentageConstraints = "0",
    volatility = "", volatilityType = "", deltaNeutralOrderType = "",
    deltaNeutralAuxPrice = "", 
    continuousUpdate = "0", referencePriceType = "",
    trailStopPrice = "", basisPoints = "", basisPointsType = "",
    scaleInitLevelSize = "", scaleSubsLevelSize = "", scalePriceIncrement = "",
    notHeld = FALSE, 
    algoStrategy = "", 
    algoParams = NULL,  #has count
    whatIf = FALSE,
    clientId = "", permId = "", exemptCode="-1",
	hedgeType = "", hedgeParam = "" ,
	optOutSmartRouting = FALSE, scaleTable="", activeStartTime="", activeStopTime="", trailingPercent="",	
	#NEW
	deltaNeutralConId = "0",
	deltaNeutralSettlingFirm = '',
	deltaNeutralClearingAccount = '',
	deltaNeutralClearingIntent = '',
	deltaNeutralOpenClose = '',
	deltaNeutralShortSale  = "0",
	deltaNeutralShortSaleSlot = "0",
	deltaNeutralDesignatedLocation = '',

	scalePriceAdjustValue = "0",
	scalePriceAdjustInterval = "0",
	scaleProfitOffset = "0",
	scaleAutoReset = "0",
	scaleInitPosition = "0",
	scaleInitFillQty = "0",
	scaleRandomPercent = "0",

	smartComboRoutingParams = NULL,	#has count
	smartComboRoutingParamsCount = '0',	#helper parameter only, used in OPEN_ORDER
	orderComboLegs = NULL,	#has count
	orderComboLegsCount = '0',	#helper parameter only, used in OPEN_ORDER
		
	comboLegs = NULL, #dummy param, used in OPEN_ORDER
	comboLegsCount = '0', #dummy param , used in OPEN_ORDER
	
	orderMiscOptions = NULL	
	)
{
    if (missing(orderId))
        orderId <- ""
    structure(list(orderId = orderId, clientId = clientId, permId = permId,
        action = action, totalQuantity = as.character(as.numeric(totalQuantity)),
        orderType = orderType, lmtPrice = as.character(lmtPrice),
        auxPrice = as.character(auxPrice), tif = tif, ocaGroup = ocaGroup,
        ocaType = ocaType, orderRef = orderRef, transmit = as.character(as.integer(transmit)),
        parentId = parentId, blockOrder = blockOrder, sweepToFill = sweepToFill,
        displaySize = displaySize, triggerMethod = triggerMethod,
        outsideRTH = outsideRTH, hidden = hidden, goodAfterTime = goodAfterTime,
        goodTillDate = goodTillDate, overridePercentageConstraints = overridePercentageConstraints,
        rule80A = rule80A, allOrNone = allOrNone, minQty = minQty,
        percentOffset = percentOffset, trailStopPrice = trailStopPrice,
        faGroup = faGroup, faProfile = faProfile, faMethod = faMethod,
        faPercentage = faPercentage, openClose = openClose, origin = origin,
        shortSaleSlot = shortSaleSlot, designatedLocation = designatedLocation,
        discretionaryAmt = discretionaryAmt, eTradeOnly = eTradeOnly,
        firmQuoteOnly = firmQuoteOnly, nbboPriceCap = nbboPriceCap,
        auctionStrategy = auctionStrategy, startingPrice = startingPrice,
        stockRefPrice = stockRefPrice, delta = delta, stockRangeLower = stockRangeLower,
        stockRangeUpper = stockRangeUpper, volatility = volatility,
        volatilityType = volatilityType, continuousUpdate = continuousUpdate,
        referencePriceType = referencePriceType, deltaNeutralOrderType = deltaNeutralOrderType,
        deltaNeutralAuxPrice = deltaNeutralAuxPrice, basisPoints = basisPoints,
        basisPointsType = basisPointsType, scaleInitLevelSize = scaleInitLevelSize,
        scaleSubsLevelSize = scaleSubsLevelSize, scalePriceIncrement = scalePriceIncrement,
        account = account, settlingFirm = settlingFirm, clearingAccount = clearingAccount,
        clearingIntent = clearingIntent, algoStrategy = algoStrategy,
        algoParams = algoParams, whatIf = as.character(as.integer(whatIf)),
        notHeld = as.character(as.integer(notHeld)),
        exemptCode = as.character(as.integer(exemptCode)),  #NEW
        hedgeType = hedgeType,  #NEW !! using it
        hedgeParam = hedgeParam,  #NEW !! using it
        optOutSmartRouting = as.character(as.integer(optOutSmartRouting)),  #NEW
        scaleTable = scaleTable, #NEW
        activeStartTime = activeStartTime, #NEW
        activeStopTime = activeStopTime , #NEW
        trailingPercent = trailingPercent, #NEW
        
		#NEW all below 
		deltaNeutralConId = deltaNeutralConId,
		deltaNeutralSettlingFirm = deltaNeutralSettlingFirm,
		deltaNeutralClearingAccount = deltaNeutralClearingAccount,
		deltaNeutralClearingIntent = deltaNeutralClearingIntent,
		deltaNeutralOpenClose = deltaNeutralOpenClose,
		deltaNeutralShortSale  = deltaNeutralShortSale,
		deltaNeutralShortSaleSlot = deltaNeutralShortSaleSlot,
		deltaNeutralDesignatedLocation = deltaNeutralDesignatedLocation,

		scalePriceAdjustValue = scalePriceAdjustValue,
		scalePriceAdjustInterval = scalePriceAdjustInterval,
		scaleProfitOffset = scaleProfitOffset,
		scaleAutoReset = scaleAutoReset,
		scaleInitPosition = scaleInitPosition,
		scaleInitFillQty = scaleInitFillQty,
		scaleRandomPercent = scaleRandomPercent,

		smartComboRoutingParams = smartComboRoutingParams,	#has count
		smartComboRoutingParamsCount = smartComboRoutingParamsCount,	
		orderComboLegs = orderComboLegs,	#has count
		orderComboLegsCount = orderComboLegsCount,	
		
		comboLegs = comboLegs, #has count. dummy only
		comboLegsCount = comboLegsCount, # dummy only
		
		orderMiscOptions = orderMiscOptions	        
        
        ), class = "twsOrder")
}




twsExecution <- function (orderId, clientId, execId, time, acctNumber, exchange, 
    side, shares, price, permId, liquidation, cumQty, avgPrice, orderRef="", evRule="", evMultiplier="") 
{
    if (is.null(names(match.call()[-1]))) 
        #return(do.call("twsExecution", rep(list(NULL), 13)))
        return(do.call("twsExecution", rep(list(NULL), 16)))
    structure(list(orderId = orderId, clientId = clientId, execId = execId, 
        time = time, acctNumber = acctNumber, exchange = exchange, 
        side = side, shares = shares, price = price, permId = permId, 
        liquidation = liquidation, cumQty = cumQty, avgPrice = avgPrice,
        orderRef=orderRef, evRule=evRule, evMultiplier=evMultiplier
        ), 
        class = "twsExecution")
}


twsOrderState <- function(status = NULL, initMargin = NULL, maintMargin = NULL, 
    equityWithLoan = NULL, commission = 0, minCommission = 0, 
    maxCommission = 0, commissionCurrency = NULL, warningText = NULL) 
{
    structure(list(status = status, initMargin = initMargin, 
        maintMargin = maintMargin, equityWithLoan = equityWithLoan, 
        commission = commission, minCommission = minCommission, 
        maxCommission = maxCommission, commissionCurrency = commissionCurrency, 
        warningText = warningText), class = "twsOrderState")
}






# in processMsg() use incoming message 'version' to determine the fields to read !!!

processMsg <- function (curMsg, con, eWrapper, timestamp, file, twsconn, ...)
{
    if (curMsg == .twsIncomingMSG$TICK_PRICE) { #v63 length OK.
        msg <- .Internal(readBin(con, "character", 6L, NA_integer_,
            TRUE, FALSE))
        eWrapper$tickPrice(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$TICK_SIZE) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 4L, NA_integer_,
            TRUE, FALSE))
        eWrapper$tickSize(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$ORDER_STATUS) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 11L, NA_integer_,
            TRUE, FALSE))
        eWrapper$orderStatus(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$ERR_MSG) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 4L, NA_integer_,
            TRUE, FALSE))
        eWrapper$errorMessage(curMsg, msg, timestamp, file, twsconn,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$OPEN_ORDER) {  #v63 length ADAPTED.
        #msg <- .Internal(readBin(con, "character", 101L, NA_integer_, TRUE, FALSE))
        msg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))
        version <- as.integer(msg[1])        
		msg <- c(msg, .Internal(readBin(con, "character", 7L, NA_integer_, TRUE, FALSE)) )  
		if (version >= 32) {
			msg <- c(msg, .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )  
		}
		msg <- c(msg, .Internal(readBin(con, "character", 3L, NA_integer_, TRUE, FALSE)) )  
		if (version >= 32) {
			msg <- c(msg, .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )  
		}		
		msg <- c(msg, tmp <-.Internal(readBin(con, "character", 50L, NA_integer_, TRUE, FALSE)) )  
		
		order.deltaNeutralOrderType <- tmp[49]
#cat('order.deltaNeutralOrderType',tmp[49],'\n')
		if (order.deltaNeutralOrderType != '') {
			if (version >= 27 & !is.na(order.deltaNeutralOrderType)) {
				msg <- c(msg, .Internal(readBin(con, "character", 4L, NA_integer_, TRUE, FALSE)) )  
			}
			if (version >= 31 & !is.na(order.deltaNeutralOrderType)) {
				msg <- c(msg, .Internal(readBin(con, "character", 4L, NA_integer_, TRUE, FALSE)) )  
			}
		}
		
		msg <- c(msg, tmp <-.Internal(readBin(con, "character", 3L, NA_integer_, TRUE, FALSE)) )  
		if (version >= 30) {
			msg <- c(msg, .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
		}
		msg <- c(msg, tmp <-.Internal(readBin(con, "character", 3L, NA_integer_, TRUE, FALSE)) ) 	
		if (version >= 29) {		
			msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
#cat(tmp[1],'\n')			
			if (tmp[1]!='') {
				comboLegsCount <- as.integer(tmp[1])		
				if (comboLegsCount > 0) {
					msg <- c(msg, .Internal(readBin(con, "character", comboLegsCount*8L, NA_integer_, TRUE, FALSE)) )
				}
			}
			msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
			if (tmp[1]!='') {
				orderComboLegsCount <- as.integer(tmp[1])					
				if (orderComboLegsCount > 0) {
					msg <- c(msg, .Internal(readBin(con, "character", orderComboLegsCount*1L, NA_integer_, TRUE, FALSE)) )
				}
			}
			
		}		
		if (version >= 26) {
			msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
			if (tmp[1]!='') {
				smartComboRoutingParamsCount <- as.integer(tmp[1])
				if( smartComboRoutingParamsCount > 0) {
					msg <- c(msg, .Internal(readBin(con, "character", smartComboRoutingParamsCount*2L, NA_integer_, TRUE, FALSE)) )
				}
			}
		}	
		msg <- c(msg, tmp <-.Internal(readBin(con, "character", 3L, NA_integer_, TRUE, FALSE)) ) 
		order.scalePriceIncrement <- as.integer(tmp[3])
#cat('order.scalePriceIncrement','\n')				
#cat(tmp[3],'\n')		
		if (!is.na(order.scalePriceIncrement)) {
			if (version >= 28 & order.scalePriceIncrement > 0.0 ) {
				msg <- c(msg, .Internal(readBin(con, "character", 7L, NA_integer_, TRUE, FALSE)) )
			}		
		}
		if( version >= 24) {
			msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
			order.hedgeType <- tmp[1]
#cat('order.hedgeType','\n')				
#cat(tmp[1],'\n')					
			if( order.hedgeType != '' ) {
				msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
			}
		}
		if( version >= 25) {
			msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
		}
		msg <- c(msg, tmp<-.Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE)) )
		if( version >= 22) {
			msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
		}		
		if( version >= 20) {
			msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
			underCompPresent <- as.integer(tmp[1])
			if( !is.na(underCompPresent)) {
				if (underCompPresent > 0) {
					msg <- c(msg, tmp<-.Internal(readBin(con, "character", 3L, NA_integer_, TRUE, FALSE)) )
				}
			}
		}
		if( version >= 21) {
			msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
			order.algoStrategy <- tmp[1]
			if( order.algoStrategy != '' ) {
				msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
				algoParamsCount <- as.integer(tmp[1])
				if( algoParamsCount > 0) {
					msg <- c(msg, tmp<-.Internal(readBin(con, "character", algoParamsCount*2L, NA_integer_, TRUE, FALSE)) )
				}
			}
		}
		msg <- c(msg, tmp<-.Internal(readBin(con, "character", 10L, NA_integer_, TRUE, FALSE)) )
		
		## Must read 8 more fields here .... !!! doing it in order.deltaNeutralOrderType. now OK !!!
		
#cat('FINAL',msg,'\n',sep='-')	
#cat(length(msg),'\n')		
#cat( paste(1:length(msg),msg,sep=':'), sep='\n')

        eWrapper$openOrder(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$ACCT_VALUE) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 5L, NA_integer_,
            TRUE, FALSE))
        eWrapper$updateAccountValue(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$PORTFOLIO_VALUE) { #v63 length ADAPTED.
        msg <- .Internal(readBin(con, "character", 19L, NA_integer_,
            TRUE, FALSE))
        eWrapper$updatePortfolio(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$ACCT_UPDATE_TIME) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$updateAccountTime(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$NEXT_VALID_ID) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$nextValidId(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$CONTRACT_DATA) {  #v63 length ADAPTED.
        msg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))
        version <- as.integer(msg[1])
        if( version >= 3) {
			msg <- c(msg, .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
		}
		msg <- c(msg, .Internal(readBin(con, "character", 16L, NA_integer_, TRUE, FALSE)) )
		if( version >= 4) {
			msg <- c(msg, .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
		}
		if( version >= 5) {
			msg <- c(msg, .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE)) )
		}
		if( version >= 6) {
			msg <- c(msg, .Internal(readBin(con, "character", 7L, NA_integer_, TRUE, FALSE)) )
		}
		if( version >= 8) {
			msg <- c(msg, .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE)) )
		}				
		if( version >= 7) {
			msg <- c(msg, secIdListCount<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )		
			if( as.integer(secIdListCount) > 0) {
				msg <- c(msg, .Internal(readBin(con, "character", as.integer(secIdListCount)*2L, NA_integer_, TRUE, FALSE))  )
			}
		}
        eWrapper$contractDetails(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$EXECUTION_DATA) {  #v63 length ADAPTED.
        msg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))
        version <- as.integer(msg[1])
        if( version >= 7) {
			msg <- c(msg, .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
		}
        msg <- c(msg, .Internal(readBin(con, "character", 7L, NA_integer_, TRUE, FALSE)) )        
		if( version >= 9) {
			msg <- c(msg, .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
		}
		msg <- c(msg, .Internal(readBin(con, "character", 3L, NA_integer_, TRUE, FALSE)) )
		if (version >= 10) {
			msg <- c(msg, .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
		}
        msg <- c(msg, .Internal(readBin(con, "character", 10L, NA_integer_, TRUE, FALSE)) )    
		if( version >= 6) {
			msg <- c(msg, .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE)) )
		}
		if( version >= 8) {
			msg <- c(msg, .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
		}
		if( version >= 9) {
			msg <- c(msg, .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE)) )
		} 
        eWrapper$execDetails(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$MARKET_DEPTH) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 7L, NA_integer_,
            TRUE, FALSE))
        eWrapper$updateMktDepth(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$MARKET_DEPTH_L2) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 8L, NA_integer_,
            TRUE, FALSE))
        eWrapper$updateMktDepthL2(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$NEWS_BULLETINS) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 5L, NA_integer_,
            TRUE, FALSE))
        eWrapper$newsBulletins(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$MANAGED_ACCTS) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$managedAccounts(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$RECEIVE_FA) {  #v63 length ADAPTED, but xml implementation pending
        msg <- .Internal(readBin(con, "character", 3L, NA_integer_,
            TRUE, FALSE))
        stop("xml data currently unsupported")
        eWrapper$receiveFA(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$HISTORICAL_DATA) {  #v63 length OK.
        header <- readBin(con, character(), 5)
        nbin <- as.numeric(header[5]) * 9
        msg <- .Internal(readBin(con, "character", as.integer(nbin),
            NA_integer_, TRUE, FALSE))
        eWrapper$historicalData(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$BOND_CONTRACT_DATA) { #Implementation pending. not trading.
        stop("unimplemented as of yet")
        eWrapper$bondContractDetails(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$SCANNER_PARAMETERS) {  #v63 length OK.
         version <- readBin(con, character(), 1L)
         msg <- readBin(con, raw(), 1000000L)
        eWrapper$scannerParameters(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$SCANNER_DATA) { #v63 length OK.
        cD <- twsContractDetails()
        version <- readBin(con, character(), 1L)
        tickerId <- readBin(con, character(), 1L)
        numberOfElements <- as.integer(readBin(con, character(), 1L))
        for (i in 1:numberOfElements) {
            msg <- readBin(con, character(), 16L)
            rank <- msg[1]
            cD$contract$conId <- msg[2]
            cD$contract$symbol <- msg[3]
            cD$contract$sectype <- msg[4]
            cD$contract$expiry <- msg[5]
            cD$contract$strike <- msg[6]
            cD$contract$right <- msg[7]
            cD$contract$exch <- msg[8]
            cD$contract$currency <- msg[9]
            cD$contract$local <- msg[10]
            cD$marketName <- msg[11]
            cD$tradingClass <- msg[12]
            distance <- msg[13]
            benchmark <- msg[14]
            projection <- msg[15]
            legsStr <- msg[16]
            eWrapper$scannerData(curMsg, tickerId, rank, cD,
                distance, benchmark, projection, legsStr)
        }
    }
    else if (curMsg == .twsIncomingMSG$TICK_OPTION_COMPUTATION) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 11L, NA_integer_,
            TRUE, FALSE))
        eWrapper$tickOptionComputation(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$TICK_GENERIC) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 4L, NA_integer_,
            TRUE, FALSE))
        eWrapper$tickGeneric(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$TICK_STRING) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 4L, NA_integer_,
            TRUE, FALSE))
        eWrapper$tickString(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$TICK_EFP) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 10L, NA_integer_,
            TRUE, FALSE))
        eWrapper$tickEFP(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$CURRENT_TIME) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$currentTime(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$REAL_TIME_BARS) { #v63 length OK.
        msg <- .Internal(readBin(con, "character", 10L, NA_integer_,
            TRUE, FALSE))
        eWrapper$realtimeBars(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$FUNDAMENTAL_DATA) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 3L, NA_integer_,
            TRUE, FALSE))
        eWrapper$fundamentalData(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$CONTRACT_DATA_END) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$contractDetailsEnd(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$OPEN_ORDER_END) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 1L, NA_integer_,
            TRUE, FALSE))
        eWrapper$openOrderEnd(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$ACCT_DOWNLOAD_END) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$accountDownloadEnd(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$EXECUTION_DATA_END) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$execDetailsEnd(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$DELTA_NEUTRAL_VALIDATION) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 5L, NA_integer_,
            TRUE, FALSE))
        eWrapper$deltaNeutralValidation(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$TICK_SNAPSHOT_END) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$tickSnapshotEnd(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$MARKET_DATA_TYPE) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 3L, NA_integer_,
            TRUE, FALSE))
        eWrapper$marketDataType(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$COMMISSION_REPORT) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 7L, NA_integer_,
            TRUE, FALSE))
        eWrapper$commissionReport(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$POSITION_DATA) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 15L, NA_integer_,
            TRUE, FALSE))
        eWrapper$positionData(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$POSITION_END) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 1L, NA_integer_,
            TRUE, FALSE))
        eWrapper$positionEnd(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$ACCOUNT_SUMMARY) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 6L, NA_integer_,
            TRUE, FALSE))
        eWrapper$accountSummary(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$ACCOUNT_SUMMARY_END) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$accountSummaryEnd(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$VERIFY_MESSAGE_API) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$verifyMessageAPI(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$VERIFY_COMPLETED) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 3L, NA_integer_,
            TRUE, FALSE))
        eWrapper$verifyCompleted(curMsg, msg, timestamp,
            file, ...)  # ===> calls startApi() in the cpp implementation.
    }
    else if (curMsg == .twsIncomingMSG$DISPLAY_GROUP_LIST) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 3L, NA_integer_,
            TRUE, FALSE))
        eWrapper$displayGroupList(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$DISPLAY_GROUP_UPDATED) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 3L, NA_integer_,
            TRUE, FALSE))
        eWrapper$displayGroupUpdated(curMsg, msg, timestamp,
            file, ...)
    }
    else {
        warning(paste("Unknown incoming message: ", curMsg, ". Please reset connection",
            sep = ""), call. = FALSE)
    }
}




# Force import of some functions from the IBrokers namespace.
# These are needed if we want to call eWrapper(FALSE) for printing,
# eWrapper(NULL) for no output and eWrapper(TRUE) for debug output.
# (xx<-ls(getNamespace("IBrokers"), all.names=TRUE))[grep('^e_.*',xx)]

e_account_time   <- IBrokers:::e_account_time
e_account_value       <- IBrokers:::e_account_value
#e_execDetails      <- IBrokers:::e_execDetails   # changed below
#e_execution_data      <- IBrokers:::e_execution_data  # changed below
#e_commissionReport  #NEW see below
e_fundamentalData     <- IBrokers:::e_fundamentalData
#e_open_order        <- IBrokers:::e_open_order   # changed below
#e_order_status      <- IBrokers:::e_order_status  # fixed below (was not returning eos)
e_portfolio_value     <- IBrokers:::e_portfolio_value 
e_real_time_bars <- IBrokers:::e_real_time_bars
e_scannerData         <- IBrokers:::e_scannerData
e_tick_EFP           <- IBrokers:::e_tick_EFP 
e_tick_generic       <- IBrokers:::e_tick_generic
e_tick_option         <- IBrokers:::e_tick_option
e_tick_price          <- IBrokers:::e_tick_price
e_tick_size          <- IBrokers:::e_tick_size
e_tick_string     <- IBrokers:::e_tick_string
e_update_mkt_depth   <- IBrokers:::e_update_mkt_depth 
e_update_mkt_depthL2 <- IBrokers:::e_update_mkt_depthL2




e_open_order <- function (curMsg, contents, ...)   
{
	tmp <- NULL
	shift <- 0
	version <- as.integer(contents[1])
	if (version<32) stop('e_open_order: only version>=32 supported at this time')
	if (contents[62]=='') stop('e_open_order: Expecting "None" or another non-empty value for for deltaNeutralOrderType')
	if (contents[79]!='0') stop('e_open_order: Expecting comboLegsCount==0. NO comboLegs supoprted at the moment')
	if (contents[80]!='0') stop('e_open_order: Expecting orderComboLegsCount==0. NO orderComboLegs supoprted at the moment')
	if (contents[81]!='0') stop('e_open_order: Expecting smartComboRoutingParamsCount==0. NO smartComboRoutingParams supoprted at the moment')
	if (contents[84]!='') stop('e_open_order: Expecting scalePriceIncrement=="". NO scalePriceIncrement supoprted at the moment')
	if (contents[85]=='') {  #hedgeType=''
		if (contents[90]!='0')  stop('e_open_order: Expecting underCompPresent=="0". NO underCompPresent supoprted at the moment')
		if (contents[91]!='')  stop('e_open_order: Expecting algoStrategy=="". NO algoStrategy supoprted at the moment')
	} else {  				#hedgeType='P', etc.... inserts a shift by one for the fields that follow.
		shift <- 1
		if (contents[91]!='0')  stop('e_open_order: Expecting underCompPresent=="0". NO underCompPresent supoprted at the moment')
		if (contents[92]!='')  stop('e_open_order: Expecting algoStrategy=="". NO algoStrategy supoprted at the moment')
	}   
	
    eoo <- list(
		contract = twsContract(
			conId = contents[3], 
			symbol = contents[4], 
			sectype = contents[5], 
			expiry = contents[6], 
			strike = contents[7], 
			right = contents[8], 
			multiplier = contents[9],
			exch = contents[10], 
			currency = contents[11], 
			local = contents[12], 
			tradingClass = contents[13] ,
			combo_legs_desc = contents[78], # value dfined in order, further below...
			primary = NULL, 
			include_expired = NULL, 
			comboleg = NULL			
		), 
        
        order = twsOrder(
			orderId = contents[2], 
			action = contents[14], # 14
			totalQuantity = contents[15], 
			orderType = contents[16], 
			lmtPrice = contents[17], 
			auxPrice = contents[18], 
			tif = contents[19], 
			ocaGroup = contents[20], 
			account = contents[21], 
			openClose = contents[22],  
			origin = contents[23], 
			orderRef = contents[24], 
			clientId = contents[25], 
			permId = contents[26],  
			outsideRTH = contents[27], 
			hidden = contents[28], 
			discretionaryAmt = contents[29], 
			goodAfterTime = contents[30],   
			#31 is deprecated
			faGroup = contents[32],  
			faMethod = contents[33], 
			faPercentage = contents[34], 
			faProfile = contents[35],  
			goodTillDate = contents[36], 
			rule80A = contents[37], 
			percentOffset = contents[38], 
			settlingFirm = contents[39], 
			shortSaleSlot = contents[40], 
			designatedLocation = contents[41], 
			exemptCode = contents[42], 
			auctionStrategy = contents[43], 
			startingPrice = contents[44], 
			stockRefPrice = contents[45], 
			delta = contents[46], 
			stockRangeLower = contents[47], 
			stockRangeUpper = contents[48], 
			displaySize = contents[49], 
			blockOrder = contents[50], 
			sweepToFill = contents[51], 
			allOrNone = contents[52], 
			minQty = contents[53], 
			ocaType = contents[54], 
			eTradeOnly = contents[55], 
			firmQuoteOnly = contents[56], 
			nbboPriceCap = contents[57], 
			parentId = contents[58], 
			triggerMethod = contents[59], 
			volatility = contents[60], 
			volatilityType = contents[61], 
			deltaNeutralOrderType = contents[62], 
			deltaNeutralAuxPrice = contents[63],  # 
			
			deltaNeutralConId = contents[64], 
			deltaNeutralSettlingFirm = contents[65], 		
			deltaNeutralClearingAccount = contents[66], 			
			deltaNeutralClearingIntent = contents[67], 		

			deltaNeutralOpenClose = contents[68], 
			deltaNeutralShortSale = contents[69], 		
			deltaNeutralShortSaleSlot = contents[70], 		
			deltaNeutralDesignatedLocation = contents[71], 		
						
			continuousUpdate = contents[72],  
			referencePriceType = contents[73], 
			trailStopPrice = contents[74], 
			
			trailingPercent = contents[75], 
			
			basisPoints = contents[76], 
			basisPointsType = contents[77], 
			#comboLegsDescrip = contents[78], # 78  # see above in Contract...combo_legs_desc

			comboLegsCount = contents[79], # 79
			comboLegs = NULL,
#~ 			comboLegs = if (version >= 29 & !is.null(tmp)) {
#~ 							if (as.integer(tmp)>0) {
#~ 								shift <- shift + 1   #does not work. shift is only modified locally !?
#~ 								ll <- list()
#~ 								ll[1:(8L*as.integer(tmp))] <- contents[shift+66 +1:(8L*as.integer(tmp))]
#~ 								shift <- shift + 8L*as.integer(tmp) -1
#~ 								ll #not entirely correct since this should be a list of lists of length 8...
#~ 							} else NULL
#~ 						}  else NULL,

			orderComboLegsCount = contents[80], # 80
			orderComboLegs = NULL,
#~ 			orderComboLegs = if (version >= 29 & !is.null(tmp)) {
#~ 								if (as.integer(tmp)>0) {
#~ 									shift <- shift + 1 #does not work. shift is only modified locally !?
#~ 									ll <- list()
#~ 									ll[1:(1L*as.integer(tmp))] <- contents[shift+66 +1:(1L*as.integer(tmp))]
#~ 									shift <- shift + 1L*as.integer(tmp) -1
#~ 									ll #not entirely correct since this should be a list of lists of length 1...
#~ 								} else NULL
#~ 							}  else NULL,
			
			smartComboRoutingParamsCount = contents[81], # 81		
			smartComboRoutingParams = NULL,
#~ 			smartComboRoutingParams = if (version >= 26 & !is.null(tmp)) {
#~ 								if (as.integer(tmp)>0) {
#~ 									shift <- shift + 1 #does not work. shift is only modified locally !?
#~ 									ll <- list()
#~ 									ll[1:(2L*as.integer(tmp))] <- contents[shift+66 +1:(2L*as.integer(tmp))]
#~ 									shift <- shift + 2L*as.integer(tmp) -1
#~ 									ll #not entirely correct since this should be a list of lists of length 2...
#~ 								} else NULL
#~ 							}  else NULL,
						
			scaleInitLevelSize = contents[82], 
			scaleSubsLevelSize = contents[83], 
			scalePriceIncrement = contents[84], 
			
			scalePriceAdjustValue = NULL,
			scalePriceAdjustInterval = NULL,
			scaleProfitOffset =  NULL,
			scaleAutoReset =  NULL,
			scaleInitPosition =  NULL,
			scaleInitFillQty =  NULL,
			scaleRandomPercent =  NULL,
				
			hedgeType = contents[85]->tmp, 
			hedgeParam	= if (tmp!='') contents[86] else NULL,
			
			optOutSmartRouting  =  contents[shift+86],
			
			clearingAccount = contents[shift+87], 
			
			#clearingIntent = {cat('clearingIntent',contents[shift+88],' shift:',shift,'\n');contents[shift+88]}, 						
			clearingIntent = contents[shift+88], 			
			
			#notHeld = {cat('notHeld',contents[shift+89],'\n');contents[shift+89]}, 
			notHeld = contents[shift+89], 
			
			#underCompPresent = contents[shift+90], 			
			#underCompConId = NULL,
#~ 			underCompConId = if (!is.null(tmp)) {
#~ 					if (!is.na(tmp2<-as.integer(tmp))) {
#~ 						if (tmp2>0) {shift<-shift+1;contents[shift+72]} else NULL  #does not work. shift is only modified locally !?
#~ 					} else NULL
#~ 				} else NULL,
			#underCompDelta = NULL,
#~ 			underCompDelta = if (!is.null(tmp)) {
#~ 					if (!is.na(tmp2<-as.integer(tmp))) {
#~ 						if (tmp2>0) {shift<-shift+1;contents[shift+72]} else NULL #does not work. shift is only modified locally !?
#~ 					} else NULL
#~ 				} else NULL,
			#underCompPrice =NULL,
#~ 			underCompPrice = if (!is.null(tmp)) {
#~ 					if (!is.na(tmp2<-as.integer(tmp))) {
#~ 						if (tmp2>0) {shift<-shift+1;contents[shift+72]} else NULL  #does not work. shift is only modified locally !?
#~ 					} else NULL
#~ 				} else NULL,				

			#algoStrategy = {cat('algoStrategy',contents[shift+91],'\n');contents[shift+91]},
			algoStrategy = contents[shift+91],
			
			#algoParamsCount = (if (!is.null(tmp)) {if (tmp!='') {shift<-shift+1;contents[shift+72]} else NULL} else NULL)->tmp2,
#~ 			algoParams = if (!is.null(tmp2)) {
#~ 								if (as.integer(tmp2)>0) {
#~ 									shift <- shift + 1  #does not work. shift is only modified locally !?
#~ 									ll <- list()
#~ 									ll[1:(2L*as.integer(tmp2))] <- contents[shift+72 +1:(2L*as.integer(tmp2))]
#~ 									shift <- shift + 2L*as.integer(tmp2) -1
#~ 									ll #not entirely correct since this should be a list of lists of length 2...
#~ 								} else NULL
#~ 							}  else NULL,			
										
			#whatIf = {cat('whatIf',contents[shift+92],'\n');contents[shift+92]}
			whatIf = contents[shift+92]
		), 
        
        orderstate = twsOrderState(
			status = contents[shift+93], #Submitted, etc...!!!
			initMargin = contents[shift+94], 
			maintMargin = contents[shift+95], 
			equityWithLoan = contents[shift+96], 
			commission = contents[shift+97], 
			minCommission = contents[shift+98], 
			maxCommission = contents[shift+99], 
			commissionCurrency = contents[shift+100], 
			warningText = contents[shift+101]
		)
	)
                
    eoo <- structure(eoo, class = "eventOpenOrder")
    cat("TWS OpenOrder:", paste("orderId=", eoo$order$orderId, 
        sep = ""), paste("conId=", eoo$contract$conId, sep = ""), 
        paste("symbol=", eoo$contract$symbol, sep = ""), paste("status=", 
            eoo$orderstate$status, sep = ""), "\n")
    eoo
}


e_order_status <- function (msg, contents, ...) 
{
    eos <- list(orderId = contents[2], status = contents[3], 
        filled = contents[4], remaining = contents[5], averageFillPrice = contents[6], 
        permId = contents[7], parentId = contents[8], lastFillPrice = contents[9], 
        clientId = contents[10], whyHeld = contents[11])
    eos <- structure(eos, class = "eventOrderStatus")
    cat("TWS OrderStatus:", paste("orderId=", eos$orderId, sep = ""), 
        paste("status=", eos$status, sep = ""), paste("filled=", 
            eos$filled, sep = ""), paste("remaining=", eos$remaining, 
            sep = ""), paste("averageFillPrice=", eos$averageFillPrice, 
            sep = ""), "\n")
    eos
}





e_execution_data <- e_execDetails <- function (curMsg, msg, file, ...) 
{
    version = msg[1]
    reqId = msg[2]
    orderId = msg[3]
    eed <- list(
		contract = twsContract(conId = msg[4], symbol = msg[5], 
			sectype = msg[6], expiry = msg[7], strike = msg[8], right = msg[9], 
			multiplier = msg[10], #NEW
			exch = msg[1+10], currency = msg[1+11], local = msg[1+12], 
			tradingClass = msg[1+13],
			combo_legs_desc = NULL, primary = NULL, include_expired = NULL, 
			comboleg = NULL), 
        
        execution = twsExecution(orderId = orderId, 
			execId = msg[2+13], 
			time = msg[2+14], 
			acctNumber = msg[2+15], 
			exchange = msg[2+16], 
			side = msg[2+17], 
			shares = msg[2+18], 
			price = msg[2+19], 
			permId = msg[2+20], 
			clientId = msg[2+21], 
			liquidation = msg[2+22], 
			cumQty = msg[2+23], 
			avgPrice = msg[2+24],
			orderRef = msg[27], 
			evRule = msg[28], 
			evMultiplier = msg[29]
			)
        )
    eed <- structure(eed, class = "eventExecutionData")
    cat("TWS Execution:", paste("orderId=", eed$execution$orderId, 
        sep = ""), paste("time=", strptime(eed$execution$time, 
        "%Y%m%d  %H:%M:%S"), sep = ""), paste("side=", eed$execution$side, 
        sep = ""), paste("shares=", eed$execution$shares, sep = ""), 
        paste("symbol=", eed$contract$symbol, sep = ""), paste("conId=", 
            eed$contract$conId, sep = ""), paste("price=", eed$execution$price, 
            sep = ""), "\n")
    eed
}



e_commissionReport <- function (curMsg, msg, file, ...) 
{
#~ 				DECODE_FIELD( version);
#~ 				DECODE_FIELD( commissionReport.execId);
#~ 				DECODE_FIELD( commissionReport.commission);
#~ 				DECODE_FIELD( commissionReport.currency);
#~ 				DECODE_FIELD( commissionReport.realizedPNL);
#~ 				DECODE_FIELD( commissionReport.yield);
#~ 				DECODE_FIELD( commissionReport.yieldRedemptionDate);
    version = msg[1]
    execId = msg[2]
    ecr <- list(
		commission = msg[3],
		currency = msg[4],
		realizedPNL = msg[5],
		yield = msg[6],
		yieldRedemptionDate = msg[7]
        )
    ecr <- structure(ecr, class = "eventCommissionReport")
    cat("TWS Commission:", 
		paste("commission=", ecr$commission, sep = ""), 
		paste("currency=", ecr$currency, sep = ""), 
    "\n")
    ecr
}











eWrapper <- function(debug = FALSE)
{
    .Data <- new.env()
    get.Data <- function(x) get(x, .Data)
    assign.Data <- function(x, value) assign(x, value, .Data)
    remove.Data <- function(x) remove(x, .Data)
    if (is.null(debug)) {
        errorMessage <- function(curMsg, msg, timestamp, file, twsconn, ...) {
            cat(msg, "\n")
        }
        tickPrice <- tickSize <- tickOptionComputation <- tickGeneric <- tickString <- tickEFP <-
        orderStatus <- openOrder <- openOrderEnd <- updateAccountValue <- updateAccountTime <-
        updatePortfolio <- accountDownloadEnd <- nextValidId <- contractDetails <- bondContractDetails <-
        contractDetailsEnd <- execDetails <- execDetailsEnd <- updateMktDepth <- updateMktDepthL2 <-
        updateNewsBulletin <- managedAccounts <- receiveFA <- historicalData <- scannerParameters <-
        scannerData <- scannerDataEnd <- realtimeBars <- currentTime <- fundamentalData <-
        deltaNeutralValidation <- tickSnapshotEnd <-
        marketDataType <- commissionReport <- positionData <- positionEnd <- accountSummary <-
        accountSummaryEnd <- verifyMessageAPI <- verifyCompleted <- displayGroupList <- displayGroupUpdated <-
        function(curMsg,
            msg, timestamp, file, ...) {
            c(curMsg, msg)
        }
    }
    else if (!debug) {
        tickPrice <- function(curMsg, msg, timestamp, file, ...) {
            symbols <- get.Data("symbols")
            e_tick_price(NULL, msg, timestamp, file, symbols,
                ...)
        }
        tickSize <- function(curMsg, msg, timestamp, file, ...) {
            symbols <- get.Data("symbols")
            e_tick_size(NULL, msg, timestamp, file, symbols,
                ...)
        }
        tickOptionComputation <- function(curMsg, msg, timestamp,
            file, ...) {
            symbols <- get.Data("symbols")
            e_tick_option(NULL, msg, timestamp, file, symbols,
                ...)
        }
        tickGeneric <- function(curMsg, msg, timestamp, file,
            ...) {
            symbols <- get.Data("symbols")
            e_tick_generic(NULL, msg, timestamp, file, symbols,
                ...)
        }
        tickString <- function(curMsg, msg, timestamp, file,
            ...) {
            symbols <- get.Data("symbols")
            e_tick_string(NULL, msg, timestamp, file, symbols,
                ...)
        }
        tickEFP <- function(curMsg, msg, timestamp, file, ...) {
            symbols <- get.Data("symbols")
            e_tick_EFP(NULL, msg, timestamp, file, symbols, ...)
        }
        orderStatus <- function(curMsg, msg, timestamp, file,
            ...) {
            e_order_status(curMsg, msg)
            c(curMsg, msg)
        }
        errorMessage <- function(curMsg, msg, timestamp, file,
            twsconn, ...) {
            if (msg[3] == "1100")
                twsconn$connected <- FALSE
            if (msg[3] %in% c("1101", "1102"))
                twsconn$connected <- TRUE
            cat("TWS Message:", msg, "\n")
        }
        openOrder <- function(curMsg, msg, timestamp, file, ...) {
			e_open_order(curMsg, msg)
            c(curMsg, msg)
        }
        openOrderEnd <- function(curMsg, msg, timestamp, file,
            ...) {
            c(curMsg, msg)
        }
        updateAccountValue <- function(curMsg, msg, timestamp,
            file, ...) {
            c(curMsg, msg)
        }
        updatePortfolio <- function(curMsg, msg, timestamp, file,
            ...) {
            e_portfolio_value(curMsg, msg)
            c(curMsg, msg)
        }
        updateAccountTime <- function(curMsg, msg, timestamp,
            file, ...) {
            c(curMsg, msg)
        }
        accountDownloadEnd <- function(curMsg, msg, timestamp,
            file, ...) {
            c(curMsg, msg)
        }
        nextValidId <- function(curMsg, msg, timestamp, file,
            ...) {
            c(curMsg, msg)
        }
        contractDetails <- function(curMsg, msg, timestamp, file,
            ...) {
            c(curMsg, msg)
        }
        bondContractDetails <- function(curMsg, msg, timestamp,
            file, ...) {
            c(curMsg, msg)
        }
        contractDetailsEnd <- function(curMsg, msg, timestamp,
            file, ...) {
            c(curMsg, msg)
        }
        execDetails <- function(curMsg, msg, timestamp, file,
            ...) {
            e_execDetails(curMsg, msg, file, ...)
        }
        execDetailsEnd <- function(curMsg, msg, timestamp, file,
            ...) {
            c(curMsg, msg)
        }
        updateMktDepth <- function(curMsg, msg, timestamp, file,
            ...) {
            symbols <- get.Data("symbols")
            e_update_mkt_depth(NULL, msg, timestamp, file, symbols,
                ...)
        }
        updateMktDepthL2 <- function(curMsg, msg, timestamp,
            file, ...) {
            symbols <- get.Data("symbols")
            e_update_mkt_depthL2(NULL, msg, timestamp, file,
                symbols, ...)
        }
        updateNewsBulletin <- function(curMsg, msg, timestamp,
            file, ...) {
            cat("newsMsgId: ", msg[2], "newsMsgType: ", msg[3],
                "newsMessage: ", msg[4], "origExch:", msg[5],
                "\n")
            c(curMsg, msg)
        }
        managedAccounts <- function(curMsg, msg, timestamp, file,
            ...) {
            c(curMsg, msg)
        }
        receiveFA <- function(curMsg, msg, timestamp, file, ...) {
            c(curMsg, msg)
        }
        historicalData <- function(curMsg, msg, timestamp, file,
            ...) {
            c(curMsg, msg)
        }
        scannerParameters <- function(curMsg, msg, timestamp,
            file, ...) {
            cat(msg <- rawToChar(msg[-which(msg == as.raw(0))]))
            c(curMsg, msg)
        }
        scannerData <- function(curMsg, reqId, rank, contract,
            distance, benchmark, projection, legsStr) {
            e_scannerData(curMsg, reqId, rank, contract, distance,
                benchmark, projection, legsStr)
        }
        scannerDataEnd <- function(curMsg, msg, timestamp, file,
            ...) {
            c(curMsg, msg)
        }
        realtimeBars <- function(curMsg, msg, timestamp, file,
            ...) {
            symbols <- get.Data("symbols")
            e_real_time_bars(curMsg, msg, symbols, file, ...)
        }
        currentTime <- function(curMsg, msg, timestamp, file,
            ...) {
            c(curMsg, msg)
        }
        fundamentalData <- function(curMsg, msg, timestamp, file,
            ...) {
            e_fundamentalData(curMsg, msg)
        }
        deltaNeutralValidation <- function(curMsg, msg, timestamp,
            file, ...) {
            c(curMsg, msg)
        }
        tickSnapshotEnd <- function(curMsg, msg, timestamp, file,
            ...) {
            c(curMsg, msg)
        }
        commissionReport <- function(curMsg, msg, timestamp, file,
            ...) {
            e_commissionReport(curMsg, msg, file, ...)
        }        
        marketDataType <- positionData <- positionEnd <- accountSummary <-
        accountSummaryEnd <- verifyMessageAPI <- verifyCompleted <- displayGroupList <- displayGroupUpdated <-
        function(curMsg, msg, timestamp, file, ...) {
            c(curMsg, msg)
        }
    }
    else {
        tickPrice <- tickSize <- tickOptionComputation <- tickGeneric <- tickString <- tickEFP <-
        orderStatus <- openOrder <- openOrderEnd <- updateAccountValue <- updateAccountTime <-
        updatePortfolio <- accountDownloadEnd <- nextValidId <- contractDetails <- bondContractDetails <-
        contractDetailsEnd <- execDetails <- execDetailsEnd <- updateMktDepth <- updateMktDepthL2 <-
        updateNewsBulletin <- managedAccounts <- receiveFA <- historicalData <- scannerParameters <-
        scannerData <- scannerDataEnd <- realtimeBars <- currentTime <- fundamentalData <-
        deltaNeutralValidation <- tickSnapshotEnd <-
        marketDataType <- commissionReport <- positionData <- positionEnd <- accountSummary <-
        accountSummaryEnd <- verifyMessageAPI <- verifyCompleted <- displayGroupList <- displayGroupUpdated <-
        function(curMsg, msg, timestamp, file, ...) {
            cat(as.character(timestamp), curMsg, msg, "\n" ) #, file = file[[1]], append = TRUE, ...)
        }
        errorMessage <- function(curMsg, msg, timestamp, file, twsconn, ...) {
            cat(as.character(timestamp), curMsg, msg, "\n" ) #, file = file[[1]], append = TRUE, ...)
        }
    }
    eW <- list(.Data = .Data, get.Data = get.Data, assign.Data = assign.Data,
        remove.Data = remove.Data, tickPrice = tickPrice, tickSize = tickSize,
        tickOptionComputation = tickOptionComputation, tickGeneric = tickGeneric,
        tickString = tickString, tickEFP = tickEFP, orderStatus = orderStatus,
        errorMessage = errorMessage, openOrder = openOrder, openOrderEnd = openOrderEnd,
        updateAccountValue = updateAccountValue, updatePortfolio = updatePortfolio,
        updateAccountTime = updateAccountTime, accountDownloadEnd = accountDownloadEnd,
        nextValidId = nextValidId, contractDetails = contractDetails,
        bondContractDetails = bondContractDetails, contractDetailsEnd = contractDetailsEnd,
        execDetails = execDetails, execDetailsEnd = execDetailsEnd,
        updateMktDepth = updateMktDepth, updateMktDepthL2 = updateMktDepthL2,
        updateNewsBulletin = updateNewsBulletin, managedAccounts = managedAccounts,
        receiveFA = receiveFA, historicalData = historicalData,
        scannerParameters = scannerParameters, scannerData = scannerData,
        scannerDataEnd = scannerDataEnd, realtimeBars = realtimeBars,
        currentTime = currentTime, fundamentalData = fundamentalData,
        deltaNeutralValidation = deltaNeutralValidation, tickSnapshotEnd = tickSnapshotEnd,
		marketDataType = marketDataType,
		commissionReport = commissionReport,
		positionData = positionData,
		positionEnd = positionEnd,
		accountSummary = accountSummary,
		accountSummaryEnd = accountSummaryEnd,
		verifyMessageAPI = verifyMessageAPI,
		verifyCompleted = verifyCompleted,
		displayGroupList = displayGroupList,
		displayGroupUpdated = displayGroupUpdated
        )
    class(eW) <- "eWrapper"
    invisible(eW)
}



.reqContractDetails <- function (conn, Contract, reqId = "1")
{
    if (!is.twsConnection(conn))
        stop("requires twsConnection object")
    if (!inherits(Contract, "twsContract"))
        stop("requires twsContract object")
    con <- conn[[1]]
    #VERSION <- "5"
    #VERSION <- "6"
    VERSION <- "7"
    request <- c(.twsOutgoingMSG$REQ_CONTRACT_DATA, VERSION,
        reqId, Contract$conId, Contract$symbol, Contract$sectype,
        Contract$expiry, Contract$strike, Contract$right, Contract$multiplier,
        Contract$exch, Contract$currency, Contract$local,
		"", #Contract$tradingClass,  #not using it
        Contract$include_expired,
        Contract$secIdType, Contract$secId)
    writeBin(as.character(request), con)
}


# reqContractDetails # no change required if we use version=7 for the request.
# But if we don't re-define this function here, then the original will not use the processMsg() function above ??
#       reqContractDetails <- IBrokers:::reqContractDetails  #does not work:

# no change required
errorHandler <- IBrokers:::errorHandler

# no change required
twsContractDetails <- IBrokers:::twsContractDetails 

reqContractDetails <- function (conn, Contract, reqId = "1", verbose = FALSE, eventWrapper = eWrapper(), 
    CALLBACK = twsCALLBACK, ...) 
{
    .reqContractDetails(conn, Contract, reqId)
    if (is.null(CALLBACK)) 
        invisible(return(NULL))
    eW <- eWrapper(NULL)
    eW$contractDetails <- function(curMsg, msg, timestamp, file, 
        ...) {
        twsContractDetails(version = msg[1], contract = twsContract(conId = msg[12 + 
            1], symbol = msg[3], sectype = msg[4], expiry = msg[5], 
            primary = msg[21], strike = msg[5 + 1], right = msg[6 + 
                1], exch = msg[7 + 1], currency = msg[8 + 1], 
            multiplier = msg[14 + 1], include_expired = Contract$include_expired, 
            combo_legs_desc = "", comboleg = "", local = msg[9 + 
                1]), marketName = msg[10 + 1], tradingClass = msg[11 + 
            1], conId = msg[12 + 1], minTick = msg[13 + 1], orderTypes = unlist(strsplit(msg[15 + 
            1], ",")), validExchanges = unlist(strsplit(msg[16 + 
            1], ",")), priceMagnifier = msg[17 + 1], underConId = msg[18 + 
            1], longName = msg[19 + 1], contractMonth = msg[22], 
            industry = msg[23], category = msg[24], subcategory = msg[25], 
            timeZoneId = msg[26], tradingHours = msg[27], liquidHours = msg[28])
    }
    contracts <- list()
    con <- conn[[1]]
    while (TRUE) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, character(), 1)
        if (curMsg != .twsIncomingMSG$CONTRACT_DATA) {
            if (curMsg == .twsIncomingMSG$ERR_MSG) {
                if (!errorHandler(con, verbose, OK = c(165, 300, 
                  366, 2104, 2106, 2107))) {
                  warning("error in contract details")
                  break
                }
            }
            else {
                processMsg(curMsg, con, eW, timestamp, file)
                if (curMsg == .twsIncomingMSG$CONTRACT_DATA_END) 
                  break
            }
        }
        if (curMsg == .twsIncomingMSG$CONTRACT_DATA) {
            contracts[[length(contracts) + 1]] <- processMsg(curMsg, 
                con, eW, timestamp, file)
        }
    }
    return(contracts)
}



is.twsPlayback <- IBrokers:::is.twsPlayback

isConnected <- function (twsconn) 
{
    is_open <- function(con) {
        if (inherits(try(isOpen(con), silent = TRUE), "try-error")) {
            FALSE
        }
        else TRUE
    }
    if (!is.twsConnection(twsconn)) {
        warning("isConnected requires a twsconn object")
        return(FALSE)
    }
    if (!is.null(twsconn$connected)) {
        return(is_open(twsconn[[1]]) && twsconn$connected)
    }
    else {
        is_open(twsconn[[1]])
    }
}



#twsCALLBACK    <- IBrokers:::twsCALLBACK  # does not work ??

twsCALLBACK <- function (twsCon, eWrapper, timestamp, file, playback = 1, ...) 
{
    if (missing(eWrapper)) 
        eWrapper <- eWrapper()
    con <- twsCon[[1]]
    if (inherits(twsCon, "twsPlayback")) {
		stop('No playback support')
    }
    else {
        tryCatch(while (isConnected(twsCon)) {
            if (!socketSelect(list(con), FALSE, 0.25)) 
                next
            curMsg <- readBin(con, "character", 1L)
            if (!is.null(timestamp)) {
                processMsg(curMsg, con, eWrapper, format(Sys.time(), 
                  timestamp), file, twsCon, ...)
            }
            else {
                processMsg(curMsg, con, eWrapper, timestamp, 
                  file, twsCon, ...)
            }
        }, error = function(e) {
            close(twsCon)
            print(e)
            stop("IB connection error. Connection closed", call. = FALSE)
        })
    }
}









# changed version, etc.  version=11 
reqMktData <- function (conn, Contract, tickGenerics = "100,101,104,106,165,221,225,236",
    snapshot = FALSE, tickerId = "1", timeStamp = "%Y%m%d %H:%M:%OS",
    playback = 1, file = "", verbose = TRUE, eventWrapper = eWrapper(),
    CALLBACK = twsCALLBACK, ...)
{
    if (!is.twsConnection(conn))
        stop("tws connection object required")
    if (!is.twsPlayback(conn)) {
        Contract <- as.twsContract(Contract)
        if (is.twsContract(Contract))
            Contract <- list(Contract)
        for (n in 1:length(Contract)) {
            if (!is.twsContract(Contract[[n]]))
                stop("twsContract required")
        }
    }
    con <- conn[[1]]
    if (!isOpen(con))
        stop("connection to TWS has been closed")
    cancelMktData <- function(con, tickerId) {
        if (inherits(con, "sockconn")) {
            for (i in 1:length(tickerId)) {
                writeBin(.twsOutgoingMSG$CANCEL_MKT_DATA, con)
                #writeBin("1", con)  
                writeBin("2", con) 
                writeBin(tickerId[i], con)
            }
        }
        else {
            seek(con, 0)
        }
    }
    if (is.null(CALLBACK))
        CALLBACK <- twsDEBUG
    snapshot <- ifelse(snapshot, "1", "0")
    if (snapshot == "1" && missing(tickGenerics))
        tickGenerics <- ""
    #VERSION <- "9"
    VERSION <- "11"
    fullSnapshot <- data.frame()
    symbols. <- NULL
    ticker_id <- as.character(tickerId)
    symbol.or.local <- function(x) {
        symbol <- x$symbol
        local <- x$local
        if (local == "") {
            return(symbol)
        }
        else return(local)
    }
    if (inherits(con, "sockconn")) {
        for (n in 1:length(Contract)) {
			if (Contract[[n]]$sectype == 'BAG') stop('BAG contract type in reqMktData not implemented')
            signals <- c(.twsOutgoingMSG$REQ_MKT_DATA,
				VERSION,
                ticker_id,
                Contract[[n]]$conId, #NEW
                Contract[[n]]$symbol, 
                Contract[[n]]$sectype,
                Contract[[n]]$expiry, 
                Contract[[n]]$strike, 
                Contract[[n]]$right,
                Contract[[n]]$multiplier, 
                Contract[[n]]$exch,
                Contract[[n]]$primary, 
                Contract[[n]]$currency,
                Contract[[n]]$local,
                #Contract[[n]]$tradingClass,  #NEW
                {if (is.null(Contract[[n]]$tradingClass)) "" else Contract[[n]]$tradingClass},  # gracefully fix NULL tradingClass
                "0",  #no contract.underComp?? in V11 or BAG in V9
                tickGenerics,
                snapshot,  #not using it
                ""  # no mktDataOptionsStr
                )
#cat('AAAAA','\n')                
#cat(signals,'\n',sep='-') 
            writeBin(signals, con)
#cat('BBBBB','\n')                            
            if (snapshot == "1") {
				stop("Snapshot not working ? so not supported") #soren
                eventWrapper <- eWrapper.snapshot()
                while (1) {
                  socketSelect(list(con), FALSE, NULL)
                  curMsg <- readBin(con, character(), 1)
                  processMsg(curMsg, con, eventWrapper, NULL,
                    file, ...)
                  if (curMsg == .twsIncomingMSG$TICK_SNAPSHOT_END) {
                    fullSnapshot <- rbind(fullSnapshot, data.frame(lastTimeStamp = eventWrapper$get.Data("lastTimeStamp"),
                      symbol = symbol.or.local(Contract[[n]]),
                      bidSize = eventWrapper$get.Data("bidSize"),
                      bidPrice = eventWrapper$get.Data("bidPrice"),
                      askPrice = eventWrapper$get.Data("askPrice"),
                      askSize = eventWrapper$get.Data("askSize"),
                      lastPrice = eventWrapper$get.Data("lastPrice"),
                      Volume = eventWrapper$get.Data("Volume"),
                      Open = eventWrapper$get.Data("Open"), High = eventWrapper$get.Data("High"),
                      Low = eventWrapper$get.Data("Low"), Close = eventWrapper$get.Data("Close")))
                    break
                  }
                }
                if (n == length(Contract))
                  return(fullSnapshot)
            }
            ticker_id <- as.character(as.numeric(tickerId) + n)
            symbols. <- c(symbols., symbol.or.local(Contract[[n]]))
        }
    }
    eventWrapper$assign.Data("symbols", symbols.)
    if (!missing(CALLBACK) && is.na(list(CALLBACK))) {
        if (is.twsPlayback(conn)) {
            seek(conn[[1]], 0)
            stop("CALLBACK=NA is not available for playback")
        }
        return(as.character(as.numeric(tickerId):length(Contract)))
    }
    if (snapshot == "0")
        on.exit(cancelMktData(con, as.character(as.numeric(tickerId):length(Contract))))
    if (!is.list(file))
        file <- list(file)
    if (length(file) != length(Contract))
        file <- rep(file, length(Contract))
    CALLBACK(conn, eWrapper = eventWrapper, timestamp = timeStamp,
        file = file, playback = playback, timeout = NULL, ...)
}


# unchanged
reqOpenOrders <- function (twsconn) 
{
    .reqOpenOrders(twsconn)
    con <- twsconn[[1]]
    eW <- eWrapper()
    while (TRUE) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, character(), 1L)
        processMsg(curMsg, con, eW)
    }
}





# reqExecutions # no change required if we use version=3 for the request.
reqExecutions <- IBrokers:::reqExecutions



cancelMktData <- function (conn, tickerId)
{
    if (!is.twsConnection(conn))
        stop("twsConnection object required")
    con <- conn[[1]]
    for (i in 1:length(tickerId)) {
        writeBin(.twsOutgoingMSG$CANCEL_MKT_DATA, con)
        #writeBin("1", con)
        writeBin("2", con)  #version
        writeBin(tickerId[i], con)
    }
}



# Added 'tradingClass' slot
twsContract <-
function (conId="0", symbol, sectype, exch, primary="", expiry="", strike="0", 
    currency, right, local, multiplier, combo_legs_desc, comboleg, 
    include_expired, secIdType = "", secId = "", tradingClass="") 
{
    if (is.null(names(match.call()[-1]))) 
        return(do.call("twsContract", rep(list(""), 14)))
    structure(list(conId = conId, symbol = symbol, sectype = sectype, 
        exch = exch, primary = primary, expiry = expiry, strike = strike, 
        currency = currency, right = right, local = local, multiplier = multiplier, 
        combo_legs_desc = combo_legs_desc, comboleg = comboleg, 
        include_expired = include_expired, secIdType = secIdType, 
        secId = secId, tradingClass = tradingClass), class = "twsContract")
}




# Added 'exemptCode' slot
twsComboLeg <- 
function (conId = 0, ratio = 0, action = c("BUY", "SELL", "SSHORT"), 
    exchange = NULL, openClose = 0, shortSaleSlot = 0, designatedLocation = NULL,
    exemptCode = -1) 
{
    structure(list(conId = conId, ratio = ratio, action = action[1], 
        exchange = exchange, openClose = openClose, shortSaleSlot = shortSaleSlot, 
        designatedLocation = designatedLocation, exemptCode = exemptCode, SAME = 0, OPEN = 1, 
        CLOSE = 2, UNKNOWN = 3), class = "twsComboLeg")
}


twsSTK <-
function (symbol, exch = "SMART", primary = "", strike = "0.0", 
    currency = "USD", right = "", local = "", multiplier = "", 
    include_expired = "0", conId = 0) 
{
    twsContract(conId, symbol, "STK", exch, primary, expiry = "", 
        strike, currency, right, local, multiplier, NULL, NULL, 
        include_expired)
}


twsCurrency <-
function (symbol, currency = "USD", exch = "IDEALPRO", primary = "", 
    strike = "0.0", right = "", local = "", multiplier = "", 
    include_expired = "0", conId = 0) 
{
    twsContract(conId, symbol, "CASH", exch, primary, expiry = "", 
        strike, currency, right, local, multiplier, NULL, NULL, 
        include_expired)
}



placeOrder<- function (twsconn, Contract, Order)
{
    if (!is.twsConnection(twsconn))
        stop("requires twsConnection object")
    if (!is.twsContract(Contract))
        stop("requires twsContract object for Contract arg")
    if (!inherits(Order, "twsOrder"))
        stop("requires twsOrder object for Order arg")
    con <- twsconn[[1]]
    VERSION <- "42"   # This is the version of placeOrder() only. It has nothing to do with
					  # the server version. But it does determine the fields that we need to
					  # send for each order !!!!
					  # from official IB API


	if (is.null(Order$hedgeType) | is.null(Order$hedgeParam)) stop(' NEW twsOrder has to be used')
	#if (is.null(Contract$tradingClass)) .... see below for a graceful fix ...

    if (Order$orderId == "")
        Order$orderId <- reqIds(twsconn)
	#print('Order$orderId'); print(Order$orderId)

    order <- c(
		.twsOutgoingMSG$PLACE_ORDER,
		VERSION,
		as.character(Order$orderId),
		as.character(Contract$conId), #NEW  "0"
        Contract$symbol,
        Contract$sectype,
        Contract$expiry,  # "", 
        Contract$strike,  # "0", 
        Contract$right,
        Contract$multiplier,
        Contract$exch,
        Contract$primary,  # "", 
        Contract$currency,
        Contract$local,
        #Contract$tradingClass, #NEW not using it.  # "", 
        {if (is.null(Contract$tradingClass)) "" else Contract$tradingClass},  #gracefully fix a NULL tradingClass
        Contract$secIdType,
        Contract$secId,
        Order$action,
        Order$totalQuantity,
        Order$orderType,
        Order$lmtPrice,
        Order$auxPrice,
        Order$tif,
        Order$ocaGroup,
        Order$account,
        Order$openClose,
        Order$origin,
        Order$orderRef,
        Order$transmit,
        Order$parentId,
        Order$blockOrder,
        Order$sweepToFill,
        Order$displaySize,
        Order$triggerMethod,
        Order$outsideRTH,
        Order$hidden)

#    if (Contract$sectype == "BAG") {
#        if (is.null(Contract$comboleg)) {
#            order <- c(order, 0)
#        }
#        else {
#            comboLeg <- Contract$comboleg
#            order <- c(order, length(comboLeg))
#            for (i in 1:length(comboLeg)) {
#                Leg <- comboLeg[[i]]
#                order <- c(order, Leg$conId, Leg$ratio, Leg$action,
#                  Leg$exch, Leg$openClose, Leg$shortSaleSlot,
#                  Leg$designatedLocation,
#                  Leg$exemptCode  #NEW
#                  )
#            }
#        }

#    }

    order <- c(order,
		"", # Legacy
		Order$discretionaryAmt,
		Order$goodAfterTime,
        Order$goodTillDate,  
        Order$faGroup,
        Order$faMethod,
        Order$faPercentage,
        Order$faProfile,
        Order$shortSaleSlot,  #  "0", 
        Order$designatedLocation,  
        Order$exemptCode, #NEW but not using it  # "-1", 
        Order$ocaType,  # "0", 
        Order$rule80A,
        Order$settlingFirm,
        Order$allOrNone,  # "0", 
        Order$minQty, 
        Order$percentOffset,
        Order$eTradeOnly,  # "1", 
        Order$firmQuoteOnly,  # "1", 
        Order$nbboPriceCap,
        Order$auctionStrategy,  # "0", 
        Order$startingPrice,
        Order$stockRefPrice,
        Order$delta,
        Order$stockRangeLower,
        Order$stockRangeUpper,  
        Order$overridePercentageConstraints,  
        Order$volatility,  #align OK
        Order$volatilityType, #????????
        Order$deltaNeutralOrderType,
        Order$deltaNeutralAuxPrice,
        # .... some here if we have deltaNeutralOrderType. but not using it
		#	"0",  #Order$deltaNeutralConId
		#	"",   #Order$deltaNeutralSettlingFirm
		#	"",   #Order$deltaNeutralClearingAccount
		#	"",   #Order$deltaNeutralClearingIntent
		#	"",   #Order$deltaNeutralOpenClose
		#	"0",   #Order$deltaNeutralShortSale
		#	"0",  #Order$deltaNeutralShortSaleSlot
		#	"",   #Order$deltaNeutralDesignatedLocation
        Order$continuousUpdate,  
        Order$referencePriceType,  
        Order$trailStopPrice,  
        Order$trailingPercent,  #NEW but not using it    # "", 
        Order$scaleInitLevelSize,   
        Order$scaleSubsLevelSize,   
        Order$scalePriceIncrement,  #  "", 
		#	if (Order$scalePriceIncrement != "") # .... some here if we have scalePriceIncrement. but not using it
		#		order <- c(order,
		#		"1.0", #Order$scalePriceAdjustValue
		#		"1", #Order$scalePriceAdjustInterval
		#		"1.0", #Order$scaleProfitOffset
		#		"0", #Order$scaleAutoReset
		#		"1", #Order$scaleInitPosition
		#		"1", #Order$scaleInitFillQty
		#		"0" #Order$scaleRandomPercent
		#		)
		Order$scaleTable,  # "", 
		Order$activeStartTime, # "", 
		Order$activeStopTime   # "" 
	)

	#Order$hedgeType,  #see below
	#Order$hedgeParam,  #see below
	if (Order$hedgeType != "") order <- c(order, Order$hedgeType, Order$hedgeParam) else order <- c(order, Order$hedgeType)  #NEW !! using it

	order <- c(order,
		Order$optOutSmartRouting,  #FALSE, #NEW  !! may use it in the future. Think about 'Flash Boys' book.
		Order$clearingAccount,  
		Order$clearingIntent,  
		Order$notHeld,  
		"0", #underComp # FALSE #NEW but not using it  
		Order$algoStrategy,      #NEW but not using it   # "" , 
		Order$whatIf, # "0", 
		"" # miscOptionsStr("")
	)

cat('placeOrder VERSION',VERSION,'\n')
cat(order,'\n',sep='*')


    writeBin(order, con)
    assign(".Last.orderId", as.integer(Order$orderId), .GlobalEnv)
    invisible(as.integer(Order$orderId))
}



####################################################################################
# New utility function. call immediately after a reqExecutions() call.
readExecutions <- function (twsconn) 
{
	#.reqOpenOrders(twsconn)
	con <- twsconn[[1]]
	eW <- eWrapper()
	while (TRUE) {
	socketSelect(list(con), FALSE, NULL)
	curMsg <- readBin(con, character(), 1L)			
	processMsg(curMsg, con, eW)
	if (curMsg == .twsIncomingMSG$EXECUTION_DATA_END) break
	}
}








