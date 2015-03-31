# Functions needed to provide 'quantstrat like' order_book management.
require(blotter)


IBgetOrderBook<-function (portfolio)
{
    if (!grepl("order_book", portfolio))
        orders <- try(get(paste("order_book", portfolio, sep = "."),
            envir = .strategy), silent = TRUE)
    else orders <- try(get(portfolio, envir = .strategy), silent = TRUE)
    if (inherits(orders, "try-error"))
        stop(paste("Orders for ", portfolio, " not found, use initOrders() to create a new order book for this portfolio"))
    if (!inherits(orders, "order_book"))
        stop("Order Book for portfolio", portfolio, "does not appear to name an order book object.")
    return(orders)
}



IBinitOrders<-function (portfolio = NULL, symbols = NULL, initDate = "1999-12-31", ...)
{
    orders <- try(IBgetOrderBook(portfolio), silent = TRUE)
    if (inherits(orders, "order_book")) {
        stop(paste("Order Book for portfolio", portfolio, "already exists."))
    }
    else {
        orders <- list()
        orders[[portfolio]] <- list()
    }
    ordertemplate <- xts(as.matrix(t(c(0, NA, "init", "long",
        0, "closed", as.character(as.POSIXct(initDate)), 1, 0))),
        order.by = as.POSIXct(initDate), ... = ...)               #PENDING# Modify this to include more columns... do the same in  updateSingleOrder
    colnames(ordertemplate) <- c("Order.Qty", "Order.Price",
        "Order.Type", "Order.Side", "Order.Threshold", "Order.Status",
        "Order.StatusTime", "Order.Set", "Txn.Fees")              #PENDING# Modify this to include more columns... do the same in  updateSingleOrder
    if (is.null(symbols)) {
        pfolio <- getPortfolio(portfolio)
        symbols <- names(pfolio$symbols)
    }
    if (!is.null(symbols)) {
        for (symbol in symbols) {
            orders[[portfolio]][[symbol]] <- ordertemplate
        }
    }
    else {
        stop("You must specify a symbols list or a valid portfolio to retrieve the list from.")
    }
    class(orders) <- "order_book"
    assign(paste("order_book", portfolio, sep = "."), orders,
        envir = .strategy)
}


# had to chagne delay default from 1e-5 to 1e-3 due to precision problems when converting vrom time to string. POSIXct
# http://r.789695.n4.nabble.com/POSIXct-value-display-incorrect-for-some-values-td4311446.html

IBaddOrder <-
function (portfolio, symbol, timestamp, qty, price, ordertype,
    side, threshold = NULL, status = "open", statustimestamp = "",
    delay = 1e-03, tmult = FALSE, replace = TRUE, return = FALSE,
    ..., TxnFees = 0)
{
    if (!is.numeric(qty))
        stop(paste("Quantity must be numeric:", qty))
    if (qty == 0)
        stop("qty", qty, "must be positive or negative")
    if (is.null(qty))
        stop("qty", qty, "must not be NULL")
    if (is.na(qty))
        stop("qty", qty, "must not be NA")
    if (!is.numeric(price))
        stop(paste("Price must be numeric:", price))
    if (price == 0)
        stop("price", price, "must be positive or negative")
    if (is.null(price))
        stop("price", price, "must not be NULL")
    if (is.na(price))
        stop("price", price, "must not be NA")
    if (!is.null(side) & !length(grep(side, c("long", "short"))) ==
        1)
        stop(paste("side:", side, " must be one of 'long' or 'short'"))
#    if (is.na(charmatch(ordertype, c("market", "limit", "stoplimit", "stoptrailing", "iceberg"))))
#        stop(paste("ordertype:", ordertype, " must be one of \"market\",\"limit\",\"stoplimit\",\"stoptrailing\", or\"iceberg\""))
    if (!is.null(threshold) & length(price) >= 1) {
        if (length(grep(ordertype, c("stoplimit", "stoptrailing","iceberg"))) == 1) {
            switch(ordertype, stoplimit = , iceberg = {
                if (isTRUE(tmult)) {
                  price = price * threshold
                } else {
                  price = price + threshold
                }
            }, stoptrailing = {
                if (isTRUE(tmult)) {
                  threshold = (price * threshold) - price
                  tmult = FALSE
                }
                price = price + threshold
            })
        }
        else {
            stop(paste("Threshold may only be applied to a stop or iceberg order type",
                ordertype, threshold))
        }
    }
    if (is.null(threshold))
        threshold = NA
    if (!length(grep(status, c("open", "closed", "canceled",
        "replaced"))) == 1)
        stop(paste("order status:", status, " must be one of \"open\", \"closed\", \"canceled\", or \"replaced\""))
    if (!is.null(timestamp) & length(timestamp) >= 1) {
        timespan <- paste("::", timestamp, sep = "")
    }
    else {
        timespan = paste(index(first(orderbook), index(last(orderbook)),
            sep = "::"))
    }
    statustimestamp = NA
    if (length(price) > 1) {
      stop('in IBaddOrder: price has to be a single value. multiple prices and order.sets are not supported at the moment..')
#        order.set <- max(getOrders(portfolio = portfolio, symbol = symbol,
#            status = "open", timespan = timespan, ordertype = NULL,
#            side = NULL, which.i = FALSE)$Order.Set)
#        if (is.na(order.set))
#            order.set <- 1
    }
    else {
        order.set = NA
    }
    if (!length(qty) == length(price))
        qty <- rep(qty, length(price))
    if (!length(ordertype) == length(price))
        ordertype <- rep(ordertype, length(price))
    if (!length(threshold) == length(price))
        threshold <- rep(threshold, length(price))
    if (is.timeBased(timestamp))
        ordertime <- timestamp + delay
    else ordertime <- as.POSIXct(timestamp) + delay
    order <- NULL
    for (i in 1:length(price)) {
        neworder <- xts(as.matrix(t(c(as.numeric(qty[i]), price[i],
            ordertype[i], side, threshold[i], status, statustimestamp,
            order.set, TxnFees))), order.by = (ordertime))
        if (is.null(order))
            order <- neworder
        else order <- rbind(order, neworder)
    }
    if (ncol(order) != 9) {
        print("bad order(s):")
        print(order)
        next()
    }
    if (!isTRUE(return)) {
        if (isTRUE(replace))
        stop('in IBaddOrder: replace=TRUE has not been implemented.')
#            updateOrders(portfolio = portfolio, symbol = symbol,
#                timespan = timespan, ordertype = ordertype, side = side,
#                oldstatus = "open", newstatus = "replaced", statustimestamp = timestamp)
        orderbook <- IBgetOrderBook(portfolio)
        orderbook[[portfolio]][[symbol]] <- rbind(orderbook[[portfolio]][[symbol]],
            order)
        assign(paste("order_book", portfolio, sep = "."), orderbook,
            envir = .strategy)
        rm(orderbook)
        return()
    }
    else {
        return(order)
    }
}


##########

# Some generic helper functions for storing and retrieving variables in the .strategy envir.
getit <- function(v) {
	stopifnot(inherits(v,'character'))
	ret<- try( get(v,envir=.strategy) ,silent=TRUE)
	if (inherits(ret,'try-error')) return(NULL)
	return(ret)
}

setit <- function(v,val) {
	stopifnot(inherits(v,'character'))
	assign(v,val,envir=.strategy)
}



