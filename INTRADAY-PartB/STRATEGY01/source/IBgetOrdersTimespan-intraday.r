
IBgetOrdersTimespan<-function (portfolio, symbol, status = "open", timespan = NULL, 
    ordertype = NULL, side = NULL, which.i = FALSE) 
{
# Note that timespan has to be a continuous timespan.
    orderbook <- IBgetOrderBook(portfolio)
    if (!any(names(orderbook[[portfolio]]) == symbol)) 
        stop(paste("symbol", symbol, "does not exist in portfolio", 
            portfolio, "having symbols", names(orderbook[[portfolio]])))
#print(timespan)
    ordersubset <- orderbook[[portfolio]][[symbol]][timespan,]  #Date subsetting
#print('ordersubset')
#print(ordersubset)
    if ( nrow(ordersubset)==0) {return(ordersubset)}
    if (!is.null(status) & !length(grep(status, c("open", "closed", 
        "canceled", "replaced"))) == 1) 
        stop(paste("order status:", status, " must be one of \"open\", \"closed\", \"canceled\", or \"replaced\""))
    if (!is.null(ordertype)) {
        if (is.na(charmatch(ordertype, c("market", "limit", "stoplimit", 
            "stoptrailing", "iceberg")))) {
            stop(paste("ordertype:", ordertype, " must be one of \"market\",\"limit\",\"stoplimit\", \"stoptrailing\", or \"iceberg\""))
        }
    }
    allorders <- orderbook[[portfolio]][[symbol]]
    # the indices that fulfil the criteria
    indices <- which((if (!is.null(status)) 
        allorders[, "Order.Status"] == status
    else TRUE) & (if (!is.null(ordertype)) 
        allorders[, "Order.Type"] == ordertype
    else TRUE) & (if (!is.null(side)) 
        allorders[, "Order.Side"] == side
    else TRUE))
    # the indices that are in the timespan 
    tsstart <- first(allorders[timespan])
    tsend   <- last(allorders[timespan]) 
    indices2 <- which( index(allorders)>=index(tsstart) & 
                       index(allorders)<=index(tsend)
                      )
    # intersect to get final
    indices <- intersect(indices, indices2) 
    if (isTRUE(which.i)) { 
        return(indices)
    } else {
#print(indices)
        #ordersubset <- orderbook[[portfolio]][[symbol]][indices,] #bug
        ordersubset <- allorders[indices,]
        return(ordersubset)
    }
}
