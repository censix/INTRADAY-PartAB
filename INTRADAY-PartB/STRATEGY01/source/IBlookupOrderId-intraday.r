require(xts)

IBLookupOrderId.init <- function() {
	tmp <- list(maxorderid = 0, content = list() )
	return(tmp)
}


IBLookupOrderId.add <- function(lu, item) {
	#item<-list( as.character(tws.orderId), as.character(conn$clientId), portfolio, symbol, index(tmpneworder) )
	if (length(item)!=5) stop('bad ibLookupOrderId entry')
	if (!is.timeBased(item[[5]])) stop('obtimestamp needs to be a POSIXct object')
	newrow <- list(
		orderId= as.numeric(item[[1]]),
		clientId= as.numeric(item[[2]]),
		portfolio= item[[3]],
		symbol= item[[4]],
		obtimestamp= item[[5]]
	)
	lu$content[[length(lu$content)+1]] <- newrow
	lu$maxorderid <- max(lu$maxorderid, as.numeric(item[[1]]))
	return(lu)
}


#tmp<-list( '13', 90, 'PF1', 'EUR', Sys.time() )
#ll <- IBLookupOrderId.add(IBLookupOrderId.init(), list( '13', 90, 'PF1', 'EUR', Sys.time() ))
#ll <- IBLookupOrderId.add(ll, list( '15', 90, 'PF1', 'EUR', Sys.time() ))
#ll <- IBLookupOrderId.add(ll, list( '17', 90, 'PF1', 'EUR', Sys.time() ))


IBLookupOrderId.getOBts <- function(lu, paraml) {
#paraml<-list(iborderid,ibclientid,portfolio,symbol)
	tmp<- lapply(lu$content, function(li){
		if (li$orderId==as.numeric(paraml[[1]]) &
			li$clientId==as.numeric(paraml[[2]]) &
			li$portfolio==paraml[[3]] &
			li$symbol==paraml[[4]]) return(li$obtimestamp) else return(NULL)		
	})
	tmp<-unlist(tmp)
	if (length(tmp)>1) stop('IBLookupOrderId.getOBts. matched more than one row!?')
	if (!is.null(tmp))  return(.POSIXct(tmp)) else return(NULL)	
}


#res<-IBLookupOrderId.getOBts(ll, list(17,92,'PF1','EUR')) #NULL
#res<-IBLookupOrderId.getOBts(ll, list(15,90,'PF1','EUR')) #[1] "2012-08-02 16:17:20 CEST"
#print(as.numeric(res),digits=20)


IBLookupOrderId.getOrderIds <- function(lu, tsl, clientId, portfolio, symbol) {
# tsl<-list(timestamp1, timestamp2, ...)   list of POSIXct timestamps
# clientId, portfolio, symbol  as usual
	tmp<- lapply(lu$content, function(li){
		if (li$obtimestamp %in% tsl &
			li$clientId==as.numeric(clientId) &
			li$portfolio==portfolio &
			li$symbol==symbol) return(li$orderId) else return(NULL)		
	})
	tmp<-unlist(tmp)
	if (!is.null(tmp))  return(as.numeric(tmp)) else return(NULL)	
}

#key1<-ll$content[[1]]$obtimestamp
#key2<-ll$content[[2]]$obtimestamp
#key3<-ll$content[[3]]$obtimestamp
#IBLookupOrderId.getOrderIds(ll, list(), 90, 'PF1', 'EUR')


