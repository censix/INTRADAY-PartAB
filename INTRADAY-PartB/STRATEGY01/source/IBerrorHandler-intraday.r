
IBerrorHandler <- function(con, verbose, OK=NULL, ...) {
  err <- readBin(con,character(),4)

  if(as.numeric(err[3]) %in% OK || as.numeric(err[3]) > 1000) {
	  errorcode <- err[3]
	  switch(errorcode,
		'1100'={
		  #close(con)  # instead of closing connection, we'll
		  print(paste('HANDLING ERROR:',err[3],err[4]))
		  if (is.null(IBCONNECTIONLOSSSTATUS)) IBCONNECTIONLOSSSTATUS<<-'*********'
		},
		'1101'=,'1102'={
		  print(paste('HANDLING ERROR:',err[3],err[4]))
		  if (!is.null(IBCONNECTIONLOSSSTATUS)) IBCONNECTIONLOSSSTATUS<<-NULL
		},
		'135' ={
		  #Error 135 means we tried to change/cancel an order that does not exist (anymore) in the tws, but is apparently still 'open' in the orderbook
		  # so here we set it to 'canceled' and forget about it.
		  print(paste('Received Error 135 with orderid', err[2]))
		  #print('BEGIN ERR')
		  #print(err)
		  #print('END ERR')
		  arglist <- list(...) #needs values: portfolio,symbol,stp
		  if (!is.na(as.numeric(err[2]))) {
			 stp<- arglist$stp
			 res<- IBupdateSingleOrder(portfolio=arglist$portfolio, symbol=arglist$symbol, ibclientid=arglist$clientId, iborderid=as.numeric(err[2]), status='open', newstatus='canceled', statustimestamp=stp)
			 if (!res)  res<-IBupdateSingleOrder(portfolio=arglist$portfolio, symbol=arglist$symbol, ibclientid=arglist$clientId, iborderid=as.numeric(err[2]), status='replaced', newstatus='canceled', statustimestamp=stp)
			 print(paste('Order causing Error 135 cancel successful:',res))
			 return(res)
		  } else stop('Something is wrong with the orderid we received with the 135 error')
		},
		'201'={ #201 Order rejected - reason:Cannot cancel the filled order"
		     if (!is.null(ORDERTIMEOUTSTATUS)) {
		       if (!is.na(as.numeric(err[2]))) {
			     idx<-match(as.numeric(err[2]), ORDERTIMEOUTSTATUS)
			     if (!is.na(idx)) ORDERTIMEOUTSTATUS <<- ORDERTIMEOUTSTATUS[-idx]
			     if (length(ORDERTIMEOUTSTATUS)==0) ORDERTIMEOUTSTATUS <<- NULL
			   } else stop('Something is wrong with the orderid we received with the 201 error')
			 }
		},
		{
			print(paste('IGNORED ERROR:',err[3],err[4]))
		}
	  )
	  if (!is.null(SHUTDOWNSTATUS)) {
		arglist <- list(...)
	    allopenordersOB <- IBgetOrdersTimespan(portfolio = arglist$portfolio, symbol = arglist$symbol,
		   status = 'open', timespan = '::', ordertype = NULL,  side = NULL)  # timespan = paste('::',as.character(stp),sep='')
	    if (nrow(allopenordersOB)==0 & SHUTDOWNSTATUS=='WAIT_CANCEL_ALL_THEN_CLOSE_ALL_THEN_STOP')   SHUTDOWNSTATUS <<-  'DO_CLOSE_ALL_THEN_STOP'
	    if (nrow(allopenordersOB)==0 & SHUTDOWNSTATUS=='WAIT_CLOSE_ALL_THEN_STOP' & getPosQty(Portfolio=arglist$portfolio, Symbol=arglist$symbol, Date=Sys.time()) == 0)   SHUTDOWNSTATUS <<- 'DO_STOP'
	  }
	  return(TRUE)

  } else {
    #if(verbose > 0) warning(paste(.twsERR[err[3],]))
    print(paste('ERROR:',err[3],err[4]))
    return(FALSE)
  }
}

# In 'datafeed' . there is no disconnect upon receiving a 1100


