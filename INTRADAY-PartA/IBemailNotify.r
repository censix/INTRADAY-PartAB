
# IBemailNotify will send an email Notification with Contract and Order information.
# !! IMPORTANT !!
# This function MUST NOT WAIT for the sending to complete. It should spawn a child shell and the forget about it
# !! IMPORTANT !!
# This file can be removed from the sources directory, and everything else will still run

.emailsend <- function(msgBODY, msgHEAD)  { 
  if (exists('ibmsgPREFIX')) msgHEAD <- paste(as.character(ibmsgPREFIX), msgHEAD) #prepend a globally defined prefix, i.e. to distinguish CASH from PAPER trading
  if (exists('ibmsgPREFIX')) msgBODY <- paste(as.character(ibmsgPREFIX), msgBODY) #prepend a globally defined prefix, i.e. to distinguish CASH from PAPER trading  
  #delay sending by a random number of 10 second intervals, but never more than 180 secs, to avoid possible collisions with other processes. needs to be fixed properly
  tmpCommand <- paste('sleep ',as.integer(10*as.integer(runif(1,0,18))),'; echo "',msgBODY,'" | sendemail -f sender@mail.my -xu dummy -xp dummypass -t recipient@mail.my -s smtp.mail.my -u "', msgHEAD, '" >/dev/null 2>&1', sep='')
  system(command=tmpCommand , ignore.stdout = TRUE, ignore.stderr = TRUE, intern = FALSE, wait = FALSE )   #wait=FALSE is very important in order to not interrupt/delay the R processing
}

IBemailMsg <- function(msgBODY='', msgHEAD='', ...) {    
  .emailsend(msgBODY, msgHEAD) 
}

IBemailNotify <- function(emType=NULL,emSymbol=NULL, emAction=NULL, emTotalQuantity=NULL, emLmtPrice=NULL, ...) {
  if (emType=='ORDER') {
    tmpNotifyBODY <- paste('Order Details:', emAction, emTotalQuantity ,emSymbol, emLmtPrice)
    tmpNotifyHEAD <- paste('Placed',emAction,'order for ',emSymbol)
  }
  if (emType=='EXECUTION') {
    tmpNotifyBODY <- paste('Execution Details:', emAction, emTotalQuantity ,emSymbol, emLmtPrice)
    tmpNotifyHEAD <- paste('Executed',emAction,'order for ',emSymbol)
  }    
  if (emType=='CANCEL') {
    tmpNotifyBODY <- paste('Cancellation Details:', emAction, emTotalQuantity ,emSymbol)
    tmpNotifyHEAD <- paste('Cancelled',emAction,'order for ',emSymbol)
  }    
  
  .emailsend(msgBODY=tmpNotifyBODY, msgHEAD=tmpNotifyHEAD)  
}
