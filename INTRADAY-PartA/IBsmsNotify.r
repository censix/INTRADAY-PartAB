
# IBsmsNotify will send an sms Notification with Contract and Order information.
# !! IMPORTANT !!
# This function MUST NOT WAIT for the sending to complete. It should spawn a child shell and the forget about it
# !! IMPORTANT !!
# This file can be removed from the sources directory, and everything else will still run

.smssend <- function(msgBODY)  {  
  #tmpCommand <- .... command for sending SMS here
  #system(command=tmpCommand , ignore.stdout = TRUE, ignore.stderr = TRUE, intern = FALSE, wait = FALSE )   #wait=FALSE is very important in order to not interrupt/delay the R processing
}

IBsmsMsg <- function(msgBODY='', ...) {    
  .smssend(msgBODY) 
}


IBsmsNotify <- function(emType=NULL,emSymbol=NULL, emAction=NULL, emTotalQuantity=NULL, emLmtPrice=NULL, ...) {
  if (emType=='ORDER') {
    tmpNotifyBODY <- paste('Order Details:', emAction, emTotalQuantity ,emSymbol, emLmtPrice)
    #tmpNotifyHEAD <- paste('Placed',emAction,'order for ',emSymbol)
  }
  if (emType=='EXECUTION') {
    tmpNotifyBODY <- paste('Execution Details:', emAction, emTotalQuantity ,emSymbol, emLmtPrice)
    #tmpNotifyHEAD <- paste('Executed',emAction,'order for ',emSymbol)
  }    
  .smssend(msgBODY=tmpNotifyBODY) 
}

