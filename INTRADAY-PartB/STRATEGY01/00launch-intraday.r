
#Strategy is executed against a portfolio "PF1" and account "AC1" 
#in the IB production (or paper) trading environment
require(quantmod) #blotter,quantmod,TTR,xts, ....
require(IBrokers)
source('source/__IBapi-clientVer63-v002.r') # required. Partial implementation of the IB API in R, for TWS >=9493

#create a new portfolio and account
#source("IBcreateExecPortfAcct-intraday.r")
ibClientId <- 96


#global: create the connection to IB  (this can probably not be stored anywhere, so
#we have to recreate it within "IBexecuteStrategy.r"
ibConnection<-NULL
if (!is.twsConnection(ibConnection)) {
   ibConnection<- ibgConnect(clientId=ibClientId, host="localhost",
                                      port=5001, verbose=TRUE,
                                      timeout=15, filename=NULL)
}


#source('intraday.config.r')
source('userStrategyEvents.r')
source('source/IBexecutePure-intraday.R') #here, call the main event loop









