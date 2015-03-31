#Create the Account and Portfolio files. Existing files will be overwritten without warning.
#
require(blotter)
#require(quantmod) #load quantstrat and dependencies: blotter,quantmod,TTR,xts,....
require(IBrokers)
source('source/__IBapi-clientVer63-v002.r') # required. Partial implementation of the IB API in R, for TWS >=9493
source('source/IBorderBook-intraday.r')  #Provides order_book functionality

# Very important. use delay=1e-3 NOT 1e-5, which goes beyond our current precision limit with 15 bit double's
# See http://r.789695.n4.nabble.com/POSIXct-value-display-incorrect-for-some-values-td4311446.html
options(digits.secs=3)


#Clean-up
#try(rm("currentStrategy"),silent=TRUE) #assume strategy object is named "currentStrategy"
try(rm("account.AC1","portfolio.PF1",pos=.blotter),silent=TRUE)
try(rm("ibContract"),silent=TRUE)

#Only one symbol per portfolio supported
symbol='EUR'  # for'EURUSD'


# Set initial values for account and portfolio
initDate='1970-03-29 13:28:00.001'
#initDate<-as.character(as.POSIXct(start(get(symbol))))
initEq<-2000

currency('USD') #this is not correct, but with EUR we get an error *in quantstrat?)
stock(symbol,currency='USD',multiplier=1) #currency= has to be the same as the one declared in the line above.

# Initialize portfolio, account and orderbook
initPortf('PF1', symbols=symbol, initDate=initDate)
initAcct('AC1', portfolios='PF1', initEq=initEq, initDate=initDate)
# Define our own orderbook structure + accessor functions, etc. #
# it should have at least the fields that QS has, but more customizable fields.
#This also needs to create a .strategy environment, where the order_book will be
.strategy <- new.env()
IBinitOrders(portfolio='PF1', initDate=initDate)  #this is a QS function.
#assign("order_book.PF1",NULL,envir=.strategy)

# Create the clientId for IB  API - created elsewhere
#ibClientId <- 90

# Create the contract to trade at IB and the datafeed file!
ibContract <- twsCurrency(symbol='EUR', currency='USD') #same as defined in datafeed-config.r
ibDatafeedShareFile =  '../../INTRADAY-PartA/datafeed-shared/EURUSD--shared.bin'


# Directory where the shared realtime datafeed can be accessed (relative path)
#IBdatafeedShareDir =   '/home/leaf/portfolios/datafeed/datafeed-shared'


# Create the EMPTY lookup list where used IB orderIds will be stored
# When the first order is placed, the correct value for orderId will be
# acquired from the TWS connection
ibLookupOrderId <- NULL
.Last.orderId <- 0


#VARIABLE
#Store/save IB contract specifications (constant) and ibLookupOrderId (variable) from global environment.
save(list=c('ibContract', 'ibDatafeedShareFile', 'ibLookupOrderId', '.Last.orderId'), file="ib.rdata" )

#VARIABLE
#Store/save existing OrderBook from the .strategy environment.
save("order_book.PF1", file="strat-portf-obook.rdata", envir=.strategy )

#VARIABLE
#Store/save existing portfolio and the account from the .blotter environment.
save(list=c("portfolio.PF1","account.AC1"), file="portf-acct.rdata", envir=.blotter )

#CONSTANT
#Store/save definitions from the .instrument environment.
#save(list=ls(envir=.instrument), file="instr.rdata", envir=.instrument )
save(list=ls(envir=FinancialInstrument:::.instrument), file="instr.rdata", envir=FinancialInstrument:::.instrument )

