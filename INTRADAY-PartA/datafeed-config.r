#  * ClientId. Better to choose a high number since we will have at least one or more Execution processes running with id >=1
IBclientid = 99


#   * List of IB contract specifications
#				symbol, (DAX)
#				exchange, (DTB)
#				currency (EUR), 
#				outputbarsize=60    # Has to be an integer multiple of 5sec, because IB only delivers RealTimeBars of 5secs!!
#				keepcsvhistory= FALSE   #When FALSE, older raw .csv files will be overwritten. if TRUE, append (these files are BIG)
#				keepoutputhistory=TRUE  #if TRUE the contents of the *-shared.bin file will first be appended to a history 
#                                       #file (SYMBOL.rdata in xts format) before is is overwritten by new data


#!! OBVIOUS: the client that places orders for this contract has to specify the contract in the !same way! but can use exchange=''SMART'

IBcontractSpecs <- list(
# Eurex Futures - 60sec bars
list(symbol='ESTX50', type='future', exchange='DTB', primary='',currency='EUR',expiry='', outputbarsizeseconds=60L, keepcsvhistory=FALSE, keepoutputhistory=TRUE), # EUROSTOXX50 Future
list(symbol='DAX',    type='future', exchange='DTB', primary='',currency='EUR',expiry='', outputbarsizeseconds=60L, keepcsvhistory=FALSE, keepoutputhistory=TRUE), # 
# Currencies - 30sec bars
list(symbol='EUR', type='currency', exchange='', primary='',currency='GBP', outputbarsizeseconds=30L, keepcsvhistory=FALSE, keepoutputhistory=TRUE),	#GBP Currency, base EUR
list(symbol='EUR', type='currency', exchange='', primary='',currency='USD', outputbarsizeseconds=30L, keepcsvhistory=FALSE, keepoutputhistory=TRUE)	#USD Currency, base EUR
)


#  * Directories 
IBdatafeedRawDir =     'datafeed-raw'     # contains raw output data from IB:           'SYMBOL-raw.csv'
IBdatafeedShareDir =   'datafeed-shared'  # contains file-based shared mmap structures: 'SYMBOL-EXCHANGE-shared.bin'
IBdatafeedHistoryDir = 'rdata/intraday/ibrokers/60sec'  # contains historical SYMBOL_xts data in files named: 'SYMBOL.rdata'
## WARNING ## note that new data will only be appended to the .rdata file at the beginning of the NEXT session. 

