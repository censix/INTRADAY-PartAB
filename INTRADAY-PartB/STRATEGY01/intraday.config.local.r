
#

###########################################################################
# Adjustable configuration settings. may need adjustment depending on the type of exchange/instrument we trade.
###########################################################################
## Daily closeout
# 24h time at which we want to clse our positions
intraday.core.glTodayCloseoutTime   <- as.POSIXct(paste( with(as.POSIXlt(Sys.Date()),paste(1900+year,1+mon,mday,sep='-')) ,'22:17:03')) # 17:17.


