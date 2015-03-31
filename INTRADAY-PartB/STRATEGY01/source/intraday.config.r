
###########################################################################
# Core configuration settings. DO NOT TOUCH THESE unless unavoidable
###########################################################################
intraday.core.glPortfolio <- 'PF1'
intraday.core.glAccount   <- 'AC1'

# These were carefully calibrated.
intraday.core.glTimeoutSocket <- 0.5 # <0.2 does somehow block new mktdata? # number of seconds after which a socket READ attempt will time out, i.e. if there is nothing to read.
intraday.core.glSleeptime     <- 0.1 #Dont go much below 0.1 seconds since it will dramatically increase processor load. don't know why.


###########################################################################
# Adjustable configuration settings. may need adjustment depending on the type of exchange/instrument we trade.
###########################################################################
## Daily closeout
# 24h time at which we want to clse our positions
intraday.core.glTodayCloseoutTime   <- as.POSIXct(paste( with(as.POSIXlt(Sys.Date()),paste(1900+year,1+mon,mday,sep='-')) ,'17:24:03')) # 17:15.

## orderTimeoutEvent
# orders not filled within 'timeoutFill' seconds will be canceled
intraday.core.timeoutFill <- 120 #in seconds

## dataTimeoutEvent: has to have two levels, l1 and l2, defined in terms of frequencies:
#
#	level1 = c(num. of mktdata bars1 , timeinterval1)
#	level2 = c(num. of mktdata bars2 , timeinterval2)
#  Example: We have a barsize of 15sec, so we can expect 8 bars in 2 minutes (120sec) in a healthy stream.
#	We still trust the stream enough for trading as long as we get at least 4 bars in 120 secs.
#	If we get less than that, we block the strategy from issuing new orders but continue to exit
#	an existing position normally. If we receive less than 1 bar in 10 minutes (600sec) we want
#	to force an exit for exising positions. In order to implement this we can set
#		level1 = c(count=4,interval=120)
#		level2 = c(count=1,interval=600)
#	We have to make sure that l1$count/l1$interval > l2$count/l2$interval. Then we assign events as follows.
#	if (num.bars per level1$interval  < level1$count) someDataTimeoutEvent<-'l1'
#	if (num.bars per level2$interval  < level2$count) someDataTimeoutEvent<-'l2'
#
#'defcount' is the *minimum* number of events required in 'definterval'
#'firstidx' is the current index of the oldest event that is still equal or younger than 'definterval' seconds
#'n'        is the current number of events that took place in the past 'definterval' seconds
#
intraday.core.ecounterl1<-list(defcount=4,definterval=120,firstidx=0,n=0,label='level1')   # 4 bars in 2 minutes, or more
intraday.core.ecounterl2<-list(defcount=1,definterval=1800,firstidx=0,n=0,label='level2')   # 1 bar  in 30 minutes, or more

## someIBConnectionLossEvent
# a timer on a specific status. if the status is not NULL for longer than definterval, the timer event is triggered
intraday.core.etimer1<-list(definterval=100,defnotify=60,firsttime=NULL) # timer for IBCONNECTIONLOSSSTATUS timeout: 1 minute 40 secs. notification after 1 minute

## timer for periodic FODIC/keepalive checking. keepalive reqs will not be done if we have SHUTDOWN or IBCONNECTIONLOSS
intraday.core.eperiodic1 <- list(definterval=4*60,lasttime=NULL) # every 14 minutes

##
intraday.core.policy.PartialFillCancellation <- 'RetryRemainder'  #or 'ExitAtMarketprice'  #


