INTRADAY-B
===========


Contents:


README.txt  
     This file.


STRATEGY01/
     Contains the example strategy on EURUSD.
     
     Use:
     1) Initialize the strategy once by calling 'IBcreateExecPortfAcct-intraday.r', which will generate the necessary .rdata files.
     2) Every day, after market open, execute '00launch-intraday.r'. All positions are automatically closed at 22:04 h
     
     Files:
       00launch-intraday.r                        Principal entry point for strategy execution
       source                                     Contains the core event processing and execution system files
       lib                                        Contains common indicator functions files       
       IBcreateExecPortfAcct-intraday.r           Strategy initialization 
       intraday.config.local.r                    Local definitions for the core event processing and execution system
       userStrategyEvents.r                       user-defined *actual* strategy       
       userStrategyEvents-SIMPLEBBANDS01.r        Dummy strategy example 
       userStrategyEvents-SIMPLEBREAKOUT01.r      Dummy strategy example
       

STRATEGY01/lib/ 
     Contains frequently needed common indicator functions use in strategies.


STRATEGY01/source/
     Sources for the core event processing and execution system. Entry point is 'IBexecutePure-intraday.R'


Note: The example works with a OHLC 15-sec bar datafeed of EURUSD as delivered by IBrokers. It can also be used
for Futures.

S.Wilkening
Updated: March 2015

