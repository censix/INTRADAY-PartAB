
Soren Wilkening.updated March 2015


Files and what they are for:
============================


01run-paper.r	
   :Primary entry point. Once 'datafeed-config.r' is configured properly, 
   run this with source('01run-paper.r') and watch the data coming in.

datafeed.r	
   :Main loop

datafeed-config.r	
   :Configuration settings. Contract definitions, bar-sizes, etc.

IBappendXtsToHistory.r	
   :Provides support for keeping history files

IBemailNotify.r
   :Provides support for sending emails

IBeWrapper.RealTimeBars.SHARED.r	
   :Support for Main loop event processing	

IBgetShared.r
   :Provides support to reading mmap files

IBsmsNotify.r
   :Provides support for sending SMS messages

_checkSharedBinXTS.R	
   :Utility to 'manually' inspect mmap files

__IBapi-clientVer63-v002.r
   :Partial implementation of the IB API in R - works with TWS/GW version >=9493

README.txt		
   :Guess what ?



Directories and what they are for:
==================================

datafeed-raw	
   :contains .csv dumps of incoming data

datafeed-shared	
   :contains mmap files with .bin extension.  These files need to be referenced from the INTRADAY-PartB module. 
   more concretely, in file "IBcreateExecPortfAcct-intraday.r"  you should modify the following line
         ibDatafeedShareFile =  '/home/leaf/portfolios/datafeed/datafeed-shared/EURUSD--shared.bin'
   so that it points to the .bin file that corresponds to your datafeed.

rdata		
   :history files with .rdata extension, containing the datafeed as xts object.



FAQ: Why do we have two R processes, one for PartA, one for PartB, rather than doing everything in a single R process ?

The main reason is that handling the datafeed AND the strategy execution in one single R process becomes extremely messy. I have found it much more intuitive to use two separate R processes. A second reason is that if you wanted to use a different datafeed provider other than IB ( while continuing to place the orders through IB) you could do this very easily without having to rewrite your strategy code. You would just need to replace PartA with your provider specific code, while PartB would not have to be changed at all!



   
