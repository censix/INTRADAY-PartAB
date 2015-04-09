
Soren Wilkening.updated March 2015

Quickstart:  
====================
Make sure you have the following R packages installed, either from CRAN or r-forge:  
  quantmod, mmap, IBrokers, blotter, FinancialInstrument.

To run the dummy EURUSD strategy, do the following:
Open two terminals in INTRADAY-PartAB/

==== Initialize strategy (terminal 1)

  cd INTRADAY-PartB/STRATEGY01 ; R --vanilla <IBcreate*

==== Run datafeed (terminal 1)

  cd ../../INTRADAY-PartA ; R --vanilla <01run*


==== Run strategy (terminal 2)

  cd INTRADAY-PartB/STRATEGY01 ; R --vanilla <00launch*
