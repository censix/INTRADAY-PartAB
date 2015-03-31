
Soren Wilkening.updated March 2015

Quickstart:  
====================
to run the dummy EURUSD strategy, do the following:
Open two terminals in INTRADAY-PartAB/

==== Initialize strategy (terminal 1)

  cd INTRADAY-PartB/STRATEGY01 ; R --vanilla <IBcreate*

==== Run datafeed (terminal 1)

  cd ../../INTRADAY-PartA ; R --vanilla <01run*


==== Run strategy (terminal 2)

  cd ../../INTRADAY-PartB/STRATEGY01 ; R --vanilla <00launch*
