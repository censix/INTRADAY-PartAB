#################
# read all the *.bin files from directory, converts the data to xts and assigns it to the symbol derived from the filename.
#################
library(mmap)
library(xts)
#library(quantmod) #only needed for plotting

source('datafeed-config.r')

allshares<-dir(IBdatafeedShareDir, pattern=glob2rx('*.bin'))

THEextractFUN<- function(x) na.trim( .xts( as.matrix(cbind(x$open,x$high, x$low, x$close, x$volume, x$wap, x$count)), x$timestamp), sides='right', is.na='all')
for (tmpfile in allshares) {
   m <- mmap(paste(IBdatafeedShareDir,'/',tmpfile,sep=''), struct(timestamp=double(),open=double(),high=double(),low=double(),close=double(),volume=double(),wap=double(),count=double() ), extractFUN=THEextractFUN)
   #print(last(m[]))  #Gets us the last non-NA entry!!
   tmpxts<-m[]
   colnames(tmpxts)<-c('Open','High','Low','Close','Volume','WAP','Count')
   symbol<-strsplit(tmpfile,'-')[[1]][1]
   assign(paste(symbol,'_xts',sep=''), tmpxts, envir=.GlobalEnv)
   #chartSeries(get(paste(symbol,'_xts',sep=''))[,1:4])
   print(symbol);print(periodicity(tmpxts))
}

#munmap(m) #free up resources
