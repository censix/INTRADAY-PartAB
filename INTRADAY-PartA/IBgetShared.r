#################
# reads the file and converts the data to xts. Does not do any checks!!
#################
library(mmap)
library(xts)

#allshares<-dir(IBdatafeedShareDir, pattern=glob2rx('*.bin'))

IBgetShared <- function(filename) {
  #'SYMBOL-EXCHANGE-mktdata.bin'
  if (!file.exists(filename)) return(NULL)
  extract2XTS<- function(x) na.trim( .xts( as.matrix(cbind(x$open,x$high, x$low, x$close, x$volume, x$wap, x$count)), x$timestamp), sides='right', is.na='all')
  m <- mmap(filename, struct(timestamp=double(),open=double(),high=double(),low=double(),close=double(),volume=double(),wap=double(),count=double() ), extractFUN=extract2XTS)
  #print(last(m[]))  #Gets us the last non-NA entry!!
  tmpxts<-m[]
  colnames(tmpxts)<-c('Open','High','Low','Close','Volume','WAP','Count')
  #symbol<-strsplit(tmpfile,'-')[[1]][1]
  munmap(m) #free up resources
  if (!is.xts(tmpxts)) return(NULL)
  if (nrow(tmpxts)<1) return(NULL)
  return(tmpxts)
}
