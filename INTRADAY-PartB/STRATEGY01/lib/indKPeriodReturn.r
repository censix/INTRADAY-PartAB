
# Calculate k-period returns.
# either a 'sliding' (default) or a 'fixed position' indicator, depending on mktdataidx value

indKPeriodReturn <- function(x,columns=c('Close'), labels=c('ret'), lagk=1, mktdataidx=NULL) {
  tmp<- log(x[,columns]) - log(lag(x[,columns] , lagk))  #log return over k periods
  #
  if (!is.null(mktdataidx)) { #fixed pos. indicator
     ignoreidx <- (mktdataidx - nrow(x) + 1:nrow(x)) %% lagk != 0
     tmp[ignoreidx,] <- NA
     tmp <- na.locf(tmp)
  }
  #
  tmp<- cbind(x,tmp)[,-(1:length(colnames(x)))]
  colnames(tmp)<-labels
  return(tmp)
}





# not done properly

#indKPeriodReturn <- function(x,columns=c('Close'), labels=c('ret'), lagk=1, recalcshift=0) {
#  # recalcshift>0  we use to calculate a 'fixed position' indicator that only gets updated every m bars
#  # recalcshift==0 we have a 'sliding' indicator that is updated for every bar of x
#  #
#  tmp<- log(x[,columns]) - log(lag(x[,columns] , lagk))  #log return over k periods
#  if (recalcshift>0) { 
#      tmp[(nrow(tmp)-recalcshift+1):nrow(tmp), ] <- NA  #ignore last 'recalcshift' rows
#      tmp <- na.locf(tmp)
#  }  
#  tmp<- cbind(x,tmp)[,-(1:length(colnames(x)))]
#  colnames(tmp)<-labels
#  return(tmp)
#}


#alternative using rollapply ??? not working, too messy ...

#indKPeriodReturn <- function(x,columns=c('Close'), labels=c('ret'), lagk=1, recalcshift=0) {
#  # recalcshift>0  we use to calculate a 'fixed position' indicator that only gets updated every m bars
#  # recalcshift==0 we have a 'sliding' indicator that is updated for every bar of x
#  #

#   tmp<- rollapplyr( data=zoo(x[,columns]), width=lagk+1, fill=NA, 
#            by.column=FALSE,  #the by.column=FALSE is needed when FUN returns more than 1 value
#            by=recalcshift+1,
#            FUN=function(z) {
#            
#                  tmp<- log(z[lagk+1]) - log(z[1])  #log return over k periods
#                  
#                  return( as.numeric(tmp)  )
#                }
#      )
#  tmp<-na.locf(tmp)
#  tmp<- cbind(x,as.xts(tmp))[,-(1:length(colnames(x)))]
#  colnames(tmp)<-labels
#  return(tmp)
#}


