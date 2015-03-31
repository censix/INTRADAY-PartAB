
# Fit an AR(1) model to all data in x , if indicated by (mktdataidx %% refitevery==0), then
# apply the fitted model to x using rolling windows of width N

# Example use:
# mktdata <- cbind(mktdata, indAR1(x=mktdata, N=?10minutes??, refitnow = (mktdataidx %% ??30minutes?? ==0) ))

#NOTE: with groupby= and mktdataidx= we can work with a data column that has a different frequency than the raw x, i.e. 'ret1min'.
#      This only makes sense if the 'ret1min' input column(s) was calculated in the same way with regard to groupby.
#      !!!! Here groupby= and mktdataidx= can only be used *together* !!!

indAR1 <- function(x,columns=c('ret'), N, labels=c('ar1coef','ahead1','ahead2','ahead5','se1','se2','se5'), storagevar='MODELar1', refitnow=FALSE, groupby=1, mktdataidx=NULL ) {

  if (groupby>1 && !is.null(mktdataidx)) { #fixed pos. indicator with grouping
     posidx <- (mktdataidx - nrow(x) + 1:nrow(x)) %% groupby == 0
     xdata <- x[posidx,columns]
  } else {
     xdata <- x[,columns]
  }
   # See if we need to re-fit the model
   tt.fit <- getit(storagevar)
   if ( refitnow || is.null(tt.fit) ) {
      tt.x <- na.omit(na.locf(xdata))
      if (nrow(tt.x)<1) stop('not enough non-NA values to fit the model')
      tt.fit <- ar(x=tt.x, aic = FALSE, order.max=1) # fit AR order=1 model
      setit(storagevar,tt.fit)
   }
   # Apply a model to the input column to get a prediction, stderr, ....
   tmp<- rollapplyr( data=zoo(xdata), width=N, fill=NA,
            by.column=FALSE,  #the by.column=FALSE is needed when FUN returns more than 1 value
            FUN=function(z,currentmodel) {

                  res   <-  predict(currentmodel, newdata=z, n.ahead=5)  # apply model to new data

                  return(c( tt.fit$ar, res$pred[c(1,2,5)], res$se[c(1,2,5)]   ))
                },
            currentmodel=tt.fit  #argument to FUN
      )

   tmp<- cbind(x,as.xts(tmp))[,-(1:length(colnames(x)))]
   tmp <- na.locf(tmp, na.rm=FALSE) #!! Expand back to raw barsize again. forward fill NA's, but leave leading NA's untouched.   
   colnames(tmp)<-labels
   return(tmp)
}



