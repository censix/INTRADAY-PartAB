
IBappendXtsToHistory <- function(newSeriesxts, rdatafile, currentSymbol) {
  #Creates new history file or appends to an existing one. Also does a
  #rudimentary check of the periodicities of historic and new data
  
  #newSeriesxts <- givenXTSobject
  #rdatafile <- './somepath/SYMBOL.rdata'
  #currentSymbol <- 'SYMBOL'
  tmpENV <- new.env()
  currentSeriesxts <- NULL
  saveit <- FALSE
  currentSymbolxts <- paste(currentSymbol,'_xts',sep='')
  if (file.exists(rdatafile)) {
    load(file=rdatafile, envir=tmpENV)
    #currentSymbolxts
    ##assign('currentSeries', get(currentSymbol))
    assign('currentSeriesxts', get(currentSymbolxts,envir=tmpENV))
    lastdate <- end(currentSeriesxts)
    #check if we have at least two rows, otherwise ignore (periodicity calc. will fail with less than two rows)
    if (nrow(newSeriesxts)<=2) {
      print(paste('Symbol',currentSymbol,'had less than 2 rows of data. Ignoring data')) 
      rm(tmpENV)
      if (ibNotificationEmail & exists('IBemailMsg',mode='function')) IBemailMsg(msgBODY=paste('WARNING: DATAFEED!','Symbol',currentSymbol,'had less than 2 rows of data. Ignoring data'), msgHEAD='WARNING: DATAFEED!')
      return(TRUE)
    } #ignore and return without appening anything
    #check periodicity before attaching. If wrong scale, abort
    p1<-periodicity(newSeriesxts) #determine the real periodicty
    p2<-periodicity(currentSeriesxts) #determine the real periodicty
    #print(names(p))
    #frequency gives us the avg number of secale units between obs.
    #if (p1$frequency != p2$frequency) stop(paste('periodicities',p1$frequency,p2$frequency,'dont match')) 
    #if ((p1$scale != p2$scale) | (p1$frequency != p2$frequency)) stop(paste('IBappendXtsToHistory: ',currentSymbol,'periodicities (',p1$scale,p1$frequency,')::(',p2$scale,p2$frequency,') dont match'))
    if ((p1$scale != p2$scale) | (p1$frequency != p2$frequency)) {
      print(paste('IBappendXtsToHistory: ',currentSymbol,'periodicities (',p1$scale,p1$frequency,')::(',p2$scale,p2$frequency,') dont match. Ignoring data'))
      rm(tmpENV)
      if (ibNotificationEmail & exists('IBemailMsg',mode='function')) IBemailMsg(msgBODY=paste('WARNING: DATAFEED!','Symbol',currentSymbol,'Periodicities dont match. Ignoring data'), msgHEAD='WARNING: DATAFEED!')
      return(TRUE)
    }  
    if (end(newSeriesxts)>end(currentSeriesxts)) { 
      #new data
      if (start(newSeriesxts)>end(currentSeriesxts)) { 
        #gap
        currentSeriesxts <- rbind(currentSeriesxts, newSeriesxts)
      } else { 
        #touch or overlap
        ovl1<-newSeriesxts[paste("::",end(currentSeriesxts), sep="")]
        nr.ovl1 <- nrow(ovl1)
        ovl2<-currentSeriesxts[paste(start(newSeriesxts),"::", sep="")]
        nr.ovl2 <- nrow(ovl2)
        #check if overlaps have same sizes and values, if yes
        if ((nr.ovl1 == nr.ovl2) & all(coredata(ovl1) == coredata(ovl2) ,na.rm=TRUE)) {
          currentSeriesxts <- rbind(currentSeriesxts, newSeriesxts[(nr.ovl1+1):nrow(newSeriesxts),])  
          print('append ok')  
        } else stop(paste('IBappendXtsToHistory: ',currentSymbol,' overlap error - trying to append two series of different frequencies or non-matching values in overlap'))
      }
      saveit<-TRUE
    } else {
      print("nothing new")
    }            
  } else {
      print('creating new history')
      currentSeriesxts <- newSeriesxts
      saveit<-TRUE
  }
  if (saveit) {
    assign(currentSymbolxts, currentSeriesxts, envir=tmpENV)
    save(list=c(currentSymbolxts), file=rdatafile, envir=tmpENV)
  }
  rm(tmpENV) #clean
}


