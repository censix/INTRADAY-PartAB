
# x>=0 is not needed but 0<mt<1
IBroundToMinTick <- function(x,mt) {
  #if (x<=0) stop('x value needs to be > 0')
  if (mt>1 | mt<=0) stop('mintick value needs to be in (0,1]')
  if (mt==1) return(round(x))
  # res <- x-(x %% mt)
  res <- x-(sign(x)*(abs(x) %% mt))
  if (abs(res)<mt) stop('The rounding to minTick returned 0 or another unacceptable value!!!')
  return(res)
}
