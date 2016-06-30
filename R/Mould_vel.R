Mould_vel <-
function(speed, Hz, plot = F){
  lmax <- speed[lomax(speed)]
  if(length(lmax) < 10){
    return(NA)
    warning('There are no enough data points to estimate a velocity threshold')
  } else {
    thresholds <- seq(min(lmax), max(lmax), length.out = Hz)
    set <- sapply(thresholds, function(x) {length(which(lmax > x))})
    uni <- seq(length(lmax), 0, length.out = Hz)
    gap <- uni - set
    
    if(Hz < 250) {h <- .1} else {h <- .05}
    
    gap <- predict(loess( gap ~ log(thresholds) ,span = h))
    while (length(lomax(gap)) > 1 & h < 1) {
      h <- h + .01
      gap <- predict(loess( (uni - set) ~ log(thresholds) ,span = h,  surface = 'direct', cell = 1))
    }
    
    if(plot == T) plotMould(uni, set, gap, thresholds, lmax, Hz)
    if(h != 1) return(thresholds[which.max(gap)]) else return(NA)
  }
}
