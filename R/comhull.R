## Combine succesive fixations based on region, overlapping fixations are combined
comhull <- function(d, classification, dat_x, dat_y, in_thres, Hz = Hz){
  d <- d[d$dur > 1,]
  fix <- tail(which(d$index == 'fixation'), 1)
  count <- length(which(d$index == 'fixation')) - 1
  while(count >= 1){
    fix2 <- which(d$index == 'fixation')[count]
    cvhull <- chull(cbind(dat_x, dat_y)[d[fix,3] : d[fix,4],])
    POLY_FIX <- cbind(dat_x[d[fix,3] : d[fix,4]][cvhull], dat_y[d[fix,3] : d[fix,4]][cvhull])
    PNT <- sum(pnt.in.poly(cbind(dat_x, dat_y)[d[fix2,3] : d[fix2,4],], POLY_FIX)[,3])
    ## If fixations have the same location and are within interpolation limit combine them...
    if(PNT > 0 & (d[fix,3] - d[fix2,4]) < in_thres * (Hz / 1000)){
      classification[d[fix2,3] : d[fix,4]] <- 'fixation'
      CL <- rle(classification)
      index <- rep.int(1:length(CL$value), CL$lengths)
      POG <- sapply(unique(index[!is.na(index)]), function(i) mean(dist(cbind(dat_x[index == i], dat_y[index == i])), na.rm = T))
      POG[is.na(POG)] <- 0
      mean_x <- as.vector(by(dat_x, index, function(i) mean(i, na.rm = T)))
      mean_y <- as.vector(by(dat_y, index, function(i) mean(i, na.rm = T)))
      
      dat_x[d[fix2,3] : d[fix,4]] <- na.approx(dat_x[d[fix2,3] : d[fix,4]])
      dat_y[d[fix2,3] : d[fix,4]] <- na.approx(dat_y[d[fix2,3] : d[fix,4]])
      
      d <- data.frame(CL$value, CL$length, c(1, cumsum(CL$length)[-length(CL$length)] + 1), cumsum(CL$length), POG, mean_x, mean_y)
      names(d)[1:4] <- c('index', 'dur', 'start', 'end')
      d <- d[d$dur > 1,]
    } 
    fix <- fix2
    count <- count - 1
  }
  return(list(classification, dat_x, dat_y))
}




