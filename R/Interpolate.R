## Interpolation function
Interpolate <- function(X, Y, D, height_mm, width_mm, height_px, width_px, res_x = res_x, res_y = res_y, Hz = Hz, in_thres = in_thres, thres_dur = thres_dur){
  
  s <- Speed(X, Y, D, height_mm, width_mm, height_px, width_px, res_x = 1280, res_y = 1024, Hz)
  s <- ifelse(s > 1000, NA, s)
  if(length(lomax(s)) < 10){
    return(list('No Return', 'No Return','No Return','No Return','No Return','No Return','No Return','No Return'))
  } else {
    M <- Mould_vel(s, Hz)
    
    classification <- ifelse(s > M, 'saccade', 'fixation')
    classification[is.na(classification)] <- 'missing'
    
    CL <- rle(classification)
    d <- data.frame(CL$value, CL$length, c(1, cumsum(CL$length)[-length(CL$length)] + 1), cumsum(CL$length))
    names(d) <- c('index', 'dur', 'start', 'end')
    
    dat_x <- X
    dat_y <- Y
    dat_d <- D
    
    for(i in which(d$index == 'missing')){
      if(i > 1 & i < dim(d)[1] & d[i, 2] < (in_thres * (Hz / 1000))){
        if(d[i + 1, 1] == 'fixation' & d[i - 1, 1] == 'fixation'){
          ii_s <- d[i - 1, 4]
          ii_e <- d[i + 1, 3]
          speed <- Speed(c(dat_x[ii_s], dat_x[ii_s], dat_x[ii_e]), c(dat_y[ii_s], dat_y[ii_s], dat_y[ii_e]), c(dat_d[ii_s], dat_d[ii_s], dat_d[ii_e]), height_mm, width_mm, height_px, width_px, res_x = res_x, res_y = res_y, Hz)
          if(speed[2] < M){
            dat_x[d[i, 3] : d[i, 4]] <- dat_x[ii_s]
            dat_y[d[i, 3] : d[i, 4]] <- dat_y[ii_s]
            dat_d[d[i, 3] : d[i, 4]] <- dat_d[ii_s]
          }
        }
      }
    }
    
    s <- Speed(dat_x, dat_y, dat_d, height_mm, width_mm, height_px, width_px, res_x = res_x, res_y = res_y, Hz)
    s <- ifelse(s > 1000, NA, s)
    
    classification <- ifelse(s > M, 'saccade', 'fixation')
    classification[is.na(classification)] <- 'missing'
    CL <- rle(classification)
    
    index <- rep.int(1:length(CL$value), CL$lengths)
    POG <- sapply(unique(index[!is.na(index)]), function(i) mean(dist(cbind(dat_x[index == i], dat_y[index == i])), na.rm = T))
    POG[is.na(POG)] <- 0
    mean_x <- as.vector(by(dat_x, index, function(i) mean(i, na.rm = T)))
    mean_y <- as.vector(by(dat_y, index, function(i) mean(i, na.rm = T)))
    
    d <- data.frame(CL$value, CL$length, c(1, cumsum(CL$length)[-length(CL$length)] + 1), cumsum(CL$length), POG, mean_x, mean_y)
    names(d)[1:4] <- c('index', 'dur', 'start', 'end')
    
    ## Combine fixations 
    dimd_new <- dim(d)[1] + 1
    while(dimd_new != dim(d)[1]){
      dimd_new <- dim(d)[1]
      ## Combine fixations
      classif <- comhull(d, classification, dat_x, dat_y, in_thres, Hz)
      
      CL <- rle(classif[[1]])
      classification <- classif[[1]]
      index <- rep.int(1:length(CL$value), CL$lengths)
      dat_x <- classif[[2]]
      dat_y <- classif[[3]]
      POG <- sapply(unique(index[!is.na(index)]), function(i) mean(dist(cbind(dat_x[index == i], dat_y[index == i])), na.rm = T))
      POG[is.na(POG)] <- 0
      mean_x <- as.vector(by(dat_x, index, function(i) mean(i, na.rm = T)))
      mean_y <- as.vector(by(dat_y, index, function(i) mean(i, na.rm = T)))
      
      d <- data.frame(CL$value, CL$length, c(1, cumsum(CL$length)[-length(CL$length)] + 1), cumsum(CL$length), POG, mean_x, mean_y)
      names(d)[1:4] <- c('index', 'dur', 'start', 'end')
    }
    
    # Remove short fixations and check for combinations again
    for(i in which(CL$value == 'fixation' & CL$length < (Hz / 1000 * thres_dur))){
      classification[((cumsum(CL$length) - CL$length) + 1)[i] : cumsum(CL$length)[i]] <- 'saccade' 
    }
    
    CL <- rle(classification)
    index <- rep.int(1:length(CL$value), CL$lengths)
    POG <- sapply(unique(index[!is.na(index)]), function(i) mean(dist(cbind(dat_x[index == i], dat_y[index == i])), na.rm = T))
    POG[is.na(POG)] <- 0
    mean_x <- as.vector(by(dat_x, index, function(i) mean(i, na.rm = T)))
    mean_y <- as.vector(by(dat_y, index, function(i) mean(i, na.rm = T)))
    
    d <- data.frame(CL$value, CL$length, c(1, cumsum(CL$length)[-length(CL$length)] + 1), cumsum(CL$length), POG, mean_x, mean_y)
    names(d)[1:4] <- c('index', 'dur', 'start', 'end')
    
    classification <- comhull(d, classification, dat_x, dat_y, in_thres, Hz)
    
    clas <- classification[[1]]
    CL <- rle(clas)
    dat_x <- classification[[2]]
    dat_y <- classification[[3]]
    
    index <- rep.int(1:length(CL$value), CL$lengths)
    mean_x <- as.vector(by(dat_x, index, function(i) mean(i, na.rm = T)))
    mean_y <- as.vector(by(dat_y, index, function(i) mean(i, na.rm = T)))
    
    index <- CL$value
    end <- cumsum(CL$length) * (1000 / Hz)
    dur <- CL$length * (1000 / Hz)
    start <- (end - dur) + 1
    
    d <- data.frame(index, dur, start, end, mean_x, mean_y)
    d <- data.frame(d, order=1:dim(d)[1])
    
    return(list(dat_x, dat_y, dat_d, d, M, s, clas, 'Return'))
  }
}