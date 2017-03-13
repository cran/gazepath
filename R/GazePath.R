gazepath <-
function(data, x1, y1, x2 = NULL, y2 = NULL, d1, d2 = NULL, trial, height_px, height_mm, width_px, width_mm, extra_var = NULL, res_x = 1280, res_y = 1024, samplerate = 500, method = 'Mould', posthoc = FALSE, thres_vel = 35, thres_dur = 100, min_dist = 250, in_thres = 150){
  ## Check if input is a data frame
  if(!is.data.frame(data)) {
    stop('please insert a data frame and define the column numbers of the variables')
  }
  ## identify unique trials
  if(length(unique(data[,trial])) != length(rle(as.numeric(data[,trial]))$lengths)){
    TRIAL_NEW <- rep.int(1:length(rle(as.numeric(data[,trial]))$lengths), rle(as.numeric(data[,trial]))$lengths)
    data <- data.frame(data, TRIAL_NEW)
    if(is.numeric(trial)){
      names(data)[trial] <- 'TRIAL_OLD'
      trial <- 'trial'
    } else {
      names(data)[which(names(data) == trial)] <- 'TRIAL_OLD'
    }
    names(data)[ncol(data)] <- trial
    warning('The trial index in the data frame was not unique, therefore trials are renamed to be unique and the old trial indx is stored in the data as TRIAL_OLD')
  }
  ## find extra variables
  extra <- list()
  
  if(!is.null(extra_var)){
    if(sum(extra_var %in% names(data)) == length(extra_var)){
      for(i in unique(data[,trial])){
        extra[[i]] <- sapply(1:length(extra_var), function(j) head(as.character(data[data[,trial] == i, which(names(data) == extra_var[j])]), 1))
      }
    } else {
      extra_var <- NULL
      print('Please make sure the variables to pass through have the correct names')
    }
  }

  ## Check distance
  data[,d1] <- ifelse(data[,d1] < min_dist, NA, data[,d1])
  if(is.null(d2)){
    D <- by(data[,d1], data[,trial], data.frame)
  } else {
    data[,d2] <- ifelse(data[,d2] < min_dist, NA, data[,d2])
    D <- by((data[,d1] + data[,d2]) / 2, data[,trial], data.frame)
  }
  
  ## Check input 1 or 2 eyes
  if(!is.null(x2) & !is.null(y2)){
    data[,x1] <- ifelse(is.na(data[,x1]), data[,x2], data[,x1])
    data[,y1] <- ifelse(is.na(data[,y1]), data[,y2], data[,y1])
    data[,x2] <- ifelse(is.na(data[,x2]), data[,x1], data[,x2])
    data[,y2] <- ifelse(is.na(data[,y2]), data[,y1], data[,y2])
    X <- by((data[,x1] + data[,x2]) / 2, data[,trial], data.frame)
    Y <- by((data[,y1] + data[,y2]) / 2, data[,trial], data.frame)
  } else {
    X <- by(data[,x1], data[,trial], data.frame)
    Y <- by(data[,y1], data[,trial], data.frame)
  }
  
  ## Determine robustness and precision
  Rob_x <- sapply(1:length(X), function(i) robust(X[[i]], samplerate))
  Rob_y <- sapply(1:length(X), function(i) robust(Y[[i]], samplerate))
  Robustness <- (Rob_x + Rob_y) / 2
  Pre_x <- sapply(1:length(X), function(i) precision(X[[i]], samplerate))
  Pre_y <- sapply(1:length(X), function(i) precision(Y[[i]], samplerate))
  Precision <- (Pre_x + Pre_y) / 2
  
  ## Convert single heigths and widths into vectors
  if(length(height_px) == 1) height_px <- rep(height_px, length(unique(data[,trial])))
  if(length(height_mm) == 1) height_mm <- rep(height_mm, length(unique(data[,trial])))
  if(length(width_px) == 1) width_px <- rep(width_px, length(unique(data[,trial])))
  if(length(width_mm) == 1) width_mm <- rep(width_mm, length(unique(data[,trial])))
  
  final <- 'Please insert a correct method'
  s <- NA
  
  if(method == 'velocity'){
    final <- list()
    for(i in 1:length(unique(data[,trial]))){
      ## Boundary check
      X[[i]] <- Boundary(X[[i]], (res_x - width_px[i]) / 2, res_x - (res_x - width_px[i]) / 2)
      Y[[i]] <- Boundary(Y[[i]], (res_y - height_px[i]) / 2, res_y - (res_y - height_px[i]) / 2)
      final[[i]] <- Eyelink(X[[i]], Y[[i]], D[[i]], height_mm[i], width_mm[i], height_px[i], width_px[i], Hz = samplerate)
    } 
  }
  
  if(method == 'dispersion'){
    final <- list()
    for(i in 1:length(unique(data[,trial]))){
      ## Boundary check
      X[[i]] <- Boundary(X[[i]], (res_x - width_px[i]) / 2, res_x - (res_x - width_px[i]) / 2)
      Y[[i]] <- Boundary(Y[[i]], (res_y - height_px[i]) / 2, res_y - (res_y - height_px[i]) / 2)
      final[[i]] <- Tobii(cbind(X[[i]], Y[[i]]), D[[i]], thres_dur = 100, Hz = samplerate)
    } 
  }
  
  if(method == 'Mould'){
    fix <- thres_vel <- numeric()
    s <- list()
    for(i in 1:length(unique(data[,trial]))){
      ## Boundary check
      X[[i]] <- Boundary(X[[i]], (res_x - width_px[i]) / 2, res_x - (res_x - width_px[i]) / 2)
      Y[[i]] <- Boundary(Y[[i]], (res_y - height_px[i]) / 2, res_y - (res_y - height_px[i]) / 2)
      ## make sure there is at least data avaible
      samp <- ifelse(samplerate < 150, samplerate, 150)
      if(length(which(!is.na(X[[i]]))) > samp & length(which(!is.na(Y[[i]]))) > samp & length(which(!is.na(D[[i]]))) > samp){
        ## Calculate speed
        s[[i]] <- Speed_Deg(X[[i]], Y[[i]], D[[i]], height_mm[i], width_mm[i], height_px[i], width_px[i], samplerate)
        ## Omit velocities over 1000 deg/s
        s[[i]] <- ifelse(s[[i]] > 1000, NA, s[[i]])
        ## Check for enough local maxima
        if(length(lomax(s[[i]])) > 10){
          thres_vel[i] <- Mould_vel(s[[i]], Hz = samplerate)
          fix <- c(fix, possiblefix(s[[i]], thres_vel[i]))
        } else{
          s[[i]] <- NA; thres_vel[i] <- NA
        }
      } else {
        s[[i]] <- NA; thres_vel[i] <- NA
      }
    }
    ## Make sure there are enough data points to make the fit
    fix <- fix[!is.na(fix)]
    if(length(fix) > 100){
      thres_dur <- Mould_dur(fix, Hz = samplerate)
    } else {
      warning('There were not enough data points to derive a duration threshold, the threshold specified is used (default = 100ms)')
    }
    ## Make the final output with the classification of all samples
    final <- list()
    for(i in 1:length(unique(data[,trial]))){
      if(!is.na(thres_vel[i])){
        final[[i]] <- fixationANDsaccade(s[[i]], thres_vel[i], thres_dur, Hz = samplerate)
      } else {
        final[[i]] <- NA
      }
    } 
  }
  
  if(method == 'Mould.all'){
    s <- list()
    for(i in 1:length(unique(data[,trial]))){
      ## Boundary check
      X[[i]] <- Boundary(X[[i]], (res_x - width_px[i]) / 2, res_x - (res_x - width_px[i]) / 2)
      Y[[i]] <- Boundary(Y[[i]], (res_y - height_px[i]) / 2, res_y - (res_y - height_px[i]) / 2)
      ## Calculate speed
      s[[i]] <- Speed_Deg(X[[i]], Y[[i]], D[[i]], height_mm[i], width_mm[i], height_px[i], width_px[i], samplerate)
      ## Omit velocities over 1000 deg/s
      s[[i]] <- ifelse(s[[i]] > 1000, NA, s[[i]])
    }
    
    thres_vel <- Mould_vel(unlist(s), plot = F, Hz = samplerate)
    fix <- possiblefix(unlist(s), thres_vel)
    fix <- fix[!is.na(fix)]
    
    thres_dur <- Mould_dur(fix, plot = F, Hz = samplerate, default = thres_dur)
    
    final <- list()
    for(i in 1:length(unique(data[,trial]))){
      final[[i]] <- fixationANDsaccade(s[[i]], thres_vel, thres_dur, Hz = samplerate)
    }
  }
  
  if(method == 'Mould.allDur'){
    s <- list()
    for(i in 1:length(unique(data[,trial]))){
      ## Boundary check
      X[[i]] <- Boundary(X[[i]], (res_x - width_px[i]) / 2, res_x - (res_x - width_px[i]) / 2)
      Y[[i]] <- Boundary(Y[[i]], (res_y - height_px[i]) / 2, res_y - (res_y - height_px[i]) / 2)
      ## Calculate speed
      s[[i]] <- Speed_Deg(X[[i]], Y[[i]], D[[i]], height_mm[i], width_mm[i], height_px[i], width_px[i], samplerate)
      ## Omit velocities over 1000 deg/s
      s[[i]] <- ifelse(s[[i]] > 1000, NA, s[[i]])
    }
    
    thres_vel <- Mould_vel(unlist(s), plot = F, Hz = samplerate)
    fix <- possiblefix(unlist(s), thres_vel)
    fix <- fix[!is.na(fix)]

    final <- list()
    for(i in 1:length(unique(data[,trial]))){
      final[[i]] <- fixationANDsaccade(s[[i]], thres_vel, thres_dur, Hz = samplerate)
    }
  }
  
  if(method == 'MouldDur'){
    fix <- thres_vel <- numeric()
    s <- list()
    for(i in 1:length(unique(data[,trial]))){
      ## Boundary check
      X[[i]] <- Boundary(X[[i]], (res_x - width_px[i]) / 2, res_x - (res_x - width_px[i]) / 2)
      Y[[i]] <- Boundary(Y[[i]], (res_y - height_px[i]) / 2, res_y - (res_y - height_px[i]) / 2)
      ## make sure there is at least 1 second of data avaible
      if(length(which(!is.na(X[[i]]))) > samplerate & length(which(!is.na(Y[[i]]))) > samplerate & length(which(!is.na(D[[i]]))) > samplerate){
        ## Calculate speed
        s[[i]] <- Speed_Deg(X[[i]], Y[[i]], D[[i]], height_mm[i], width_mm[i], height_px[i], width_px[i], samplerate)
        ## Omit velocities over 1000 deg/s
        s[[i]] <- ifelse(s[[i]] > 1000, NA, s[[i]])
        ## Check for enough local maxima
        if(length(lomax(s[[i]])) > 10){
          thres_vel[i] <- Mould_vel(s[[i]], Hz = samplerate)
          fix <- c(fix, possiblefix(s[[i]], thres_vel[i]))
        } else{
          s[[i]] <- NA; thres_vel[i] <- NA
        }
      } else {
        s[[i]] <- NA; thres_vel[i] <- NA
      }
    }
    ## Set fixed duration threshold
    thres_dur <- thres_dur
    ## Make the final output with the classification of all samples
    final <- list()
    for(i in 1:length(unique(data[,trial]))){
      if(!is.na(thres_vel[i])){
        final[[i]] <- fixationANDsaccade(s[[i]], thres_vel[i], thres_dur, Hz = samplerate)
      } else {
        final[[i]] <- NA
      }
    } 
  }
  
  if(method == 'gazepath'){
    fix <- thres_vel <- numeric()
    final <- s <- list()
    for(i in 1:length(unique(data[,trial]))){
      ## Boundary check
      X[[i]] <- Boundary(X[[i]], (res_x - width_px[i]) / 2, res_x - (res_x - width_px[i]) / 2)
      Y[[i]] <- Boundary(Y[[i]], (res_y - height_px[i]) / 2, res_y - (res_y - height_px[i]) / 2)
      ## make sure there is at least 1 second of data available
      if(length(which(!is.na(X[[i]]))) > samplerate & length(which(!is.na(Y[[i]]))) > samplerate & length(which(!is.na(D[[i]]))) > samplerate){
        ## Use the interpolation function
        interpol <- Interpolate(X[[i]], Y[[i]], D[[i]], height_mm[i], width_mm[i], height_px[i], width_px[i], res_x = res_x, res_y = res_y, Hz = samplerate, in_thres = in_thres, thres_dur = thres_dur)
        if(interpol[[8]] == 'Return'){
          final[[i]] <- ifelse(interpol[[7]] == 'missing', NA, ifelse(interpol[[7]] == 'fixation', 'f', 's'))
          thres_vel[i] <- interpol[[5]]
          s[[i]] <- interpol[[6]]
          X[[i]] <- interpol[[1]]
          Y[[i]] <- interpol[[2]]
        } else {
          s[[i]] <- NA; thres_vel[i] <- NA; final[[i]] <- NA
        }
      } else {
        s[[i]] <- NA; thres_vel[i] <- NA; final[[i]] <- NA
      }
    }
  }
  
  ## Post-hoc Check
  if(posthoc == TRUE){
    for(i in 1:length(X)) {
      PH <- posthocCheck(final[[i]], X[[i]], Y[[i]])
      final[[i]] <- PH[[1]]
      X[[i]] <- PH[[2]]
      Y[[i]] <- PH[[3]]
    }
  }
  ## Simplify the raw classification
  sim <- list()
  for(i in 1:length(X)){
    sim[[i]] <- simplify(final[[i]], X[[i]], Y[[i]], samplerate, D[[i]], width_px[i], width_mm[i], extra[[i]], extra_var)
  }
  
  output <- list(final, X, Y, method, Robustness, Precision, thres_vel, thres_dur, s, samplerate, D, height_px, height_mm, width_px, width_mm, sim)
  names(output) <- c('classifications', 'x-coor', 'y-coor', 'method', 'robustness',
                     'precision', 'vel_thres', 'dur_thres', 'speed', 'samplerate',
                     'distance', 'height_px', 'height_mm', 'width_px', 'width_mm',
                     'fixations')
  class(output) <- 'gazepath'
  return(output)
}
