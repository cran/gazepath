simplify <-
function(classification, x, y, Hz, D, width_px, width_mm, extra, extra_var){
  class <- rle(classification)
  simple <- data.frame(class$values, class$lengths, 
                       c(1, cumsum(class$lengths) + 1)[-(length(class$values) + 1)], 
                       cumsum(class$lengths))
    if(length(which(class$values == 'f')) > 0){
      x_start <- y_start <- x_end <- y_end <- mean_x <- mean_y <- POGvar <- RMS <- numeric()
      for(i in 1:dim(simple)[1]){
        x_start <- c(x_start, x[simple[i, 3]])
        y_start <- c(y_start, y[simple[i, 3]])
        x_end <- c(x_end, x[simple[i, 4]])
        y_end <- c(y_end, y[simple[i, 4]])
        mean_x <- c(mean_x, mean(x[simple[i,3] : simple[i,4]]))
        mean_y <- c(mean_y, mean(y[simple[i,3] : simple[i,4]]))
        m <- as.matrix(dist(cbind(c(mean_x[length(mean_x)], x[simple[i,3] : simple[i,4]]), c(mean_y[length(mean_y)], y[simple[i,3] : simple[i,4]]))))
        RMS <- c(RMS, sqrt(mean((atan((diag(m[-1,-c(1,2)]) / 2) / mean(D, na.rm = T)) * (180 / pi) * (width_mm / width_px) * 2)**2)))
        POGvar <- c(POGvar, mean(m[-1,1]))
      }
      ## Calculate saccade amplitude and transform POGvar from pixels to degrees of visual angle and to sd
      ss <- which(class$values == 's')
      POGvar[ss] <- sqrt((x_start[ss] - x_end[ss]) ^ 2 + (y_start[ss] - y_end[ss]) ^ 2)
      POGsdSacAmp <- atan((POGvar / 2) / mean(D, na.rm = T)) * (180 / pi) * (width_mm / width_px) * 2
      POGsdSacAmp[!ss] <- sqrt(POGsdSacAmp)
      RMS[ss] <- NA
      
      simple <- data.frame(class$values, class$lengths * (1000/Hz), 
                           c(1, cumsum(class$lengths * (1000/Hz)) + 1)[-(length(class$values) + 1)], 
                           cumsum(class$lengths * (1000/Hz)), x_start, y_start, x_end, y_end, mean_x, mean_y, POGsdSacAmp, RMS)
      names(simple)[1:4] <- c('Value', 'Dur', 'Start', 'End')
      if(!is.null(extra_var)){
        for(i in 1:length(extra_var)){
          simple <- data.frame(simple, extra[i])
          names(simple)[dim(simple)[2]] <- extra_var[i]
        }
      }
    }
  # Remove NA values
  if(length(which(is.na(simple[,1]))) != 0){
    simple <- simple[-which(is.na(simple[,1])),]
  }
  return(simple)
}
