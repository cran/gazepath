summary.gazepath <-
function(object, ..., complete_only = FALSE, fixations_only = FALSE){
  output <- numeric()
  end <- dim(object[[16]][[1]])[2]
  for(i in 1:length(object[[16]])){
    if(end == 4) end <- dim(object[[16]][[i]])[2]
    sim <- object[[16]][[i]]
    l <- length(which(sim[,1] == 'f'))
    if(l != 0){
      if(complete_only == TRUE){
        if(fixations_only == TRUE){
          index <- complete(sim, 'f')
          if(length(index) != 0){
            output <- rbind(output, cbind(sim[index, c(1:4, 9:end)], 1:length(index), i))
          }
        } else {
          if(length(which(sim[,1] == 's')) != 0){
            index <- sort(c(complete(sim, 'f'), complete(sim, 's')))
            if(length(index) != 0){
              output <- rbind(output, cbind(sim[index, c(1:4, 9:end)], 1:length(index), i))
            }
          } 
        }
      } else {
        if(fixations_only == TRUE){
          output <- rbind(output, cbind(sim[sim[,1] == 'f', c(1:4, 9:end)], 1:l, i))
        } else {
          l <- sum(sim[,1] == 'f' | sim[,1] == 's')
          output <- rbind(output, cbind(sim[sim[,1] == 'f' | sim[,1] == 's',c(1:4, 9:end)], 1:l, i))
        }
      }
    }
  }
  if(length(output) == 0){
    print('There were no fixations or saccades classified, probably data quality of this particpant is very low')
  } else {
    names(output)[c(1:4,(end - 3):(end - 2))] <- c('Value', 'Duration', 'Start', 'End', 'Order', 'Trial')
    row.names(output) <- 1:dim(output)[1]
    return(output)
  }
}
