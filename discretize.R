discretize <- function(vec){
  vec <- unlist(vec)
  tot <- sum(vec)
  tot.int <- round(tot)
  # scale vector to new tot.int
  vec <- tot.int/tot*vec
  len <- length(vec)
  
  new.vec <- round(vec)
  
  diff.vec <- numeric(len)
  
  for (i in 1:len) {
    diff.vec[i] <- new.vec[i] - vec[i]
  }
  
  if (round(sum(new.vec)-tot.int)>0) {
    over <- round(sum(new.vec))-tot.int
    descending.diff <- order(diff.vec, decreasing = TRUE)
    for (i in 1:over) {
      ind <- descending.diff[i]
      if (diff.vec[ind]<=0) stop()
      new.vec[ind] <- new.vec[ind] - 1
    }
  }
  else if (round(tot.int-sum(new.vec))>0){
    # find number below total
    below <- tot.int-round(sum(new.vec))
    ascending.diff <- order(diff.vec)
    for (i in 1:below) {
      ind <- ascending.diff[i]
      if (diff.vec[ind] >= 0) stop()
      new.vec[ind] <- new.vec[ind] + 1
    }
  }
 
  return(new.vec)
}