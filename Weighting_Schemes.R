# Returns the inner weight matrix E using the centroid weighting scheme
centroid <- function(E, R, C){
  for(i in 1: ncol(C)){
    for(j in 1: nrow(C)){
      if(C[i, j] == 1){
        E[i,j] = sign(R[i,j])
      }
      else {
        0
      }
    }
  }
  return(E)
}

# Returns the inner weight matrix E using the factorial weighting scheme
factorial <- function(E, R, C){
  for(i in 1: ncol(C)){
    for(j in 1: nrow(C)){
      if(C[i, j] == 1){
        E[i,j] = R[i,j]
      }
      else {
        0
      }
    }
  }
  return(E)
}

# Returns the inner weight matrix using the path weighting scheme
path <- function(LVScores){
  # E has -1 values if a row depicts the column's successor and 1 if it's the predecessor
  E <- result$InnerMatrix - t(result$InnerMatrix)
  predecessors <- getPredecessors()

  innerW <- E
  for (i in result$latent){
    if(length(predecessors[[i]])==0) next
    else if (length(predecessors[[i]])==1){
      innerW[predecessors[[i]], i] <- cor(LVScores[,predecessors[[i]]], LVScores[,i])
    }
    innerW[predecessors[[i]], i] <- solve(cor(as.matrix(LVScores[,predecessors[[i]]]))) %*%
      cor(LVScores[,predecessors[[i]]], LVScores[,i])
    
  }
  
  innerW[E == 0] <- 0
  # Use correlation if the related Latent is a successor.
  innerW[E == -1] <- cor(as.matrix(LVScores[, latent]))[E == -1]

  return(innerW)
  }