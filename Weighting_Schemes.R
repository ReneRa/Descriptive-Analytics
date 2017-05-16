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