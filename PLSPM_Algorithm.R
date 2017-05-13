PLSPM <- function(data){

  # Handle missing data   
  # TODO: EVERYTHING IS REMOVED RIGHT NOW, MIGHT WANT TO CALCULATE MISSING VALUES
  data <- data[,result$manifest]
  N <- nrow(data)
  missings <- which(complete.cases(data) == FALSE)
  
  # Remove missing values if there are any
  if(length(missings) != 0){
    data <- na.omit(data)
  }
  data <- data[,result$manifest]
  data <- as.data.frame(scale(data))

data1 = data[2:25]
data1 = as.data.frame(scale(data1))



###Step1 Initialization
  M = as.matrix(result$OuterMatrix); M
  initialLV <- as.matrix(data) %*% M


###Step2 Inner Approximation
  R = cor(initialLV)
  D = as.data.frame(result$InnerMatrix)
  C = D + t(D)
  
  E  <- matrix(0, ncol=length(latent), nrow= length(latent))
  colnames(E) <- colnames(C)
  rownames(E) <- rownames(C)
  E <- factorial(E, R, C)
  
  innerLV <- scale(initialLV %*% E)
  
}


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