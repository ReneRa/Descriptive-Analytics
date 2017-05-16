PLSPM <- function(data, treshold){

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


###Step1 Initialization
  M = as.matrix(result$OuterMatrix); 
  LVScores <- as.matrix(data) %*% M
  LVScores <- scale(LVScores)                   # Why did we not scale previously?
  innerWeights = NULL
  firstIteration = TRUE
  it = 0
  while(TRUE){
    it = it + 1
    ###Step2 Inner Approximation
    R = cor(LVScores)
    D = as.data.frame(result$InnerMatrix)
    C = D + t(D)
    
    E  <- matrix(0, ncol=length(latent), nrow= length(latent))
    colnames(E) <- colnames(C)
    rownames(E) <- rownames(C)
    E <- factorial(E, R, C)                   
    
    innerLV <- LVScores %*% E       #scale(LVScores %*% E)
    
    
    #library(sem)                            #factorial weighting scheme
    #C[C==1] <- cor(fscores, use="everything", method="pearson")[C == 1]
    #innerW <- C
    #innerW[C == 1] <- cor(fscores, use="everything", method="pearson")[C == 1]
    #return(innerW)
    
    
    
    
    ###Step3 
    if(!is.null(innerWeights)){
      oldWeights = innerWeights
    }
    innerWeights <- t(cor(innerLV, data))
    
    #Set non adjacent LV's to 0
    for(col in 1:ncol(innerWeights)){
      for(row in 1:nrow(innerWeights)){
        if(result$OuterMatrix[row, col] == 0){
          innerWeights[row,col] = 0
        }
      }
    }
    
    ###Step4
    
    #LVScores = as.matrix(data) %*% OuterW() 
    LVScores = as.matrix(data) %*% innerWeights  ## Supposed to be Outer Weights????
    
    ###Step5
    difference = 0
    
    if(firstIteration == FALSE){
    
      # Calculate the difference between the old and the new weights
      for(row in 1:nrow(innerWeights)){
        for(col in 1:ncol(innerWeights)){
          if(innerWeights[row, col] != 0){
            difference = difference + ((oldWeights[row, col] - innerWeights[row, col]) / innerWeights[row,col])
          }
        }
      }
    
      if (abs(difference) < treshold){
        break
      }
    }
    
    if(firstIteration == TRUE){
      firstIteration = FALSE
    }
  }
  
  result = list()
  result$LVScores = LVScores
  result$weights = innerWeights
  return(result)
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

