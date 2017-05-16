# Returns the final model. Takes data, treshold and method as inputs.
# Method is the method used to approximate the inner weight matrix E
PLSPM <- function(data, treshold, method){

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

  #Step1 Initialization
  LVScores = step1(data)              
  outerWeights = NULL
  firstIteration = TRUE
  numIterations = 0
  
  # Iterate until difference between old and new weights is below given treshold.
  while(TRUE){
    numIterations = numIterations + 1
    
    #Step 2
    innerLV <- step2(LVScores, method)
    
    # Save the old weights to check if the weight difference is below the given treshold later on.
    if(!is.null(outerWeights)){
      oldWeights = outerWeights
    }
    
    # Step 3
    outerWeights <- step3(data, LVScores)
    
    # Step4
    LVScores = step4(data, outerWeights)
    
    # Algorithm shouldn't converge on first run since the difference can't be calculated
    if(firstIteration == FALSE){
      if(step5(outerWeights, oldWeights, treshold) == TRUE){
        print(paste0("Algorithm converged in ", numIterations, " iterations"))
        break
      }
    }
    if(firstIteration == TRUE){
      firstIteration = FALSE
    }
  }
  
  result = list()
  result$LVScores = LVScores
  result$weights = outerWeights
  return(result$weights)
}


# Step 1: Initializing the latent factor scores
step1 <- function(data){
  M = as.matrix(result$OuterMatrix); 
  LVScores <- as.matrix(data) %*% M
  return(scale(LVScores))
}


# Step 2: Inner Approximation
step2 <- function(LVScores, method){
  R = cor(LVScores)
  D = as.data.frame(result$InnerMatrix)
  C = D + t(D)
  
  E  <- matrix(0, ncol=length(latent), nrow= length(latent))
  colnames(E) <- colnames(C)
  rownames(E) <- rownames(C)
  
  source("Weighting_Schemes.R")
  if(method=="centroid"){E <- centroid(E, R, C)}
  else if(method=="factorial"){E <- factorial(E, R, C)}  
  
  innerLV <- scale(LVScores %*% E)       #scale(LVScores %*% E)
}


# Step 3: Outer Approximation
step3 <- function(data, LVScores){
  outerWeights <- result$OuterMatrix
  for (i in result$latent){
    latentSubset <- as.matrix(subset(data, select=result$blocks[[i]]))
    latentScores <- as.matrix(LVScores[,i])
    
    outerWeights[result$blocks[[i]],i] <- cov(latentScores, latentSubset)
  }
  
  return(apply(outerWeights, 2, sumMatrixto1))
}


# Step 4: Outer Estimation of factor scores
step4 <- function(data, outerWeights){
  return(as.matrix(data) %*% outerWeights) 
}


# Step 5: Check for convergence. Returns TRUE if algorithm should converge and FALSE otherwise
step5 <- function(outerWeights, oldWeights, treshold){
  difference = 0
  
  # Calculate the difference between the old and the new weights
  for(row in 1:nrow(outerWeights)){
    for(col in 1:ncol(outerWeights)){
      if(outerWeights[row, col] != 0){
        difference = difference + ((oldWeights[row, col] - outerWeights[row, col]) / outerWeights[row,col])
      }
    }
  }
  
  # If the difference is below the given treshold, the algorithm should terminate
  if (abs(difference) < treshold){return(TRUE)}
  else{return(FALSE)}
}

# Recalculates elements in a column so that all values in that column sum to 1
sumMatrixto1 <-
  function(x){x <- x/sum(x)}