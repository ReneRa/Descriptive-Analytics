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
  else if(method=="path"){E <- path(LVScores)}

  innerLV <- scale(LVScores %*% E)       #scale(LVScores %*% E)
}


# Step 3: Outer Approximation
step3 <- function(data, LVScores){
  blocks <- result$blocks
  outerWeights <- result$OuterMatrix
  for (i in result$latent){
    latentSubset <- as.matrix(subset(data, select=result$blocks[[i]]))
    latentScores <- as.matrix(LVScores[,i])
    
    if (attr(blocks[[i]], "mode")== "A"){
      outerWeights[result$blocks[[i]],i] <- cor(latentScores, latentSubset)
    }
    
    if (attr(blocks[[i]], "mode") == "B") {
      #outerWeights[result$blocks[[i]],i]  <- ((solve(t(latentSubset) %*% latentSubset)) %*% t(latentSubset)) %*% latentScores
      outerWeights[result$blocks[[i]],i] <- solve(var(latentSubset)) %*% cor(latentSubset, latentScores)
      
      }
    
  }
  
  return(outerWeights)
  
  #return(apply(outerWeights, 2, sumMatrixto1))
}


# # Step 4: Outer Estimation of factor scores
# step4 <- function(data, outerWeights){
#   return(as.matrix(data) %*% outerWeights) 
# }

step4 <- function(data, outerWeights){
  blocks = result$blocks
  Latent <- matrix(NA, nrow=nrow(data), ncol=length(result$latent)) # factor scores
  colnames(Latent) <- result$latent
  for(i in result$latent){
    mf <- as.matrix(data[ , blocks[[i]] ])
    #Latent[,i] <- mf %*% as.matrix(outerW[blocks[[i]], i])
    Latent[,i] <- mf %*% outerWeights[blocks[[i]], i, drop=FALSE]
  }
  Latent <- scale(Latent)
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