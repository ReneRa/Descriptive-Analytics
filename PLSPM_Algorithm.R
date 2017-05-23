# Returns the final model. Takes data, treshold and method as inputs.
# Method is the method used to approximate the inner weight matrix E
PLSPM <- function(data, treshold, method){
  source("Iterative_Steps.R")
  
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
    LVScores <- step2(LVScores, method)
    
    # Save the old weights to check if the weight difference is below the given treshold later on.
    if(!is.null(outerWeights)){
      oldWeights = outerWeights
    }
    
    # Step 3
    outerWeights <- step3(data, LVScores)
    
    # Step4
    LVScores = step4(data, outerWeights)
    
    sd <-rep(attr(LVScores, "scaled:scale"), each=length(result$manifest))
    outerWeights = outerWeights / sd
    
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
  
  crossLoadings = cor(data, LVScores)
  outerLoadings = as.matrix(result$OuterMatrix) * as.matrix(crossLoadings)
  
  result = list()
  result$LVScores = LVScores
  result$outerWeights = outerWeights
  result$crossLoadings = crossLoadings
  result$outerLoadings = outerLoadings
  result$pathCoefficients = getPCs(LVScores)
  
  return(result)
}

getPCs<- function(LVScores){
  
  predecessors = getPredecessors()
  latent = result$latent
  pathCoef = matrix(0, nrow = length(latent), ncol = length(latent))
  rownames(pathCoef) = latent
  colnames(pathCoef) = latent

  for (i in latent){
    # Check if current LV has predecessor
    currentPredecessors = predecessors[[i]]
    if (length(currentPredecessors) != 0){
      predLVScores = LVScores[,currentPredecessors]
      pathCoef[currentPredecessors, i] = solve(cor(LVScores[,currentPredecessors, drop=FALSE]), cor(LVScores[,currentPredecessors], LVScores[,i]))
    }
  }
 return(pathCoef)
  
}

# Returns a list with Latents and their predecessors
getPredecessors <- function(){
  predecessors = list()
  for(col in colnames(result$InnerMatrix)){
    predecessors[col] <- list(names(which(result$InnerMatrix[, col] == 1)))
  }
  return(predecessors)
}

# Recalculates elements in a column so that all values in that column sum to 1
sumMatrixto1 <-
  function(x){x <- x/sum(x)}