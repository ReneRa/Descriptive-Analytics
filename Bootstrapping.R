##Bootstrapping
Bootstrapping <- function(data, k){
#Handle Missing Data
  missings <- which(complete.cases(data) == FALSE)
  if(length(missings) != 0){
    data <- na.omit(data)
  }

  #Getting the original Coefficients
  source("PLSPM_Algorithm.R")
  finalResult = PLSPM(data, 1e-7, weightingScheme)
  pathCoefficient <- list(finalResult$pathCoefficients)
  Origcoeff.df <- as.data.frame(lapply(pathCoefficient, function(x){
    as.vector(x[x != 0])
  }))
  
  #Bootstrapping and Algorithm
  sublist <<- list()
  for(i in 1:k) {
    Subset<- data[sample(1:nrow(data), nrow(data), replace=TRUE),]
    subdata <- as.data.frame(Subset)
    # PLS Algorithm
    source("PLSPM_Algorithm.R")
    finalResult = PLSPM(subdata, 1e-7, weightingScheme)
    sublist[[i]] <- finalResult$pathCoefficients
  }

  #Identify all Inner Coefficients
  coeff.df <- as.data.frame(lapply(sublist, function(x){
    as.vector(x[x != 0])
    }))
  

  #Significance Test
  Origcoeff.df <-as.vector(t(Origcoeff.df))
  coeff.df <- data.frame(t(coeff.df))
  rownames(coeff.df) <- NULL
  sd.df <- as.vector(sapply(coeff.df, sd))
  ttest <- Origcoeff.df/sd.df
  
  Estimate <- result$InnerMatrix
  estimateScores <- colMeans(coeff.df)
  ErrorScores <- sd.df
  Std.Error <- result$InnerMatrix
  Estimate[Estimate == "1"] <- estimateScores
  Std.Error[Std.Error == "1"] <-ErrorScores
  Ttest <- result$InnerMatrix
  Ttest[Ttest =="1"] <- ttest
  #when the size of the resulting empirical t value is above
  #1.96, we can assume that the path coefficient is significantly different
  #from zero at a significance level of 5%
  test <- apply(Ttest, 2, function(x) ifelse(x < 1.96 & x != 0,"Not Significant" , ifelse( x> 1.96,"Significant",x)))
  test <- noquote(test) 
  
  BootstrappingResults <- list()
  BootstrappingResults$Estimate <- Estimate
  BootstrappingResults$Std.Error <- Std.Error
  BootstrappingResults$tvalues <- Ttest
  BootstrappingResults$ttest <- test
  return(BootstrappingResults)
  
}

