#Assessment measures 

missings <- which(complete.cases(data) == FALSE)
if(length(missings) != 0){
  data <- na.omit(data)
}
data <- data[,result$manifest]
data <- as.data.frame(scale(data))


  discriminantLoadings = finalResult$discrimantLoadings
  outerLoadings = finalResult$outerLoadings
  LVscores = finalResult$LVScores
  outerMatrix = result$OuterMatrix
  
  #Communality - measures part of the variance that is common between both a latent variable and it's indicator;
  #evaluating how well the indicators are explained by its latent variable.
  #Calculated as the square of the standardized outerLoadings
  #HOWTO: standardize columns: (outerLoadings / colSums(outerLoadings)[col(outerLoadings)])
  communalityIndex <- function(){
    countMvs = as.data.frame(colSums(outerMatrix != 0));
    communalityIndex = outerLoadings^2
    avgCommunality = as.data.frame(t(avgIndex(communalityIndex)));
    colnames(avgCommunality) = c("AverageCommunality")
    avgCommunality$CountMVs = unlist(countMvs)
    return(avgCommunality)
  }
  
  weightedAvgCommunality <- function(){
    avgCommunality = communalityIndex()
    weightedAvgCommunality = matrix(0, ncol=1, nrow=nrow(avgCommunality));
    rownames(weightedAvgCommunality) = rownames(avgCommunality)
    for (i in 1:nrow(avgCommunality)){
      weightedAvgCommunality[i]= avgCommunality[i,1] * (avgCommunality[i,2]/sum(avgCommunality[,2]))
    }
    weightedAvgCommunality = sum(weightedAvgCommunality)
    return(weightedAvgCommunality)
  }
  
  #rSquared = the amount of variance in the endogenous LV, explained by its independent LVs
  rSquared <- function(){
    pathCoefficients = finalResult$pathCoefficients
    LVScores = finalResult$LVScores
    endLVscore = LVScores %*% pathCoefficients
  # (LVscore - meanScore)^2 / (predecessorLVscores - meanScore)^2 
    rSquared = t(as.matrix(apply(endLVscore, 2, var) / apply(LVScores, 2, var)))
    return(rSquared)
  }
  
  
  #Caluclates average of Indexes
  avgIndex <- function(df){
    avgIndex = matrix(0, nrow = 1, ncol = ncol(df)); colnames(avgIndex) = colnames(df)
    nonZeroCols = colSums(df != 0)
    for (i in 0:ncol(avgIndex)){
      avgIndex[i] = sum(df[,i]/nonZeroCols[i])
    }
    return(avgIndex)
  }
  
  stdColValues = function(df){
    stdCols = matrix(0, nrow = nrow(df), ncol=ncol(df)); colnames(stdCols) = colnames(df); rownames(stdCols) = rownames(df)
    for(i in 1:ncol(stdCols)){
     for(j in 1:nrow(stdCols)){
      stdCols[j,i] = df[j,i] / sum(df[,i])
     }
    }
    return(stdCols)
  }
  
  #Goodness of Fit (GOF) index an quality assessment for structural and measurement models-
  #Takes into account communality - making it more applicable to reflective indicators, rather than formative
  #Calculated as the geometric of the average communality and the average rSquared value
  GoF = function(){
    avgCommunality = weightedAvgCommunality
    avgrSquare = sum(rSquared)/sum(rowSums(rSquared !=0))
    GOF = sqrt(avgCommunality * avgrSquare)
    
    result = list()
    result["avgCommunality"] = avgCommunality
    result["avgrSquare"] = avgrSquare
    result["GOF"] = GOF
   
    return(result)
  }
  
  
  #Redundancy Index - measures amount of variance in an endogenous construct explained by its independent latent variables.
  #It reflects the ability of a set of independent latent variables to explain variation in the dependent latent variable. 
  #Rd(LV[k], mv[j,k]) = loading[j,k]*rSquared[k]
  redundancyIndex = function(){
    redundancy = matrix(0, nrow=nrow(outerLoadings), ncol=ncol(rSquared))
    colnames(redundancy)=colnames(rSquared); rownames(redundancy) = rownames(outerLoadings)
    for(i in 1:ncol(rSquared)){
      for(j in 1:nrow(outerLoadings)){
        redundancy[j,i] = as.matrix((outerLoadings[j,i]^2) * rSquared[,i])
      }
    }
    
    avgRedundancy = t(avgIndex(redundancy))
    return(avgRedundancy)
  }
  
  #redundancy = redundancyIndex()
  #avgRedundancy = t(avgIndex(redundancy))
  
  #Average Variance Extrcted is the degree to which a latent construct explains the variance of its indicators;
  #The amount of variance that a latent variable captures from its indicators,
  #in relation to the amount of variance due to measurement error.
  #The square of standardized indicator's outer loading represents how much of the variation in an item is explained
  #by the construct and is described as the variance extracted from the item
  AVE <- function(){
    stdOuterLoadings <- stdColValues(outerLoadings)
    avgVarianceExtracted <- stdOuterLoadings^2
    return(avgVarianceExtracted)
  }
  
  communalityIndex = communalityIndex()
  rSquared = rSquared()
  
  
  #Dillon-Goldstein's Rho
  DillonRho <- function(){
  RhoScores <- list()
  blocks <- result$blocks
  Loadings <- finalResult$outerLoadings
  for(i in 1:length(blocks)) {
    MV <- as.data.frame(Loadings[,i])
    MVs <- as.data.frame(MV[!apply(MV == "0", 1, all),])
    Rho = sum(MVs)^2/(sum(MVs)^2 +sum(1-MVs^2))
    RhoScores[[i]] <- Rho
  }
  names(RhoScores) <- paste(result$latent,  sep = "")
  return(RhoScores)
  }
  #Crombachs Alpha
  #assumes that all indicators are equally reliable
  #Cronbach's alpha is sensitive to the number of items in the scale and
  #generally tends to underestimate the internal consistency reliability
  
  CrombachsAlpha <- function(){
    
    # Remove missing values if there are any
    missings <- which(complete.cases(data) == FALSE)
    if(length(missings) != 0){
      data <- na.omit(data)
    }
    data <- data[,result$manifest]
    data <- as.data.frame(scale(data))
    #Split data into blocks
    BlockValues = list()
    for (i in result$latent){
      latentSubset <- as.matrix(subset(data, select=result$blocks[[i]]))
      BlockValues[[i]] = latentSubset
    }
    #Calculate CrombachsAlpha
    alphaScores<- list()
    for (i in 1:length(result$latent)) {
      curBlock <- as.data.frame(BlockValues[i])
      p <- ncol(curBlock)
      alpha = (p/(p-1)) * (1- sum(apply(curBlock,2,var))/sum(var(curBlock)))
      alphaScores[[i]] <- alpha
    }
    names(alphaScores) <- paste(result$latent,  sep = "") 
    return(alphaScores)
  }

  AssessmentMeasure <- list()
  AssessmentMeasure$RSquare <- rSquared
  AssessmentMeasure$CommunalityIndex <- communalityIndex
  AssessmentMeasure$GoodnessOfFit<- GoF()
  AssessmentMeasure$AverageVarianceExtracted<-AVE()
  AssessmentMeasure$DillionGoldsteinsRho <-DillonRho()
  AssessmentMeasure$CronbachsAlpha<-CrombachsAlpha()
  AssessmentMeasure$AverageRedundancy <- redundancyIndex()

