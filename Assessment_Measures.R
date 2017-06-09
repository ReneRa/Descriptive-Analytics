#Assessment measures 

outerLoadings = finalResult$outerLoadings
LVscores = finalResult$LVScores

#Communality - measures part of the variance that is common between both a latent variable and it's indicator;
#evaluating how well the indicators are explained by its latent variable.
#Calculated as the square of the standardized outerLoadings
#HOWTO: standardize columns: (outerLoadings / colSums(outerLoadings)[col(outerLoadings)])
communalityIndex <- function(){
  communalityIndex = outerLoadings^2
  return(communalityIndex)
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

communalityIndex = communalityIndex()
rSquared = rSquared()
avgCommunality = mean(avgIndex(communalityIndex))
avgrSquare = sum(rSquared)/sum(rowSums(rSquared !=0))
stdOuterLoadings = stdColValues(outerLoadings)
avgVarianceExtracted = stdOuterLoadings^2

#Goodness of Fit (GOF) index an quality assessment for structural and measurement models-
#Takes into account communality - making it more applicable to reflective indicators, rather than formative
#Calculated as the geometric of the average communality and the average rSquared value
GoF = sqrt(avgCommunality * avgrSquare)


#Redundancy Index - measures amount of variance in an endogenous construct explained by its independent latent variables.
#It reflects the ability of a set of independent latent variables to explain variation in the dependent latent variable. 
#Rd(LV[k], mv[j,k]) = loading[j,k]*rSquared[k]
redundancyIndex = function(){
  redundancy = matrix(0, nrow=nrow(outerLoadings), ncol=ncol(rSquared))
  colnames(redundancy)=colnames(rSquared); rownames(redundancy) = rownames(outerLoadings)
  for(i in 1:ncol(rSquared)){
    for(j in 1:nrow(outerLoadings)){
      redundancy[j,i] = as.matrix(outerLoadings[j,i] * rSquared[,i])
    }
  }
  return(redundancy)
}
redundancy = redundancyIndex()
avgRedundancy = t(avgIndex(redundancy))

#Average Variance Extrcted is the degree to which a latent construct explains the variance of its indicators;
#The amount of variance that a latent variable captures from its indicators,
#in relation to the amount of variance due to measurement error.
#The square of standardized indicator's outer loading represents how much of the variation in an item is explained
#by the construct and is described as the variance extracted from the item


