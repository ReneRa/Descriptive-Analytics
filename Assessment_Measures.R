#Assessment measures 

#Communality - measures part of the variance that is common between both a latent variable and it's indicator;
#evaluating how well the indicators are explained by its latent variable.
#Calculated as the square of the standardized outerLoadings
#HOWTO: standardize columns: (outerLoadings / colSums(outerLoadings)[col(outerLoadings)])
communalityIndex <- function(){
  outerLoadings = finalResult$outerLoadings
  communalityIndex = outerLoadings^2
  return(communalityIndex)
}

avgCommunalityIndex <- function(communalityIndex){
  avgCommunalityIndex = matrix(0, nrow = 1, ncol = ncol(communalityIndex)); colnames(avgCommunalityIndex) = colnames(communalityIndex)
  nonZeroCols = colSums(communalityIndex != 0)
  for (i in 0:ncol(avgCommunalityIndex)){
    sum(communalityIndex[,i]/nonZeroCols)
  }
return(avgCommunalityIndex)
}
#TODO: Print "." instead of zero's. Print average value for each outerLoading variable. Print overll avg.


#rSquared = the amount of variance in the endogenous LV, explained by its independent LVs
rSquared <- function(){
  LVScores = finalResult$LVScores
  endLVscore = LVScores %*% pathCoefficients
# (LVscore - meanScore)^2 / (predecessorLVscores - meanScore)^2 
  rSquared = as.matrix(apply(endLVscore, 2, var) / apply(LVScores, 2, var))
  return(rSquared)
}


#Goodness of Fit (GOF) index an quality assessment for structural and measurement models-
#Takes into account communality - making it more applicable to reflective indicators, rather than formative
#Calculated as the geometric of the average communality and the average rSquared value
