#assessment measures 

#Communality - measures part of the variance that is common between both a latent variable and it's indicator;
#evaluating how well the indicators are explained by its latent variable.

communalityIndex <- function(){
  LVScores = finalResult$LVScores
  outerLoadings = finalResult$outerLoadings
  for(i in LVScores){
    communality[i] = outerLoadings^2
  }
  return(communality)
}

#rSquared = the amount of variance in the endogenous LV, explained by its independent LVs
rSquared <- function()
  LVScores = finalResult$LVScores
  endLVscore = LVScores %*% pathCoefficients
# (LVscore - meanScore)^2 / (predecessorLVscores - meanScore)^2 
  rSquared = as.matrix(apply(endLVscore, 2, var) / apply(LVScores, 2, var))
  

  