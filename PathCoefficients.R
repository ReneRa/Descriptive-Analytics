#Returns path coefficients 
getPCs<- function(){
  latent = result$latent
  strucmod = result$strucmod
  lvS = as.data.frame(finalResult$LVScores)
  pathCoef = matrix(0, nrow = length(latent), ncol = length(latent))
  rownames(pathCoef) = latent
  colnames(pathCoef) = latent
  
  for(i in latent){
    if (i %in% strucmod[,2]){
      index <- which(i ==strucmod[,2])
      indepLV <- strucmod[index,1]
      
      #pathCoef[indepLV, i] = solve(cor(as.matrix(lvS[,indepLV]))) %*% cor(lvS[,indepLV], lvS[,i])
      pathCoef[indepLV, i] = solve(cor(lvS[,indepLV, drop = FALSE]), cor(lvS[,indepLV], lvS[,i]))
      } 
  }
  return(pathCoef)
}



