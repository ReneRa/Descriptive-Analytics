S = cor(finalResult$LVScores)
weights = finalResult$outerWeights
LV = finalResult$LVScores

Pa = ((t(weights) %*% weights)^2) %*% (t(weights %*% (S-diag(S))) %*% weights) / (t(weights) %*% (weights %*% t(weights) - diag(weights %*% t(weights)))%*% weights)

cor(LV[,1], LV[,2])/sqrt((Pa[,1]) %*% (Pa[,2]))

Pa %*% LV[1,]
Pa

latent = result$latent

correlations  <- matrix(0, ncol=length(latent), nrow= length(latent))
colnames(correlations) = latent
rownames(correlations) = latent

for (latent1 in latent){
  for (latent2 in latent){
    correlations[latent1, latent2] = cor(LV[,latent1], LV[,latent2])/sqrt((Pa[,latent1]) %*% (Pa[,latent2]))
  }
}

solve(correlations) * correlations["Loyalty",]

result$strucmod

correl