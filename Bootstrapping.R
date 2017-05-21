##Bootstrapping

#Handle Missing Data
missings <- which(complete.cases(data) == FALSE)
if(length(missings) != 0){
  data <- na.omit(data)
}

##create subsets
k= 10
sublist = list()
for(i in 1:k) {
  Subset<- data[sample(1:nrow(data), nrow(data), replace=TRUE),]
  subdata <- as.data.frame(Subset)
  # PLS Algorithm
  source("PLSPM_Algorithm.R")
  finalResult = PLSPM(subdata, 1e-7, weightingScheme)
  sublist[[i]] <- finalResult$outerWeights
}

for (i in 1:#nicht k, sondern anzahl an Ausprägungen) {
  #
  
}
  
  
# sign indeterminacy of latent variable
# determine standard error and standard deviation
# Deal with sign changes, no sign change option, the individual-level sign change
# option, and the construct-level sign change option
# Chosen method: No Sign Change