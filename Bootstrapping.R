##Bootstrapping

#Handle Missing Data
missings <- which(complete.cases(data) == FALSE)
if(length(missings) != 0){
  data <- na.omit(data)
}

#Bootstrapping and Algorithm
k= 10
sublist = list()
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
#Identify original Coefficients
pathCoefficient <- list(finalResult$pathCoefficients)
Origcoeff.df <- as.data.frame(lapply(pathCoefficient, function(x){
  as.vector(x[x != 0])
}))


#Significance Test
Origcoeff.df <-as.vector(t(Origcoeff.df))
coeff.df <- data.frame(t(coeff.df))
rownames(coeff.df) <- NULL
sd.df <- as.vector(sapply(coeff.df, sd))
ttest <- orig.coeff / sd

#when the size of the resulting empirical t value is above
#1.96, we can assume that the path coefficient is significantly different
#from zero at a significance level of 5%

