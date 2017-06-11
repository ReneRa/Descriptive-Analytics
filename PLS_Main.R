#Descriptive Analytics Project
rm(list = ls())

# Set to true to let the algorithm run formative blocks instead of reflective blocks.
formativeBlocks = FALSE

# Set this value to the desired weighting scheme: Centroid, factorial or path
weightingScheme = "centroid"
bootstrapping = FALSE
k= 100

data = read.csv("bank.csv", header=TRUE )

# Manual Input
strucmodel <<- matrix(c("Image","Expectation", "Image","Loyalty","Image","Satisfaction","Expectation","Satisfaction","Expectation","Quality","Expectation","Value","Quality","Value","Quality","Satisfaction","Value","Satisfaction","Satisfaction","Loyalty"),ncol=2, byrow = T)
colnames(strucmodel)<-c("Source","Target")
if(formativeBlocks){
  measuremodel <<- matrix(c("IMAG1", "Image","IMAG2","Image","IMAG3","Image","IMAG4","Image","IMAG5","Image","EXPE1","Expectation","EXPE2","Expectation","EXPE3","Expectation","QUAL1","Quality","QUAL2","Quality","QUAL3","Quality","QUAL4","Quality","QUAL5","Quality","QUAL6","Quality","QUAL7","Quality","QUAL8","Quality","QUAL9","Quality","VALU1","Value","VALU2","Value","SATI1","Satisfaction","SATI2","Satisfaction","SATI3","Satisfaction","LOYA1","Loyalty","LOYA2","Loyalty"),ncol=2,byrow = T)
}else
  {
  measuremodel <<- matrix(c("Image", "IMAG1","Image","IMAG2","Image","IMAG3","Image","IMAG4","Image","IMAG5","Expectation","EXPE1","Expectation","EXPE2","Expectation","EXPE3","Quality","QUAL1","Quality","QUAL2","Quality","QUAL3","Quality","QUAL4","Quality","QUAL5","Quality","QUAL6","Quality","QUAL7","Quality","QUAL8","Quality","QUAL9","Value","VALU1","Value","VALU2","Satisfaction","SATI1","Satisfaction","SATI2","Satisfaction","SATI3","Loyalty","LOYA1","Loyalty","LOYA2"),ncol=2,byrow = T)
}
colnames(strucmodel)<-c("Source","Target")

# PLS Preparation
source("PLS_Prep.R")
PLS_Prep(data,strucmodel,measuremodel)

if(bootstrapping == TRUE) {
  # Bootstrapping with k subsets
  source("Bootstrapping.R")
  Bootstrapping(data, k)
} else {
  
# PLS Algorithm
source("PLSPM_Algorithm.R")
finalResult = PLSPM(data, 1e-7, weightingScheme)
}
validateFormativeBlocks <- function(){
  semPLSModel <- plsm(data = data, strucmod = strucmodel, measuremod = measuremodel)
  semplsResults <- sempls(semPLSModel, data)
}

# Assesment Measures
source(file = "Assessment_Measures.R")
AssessmentMeasure$RSquare
AssessmentMeasure$CommunalityIndex
AssessmentMeasure$GoodnessOfFit
AssessmentMeasure$RedundancyIndexes
AssessmentMeasure$DillionGoldsteinsRho
AssessmentMeasure$CronbachsAlpha
AssessmentMeasure$AverageRedundancy
