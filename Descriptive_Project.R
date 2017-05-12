#Descriptive Analytics Project
rm(list = ls())
setwd("Desktop/Descriptive Statistics/")
Bank = read.csv("bank.csv", header=TRUE )
#Creating blocks as.data.frame and normalized (mean=0,sd=1)
Block_Image <- scale(data.frame(IMAG1 =rnorm(Bank$IMAG1),IMAG2 = rnorm(Bank$IMAG2),IMAG3 = rnorm(Bank$IMAG3),IMAG4 = rnorm(Bank$IMAG4),IMAG5 = rnorm(Bank$IMAG5)))
Block_Expectation <- scale(data.frame(EXPE1 =rnorm(Bank$EXPE1), EXPE2 =rnorm(Bank$EXPE2),EXPE3 =rnorm(Bank$EXPE3)))
Block_Qualification<-scale(data.frame(QUAL1 =rnorm(Bank$QUAL1), QUAL2 =rnorm(Bank$QUAL2),QUAL3 =rnorm(Bank$QUAL3),QUAL4 =rnorm(Bank$QUAL4),QUAL5 =rnorm(Bank$QUAL5),QUAL6 =rnorm(Bank$QUAL6),QUAL7 =rnorm(Bank$QUAL7),QUAL8 =rnorm(Bank$QUAL8),QUAL9 =rnorm(Bank$QUAL9)))
Block_Value<-scale(data.frame(VALU1 =rnorm(Bank$VALU1),VALU2 =rnorm(Bank$VALU2)))
Block_Satisfaction<-scale(data.frame(SATI1 =rnorm(Bank$SATI1),SATI2 =rnorm(Bank$SATI2),SATI3 =rnorm(Bank$SATI3)))
Block_Loyalty<-scale(data.frame(LOYA1 =rnorm(Bank$LOYA1),LOYA2 =rnorm(Bank$LOYA2)))
# Outer Matrix, adjacency matrix
Outer_Matrix = matrix(c(1,0,0,0,0,0, 1,0,0,0,0,0, 1,0,0,0,0,0, 0,1,0,0,0,0, 0,1,0,0,0,0, 0,1,0,0,0,0, 0,1,0,0,0,0, 0,1,0,0,0,0, 0,0,1,0,0,0, 0,0,1,0,0,0, 0,0,1,0,0,0, 0,0,1,0,0,0, 0,0,1,0,0,0, 0,0,1,0,0,0, 0,0,1,0,0,0, 0,0,1,0,0,0, 0,0,1,0,0,0, 0,0,0,1,0,0, 0,0,0,1,0,0, 0,0,0,1,0,0, 0,0,0,0,1,0, 0,0,0,0,1,0, 0,0,0,0,0,1, 0,0,0,0,0,1, 0,0,0,0,0,1),byrow= TRUE,nrow=25,ncol=6)
dimnames(Outer_Matrix) = list(c("EXPE1","EXPE2","EXPE3","IMAG1","IMAG2","IMAG3","IMAG4","IMAG5","QUAL1","QUAL2","QUAL3","QUAL4","QUAL5","QUAL6","QUAL7","QUAL8","QUAL9","SATI1","SATI2","SATI3","VALU1","VALU2","LOYA1","LOYA2","LOYA3"),c("Expectations","Image","Quality","Satisfaction","Value","Loyalty"))
Outer_Matrix
# Inner Block, Path Model, lower triangular boolean matrix:
Image = c(0,0,0,0,0,0)
Expectation = c(1,0,0,0,0,0)
Quality = c(0,1,0,0,0,0)
Value= c(0,1,1,0,0,0)
Satisfaction = c(1,1,1,1,0,0)
Loyalty = c(1,0,0,0,1,0)
path_matrix = rbind(Image,Expectation,Quality,Value,Satisfaction,Loyalty)
library(plspm)
innerplot(path_matrix) #PLSPM Package to review result
# PLS Algorithm
#Step1 (Initialization)
Outer Matrix * MV
-#Step2 Inner Approximation
In the inner approximation we estimate each LV as a
weighted sum of its neighbouring LVs. The weighting depends on the used scheme
(see Section 2.3.1). Again we are scaling the recomputed LVs to have unit variance.
#Step3 Outer Approximation Mode A/B

#Step4 Calculating Factor Scores
#Step5 Convergence criteria (Otherwise go back to step2)
################################################################
#Init M1()
# Initialize the outer weights



