#Descriptive Analytics Project
rm(list = ls())
data = read.csv("bank.csv", header=TRUE )

# Manual Input
strucmodel <<- matrix(c("Image","Expectation", "Image","Loyalty","Image","Satisfaction","Expectation","Satisfaction","Expectation","Quality","Expectation","Value","Quality","Value","Quality","Satisfaction","Value","Satisfaction","Satisfaction","Loyalty"),ncol=2, byrow = T)
colnames(strucmodel)<-c("Source","Target")
measuremodel <<- matrix(c("Image", "IMAG1","Image","IMAG2","Image","IMAG3","Image","IMAG4","Image","IMAG5","Expectation","EXPE1","Expectation","EXPE2","Expectation","EXPE3","Quality","QUAL1","Quality","QUAL2","Quality","QUAL3","Quality","QUAL4","Quality","QUAL5","Quality","QUAL6","Quality","QUAL7","Quality","QUAL8","Quality","QUAL9","Value","VALU1","Value","VALU2","Satisfaction","SATI1","Satisfaction","SATI2","Satisfaction","SATI3","Loyalty","LOYA1","Loyalty","LOYA2"),ncol=2,byrow = T)
colnames(strucmodel)<-c("Source","Target")

# PLS Preparation
source("PLS_Prep.R")
PLS_Prep(data,strucmodel,measuremodel)

# PLS Algorithm
