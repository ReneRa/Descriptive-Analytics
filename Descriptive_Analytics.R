#Descriptive Analytics Project
rm(list = ls())
data = read.csv("bank.csv", header=TRUE )

# Should not be used
Block_Image <- as.data.frame(data[2:6])
Block_Expectation <-as.data.frame(data[7:9])
Block_Qualification<-as.data.frame(data[10:18])
Block_Value<-as.data.frame(data[19:20])
Block_Satisfaction<-as.data.frame(data[21:23])
Block_Loyalty<-as.data.frame(data[24:25])

strucmodel <- matrix(c("Image","Expectations", "Image","Loyalty","Image","Satisfaction","Expectation","Satisfaction","Expectation","Quality","Expectation","Value","Quality","Value","Quality","Satisfaction","Value","Satisfaction","Satisfaction","Loyalty"),ncol=2, byrow = T)
colnames(strucmodel)<-c("Source","Target")
measuremodel<- matrix(c("Image", "IMAG1","Image","IMAG2","Image","IMAG3","Image","IMAG4","Image","IMAG5","Expectation","EXPE1","Expectation","EXPE2","Expectation","EXPE3","Quality","QUAL1","Quality","QUAL2","Quality","QUAL3","Quality","QUAL4","Quality","QUAL5","Quality","QUAL6","Quality","QUAL7","Quality","QUAL8","Quality","QUAL9","Value","VALU1","Value","VALU2","Satisfaction","SATI1","Satisfaction","SATI2","Satisfaction","SATI3","Loyalty","LOYA1","Loyalty","LOYA2"),ncol=2,byrow = T)
colnames(strucmodel)<-c("Source","Target")

#my.plspm.model <- function(data, strucmod, measuremod)

  
