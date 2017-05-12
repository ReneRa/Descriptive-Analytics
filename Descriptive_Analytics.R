#Descriptive Analytics Project
rm(list = ls())
Bank = read.csv("bank.csv", header=TRUE )

Block_Image <- as.data.frame(Bank[2:6])
Block_Expectation <-as.data.frame(Bank[7:9])
Block_Qualification<-as.data.frame(Bank[10:18])
Block_Value<-as.data.frame(Bank[19:20])
Block_Satisfaction<-as.data.frame(Bank[21:23])
Block_Loyalty<-as.data.frame(Bank[24:25])

(IMAG1 =rnorm(Bank$IMAG1),IMAG2 = rnorm(Bank$IMAG2),IMAG3 = rnorm(Bank$IMAG3),IMAG4 = rnorm(Bank$IMAG4),IMAG5 = rnorm(Bank$IMAG5)))
Block_Expectation <- scale(data.frame(EXPE1 =rnorm(Bank$EXPE1), EXPE2 =rnorm(Bank$EXPE2),EXPE3 =rnorm(Bank$EXPE3)))
Block_Qualification<-scale(data.frame(QUAL1 =rnorm(Bank$QUAL1), QUAL2 =rnorm(Bank$QUAL2),QUAL3 =rnorm(Bank$QUAL3),QUAL4 =rnorm(Bank$QUAL4),QUAL5 =rnorm(Bank$QUAL5),QUAL6 =rnorm(Bank$QUAL6),QUAL7 =rnorm(Bank$QUAL7),QUAL8 =rnorm(Bank$QUAL8),QUAL9 =rnorm(Bank$QUAL9)))
Block_Value<-scale(data.frame(VALU1 =rnorm(Bank$VALU1),VALU2 =rnorm(Bank$VALU2)))
Block_Satisfaction<-scale(data.frame(SATI1 =rnorm(Bank$SATI1),SATI2 =rnorm(Bank$SATI2),SATI3 =rnorm(Bank$SATI3)))
Block_Loyalty<-scale(data.frame(LOYA1 =rnorm(Bank$LOYA1),LOYA2 =rnorm(Bank$LOYA2)))