#Descriptive Analytics Project
rm(list = ls())
Bank = read.csv("bank.csv", header=TRUE )

Block_Image <- as.data.frame(Bank[2:6])
Block_Expectation <-as.data.frame(Bank[7:9])
Block_Qualification<-as.data.frame(Bank[10:18])
Block_Value<-as.data.frame(Bank[19:20])
Block_Satisfaction<-as.data.frame(Bank[21:23])
Block_Loyalty<-as.data.frame(Bank[24:25])
