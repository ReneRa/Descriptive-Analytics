#Descriptive Analytics Project
rm(list = ls())
data = read.csv("bank.csv", header=TRUE )

#Structural and Measurement Model
strucmodel <- matrix(c("Image","Expectation", "Image","Loyalty","Image","Satisfaction","Expectation","Satisfaction","Expectation","Quality","Expectation","Value","Quality","Value","Quality","Satisfaction","Value","Satisfaction","Satisfaction","Loyalty"),ncol=2, byrow = T)
colnames(strucmodel)<-c("Source","Target")
measuremodel<- matrix(c("Image", "IMAG1","Image","IMAG2","Image","IMAG3","Image","IMAG4","Image","IMAG5","Expectation","EXPE1","Expectation","EXPE2","Expectation","EXPE3","Quality","QUAL1","Quality","QUAL2","Quality","QUAL3","Quality","QUAL4","Quality","QUAL5","Quality","QUAL6","Quality","QUAL7","Quality","QUAL8","Quality","QUAL9","Value","VALU1","Value","VALU2","Satisfaction","SATI1","Satisfaction","SATI2","Satisfaction","SATI3","Loyalty","LOYA1","Loyalty","LOYA2"),ncol=2,byrow = T)
colnames(strucmodel)<-c("Source","Target")


PLS_Prep<-function(data,strucmodel,measuremodel){
latent <- unique(as.vector(strucmodel))
manifest <-sort(setdiff(as.vector(measuremodel),latent))
# Initialize Outer Weight
Init<- function(model){
  measuremod <- measuremodel
  latent <- latent
  mf <- manifest
  M <- matrix(0, ncol=length(latent), nrow= length(mf))
  colnames(M) <- latent
  rownames(M) <- mf
  for(i in 1: nrow(measuremod)){
    if (measuremod[i,1] %in% mf)
      M[which(mf==measuremod[i,1]), which(latent ==measuremod[i,2])] <-1
    if(measuremod[i,2] %in% mf)
      M[which(mf==measuremod[i,2]), which(latent ==measuremod[i,1])] <-1
  }
  return(M)
}
#Initialize Inner Weights

InnerW <- function(structuremodel) {
  latent <- latent
  Innermatrix <- matrix(0, ncol=length(latent), nrow=length(latent))
  colnames(Innermatrix) <-latent
  rownames(Innermatrix) <-latent
  for(i in 1:nrow(strucmodel)) {
    Innermatrix[which(latent==strucmodel[i,1]), which(latent==strucmodel[i,2])] <-1
  }
  return(Innermatrix)
}

block <- function(latent, manifest, measuremodel) {
  ln<-length(latent)
  colnames(measuremodel) <- NULL
  blocks<- list()
  
  for(i in 1:ln) {
    blocks[[i]] <- measuremodel[c(which(measuremodel[,1]==latent[i],which(measuremodel[,2]==latent[i])))]
    blocks[[i]] <- append(blocks[[i]], measuremodel[c(which(measuremodel[,2]==latent[i], which(measuremodel[,1]==latent[i])))])
    blocks[[i]] <- sort(blocks[[i]][blocks[[i]] %in% manifest])
  
    #determine the mode ("A"=reflective, "B"=Formative)  
    if(all(blocks[[i]] %in% measuremodel[,2])) {
      attr(blocks[[i]], "mode") <-"A"
    }
    else if(all(blocks[[i]] %in% measuremodel[,1])){
      attr(blocks[[i]], "mode") <-"B"
    }
    else stop("A block must bei either formative or reflective, not both")
  }
  names(blocks) <- latent
  return(blocks)
}

result <- list()
result$latent <- latent
result$manifest <- manifest
result$strucmod <- strucmodel
result$measuremod <- measuremodel
result$blocks <- blocks <-block(latent, manifest, measuremodel) 
result$InnerMatrix <- InnerW(strucmod)
result$OuterMatrix <-Init(model=Result)
return(result)
}
