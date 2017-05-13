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

strucmodel <- matrix(c("Image","Expectation", "Image","Loyalty","Image","Satisfaction","Expectation","Satisfaction","Expectation","Quality","Expectation","Value","Quality","Value","Quality","Satisfaction","Value","Satisfaction","Satisfaction","Loyalty"),ncol=2, byrow = T)
colnames(strucmodel)<-c("Source","Target")
measuremodel<- matrix(c("Image", "IMAG1","Image","IMAG2","Image","IMAG3","Image","IMAG4","Image","IMAG5","Expectation","EXPE1","Expectation","EXPE2","Expectation","EXPE3","Quality","QUAL1","Quality","QUAL2","Quality","QUAL3","Quality","QUAL4","Quality","QUAL5","Quality","QUAL6","Quality","QUAL7","Quality","QUAL8","Quality","QUAL9","Value","VALU1","Value","VALU2","Satisfaction","SATI1","Satisfaction","SATI2","Satisfaction","SATI3","Loyalty","LOYA1","Loyalty","LOYA2"),ncol=2,byrow = T)
colnames(strucmodel)<-c("Source","Target")

#my.plspm.model <- function(data, strucmod, measuremod)


latent <- unique(as.vector(strucmodel))
manifest <- sort(setdiff(as.vector(measuremodel),latent))


block <- function(latent, manifest, measuremodel){
  ln <- length(latent)
  colnames(measuremodel) <- NULL
  blocks <- list()
  
  
  for (i in 1:ln){
    blocks[[i]] <- measuremodel[c(which(measuremodel[,1] == latent[i], which(measuremodel[,2] == latent[i]))),]
    blocks[[i]] <- append(blocks[[i]], 
                          measuremodel[c(which(measuremodel[,2] == latent[i], which(measuremodel[,1] == latent[i]))),])
    blocks[[i]] <- sort(blocks[[i]][blocks[[i]] %in% manifest])
  
  
    # Determine the mode (A=Reflective, B=Formative)
    if(all(blocks[[i]] %in% measuremodel[,2])){
      attr(blocks[[i]], "mode") <- "A"
    } else if(all(blocks[[i]] %in% measuremodel[,1])){
      attr(blocks[[i]], "mode") <- "B"
    }
      else stop("A block must be either formative or reflective. Not both")
  }
  return(blocks)
  
}

blocks <- block(latent, manifest, measuremodel)

  