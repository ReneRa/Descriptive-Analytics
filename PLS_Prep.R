PLS_Prep<-function(data,strucmodel,measuremodel){

  latent <<- unique(as.vector(strucmodel))
  manifest <<-sort(setdiff(as.vector(measuremodel),latent))
  
  source("PLS_Prep.R")
  block <- block(latent, manifest, measuremodel) 
  InnerW(strucmodel)
  OuterW(latent)
  
  result <<- list()
  result$latent <<- latent
  result$manifest <<- manifest
  result$strucmod <<- strucmodel
  result$measuremod <<- measuremodel
  result$blocks <<-block(latent, manifest, measuremodel) 
  result$InnerMatrix <<- InnerW(strucmodel)
  result$OuterMatrix <<- OuterW(latent)
  return(result)
}

OuterW<- function(latent){
  OuterMatrix <- matrix(0, ncol=length(latent), nrow= length(manifest))
  colnames(OuterMatrix) <- latent
  rownames(OuterMatrix) <- manifest
  for(i in 1: nrow(measuremodel)){
    if(measuremodel[i,1] %in% manifest)
      OuterMatrix[which(manifest==measuremodel[i,1]), which(latent ==measuremodel[i,2])] <-1
    if(measuremodel[i,2] %in% manifest)
      OuterMatrix[which(manifest==measuremodel[i,2]), which(latent ==measuremodel[i,1])] <-1
  }
  return(OuterMatrix)
}

#Initialize Inner Weights
InnerW <- function(strucmodel) {
  Innermatrix <- matrix(0, ncol=length(latent), nrow=length(latent))
  colnames(Innermatrix) <-latent
  rownames(Innermatrix) <-latent
  for(i in 1:nrow(strucmodel)) {
    Innermatrix[which(latent==strucmodel[i,1]), which(latent==strucmodel[i,2])] <-1
  }
  return(Innermatrix)
}


#create Blocks

#block <- resulfunction(latent, manifest, measuremodel) {
#  ln<-length(latent)


# block <- function(latent, manifest, measuremodel) {
#   ln<-length(latent)
#   colnames(measuremodel) <- NULL
#   blocks<- list()
#   
#   for(i in 1:ln) {
#     blocks[[i]] <- measuremodel[c(which(measuremodel[,1]==latent[i],which(measuremodel[,2]==latent[i])))]
#     blocks[[i]] <- append(blocks[[i]], measuremodel[c(which(measuremodel[,2]==latent[i], which(measuremodel[,1]==latent[i])))])
#     blocks[[i]] <- sort(blocks[[i]][blocks[[i]] %in% manifest])
#     
#     #determine the mode ("A"=reflective, "B"=Formative)  
#     if(all(blocks[[i]] %in% measuremodel[,2])) {
#       attr(blocks[[i]], "mode") <-"A"
#     }
#     else if(all(blocks[[i]] %in% measuremodel[,1])){
#       attr(blocks[[i]], "mode") <-"B"
#     }
#     else stop("A block must bei either formative or reflective, not both")
#   }
#   names(blocks) <- latent
#   return(blocks)
# }

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