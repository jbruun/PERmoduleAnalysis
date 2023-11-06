infmat<-function(mydata,cols){
  #mydata is a matrix with 1 respondent per row and responses as columns
  #cols is a vector with column-numbers of interest
  #This function creates a matrix with information content of each response as entries
  pmat<-matrix(0,ncol=length(cols),nrow=dim(mydata)[1]) 
  for(j in 1:length(cols)){
    pmat[,j]<-probs(mydata,cols[j])  
    
  }
  IM<--log2(pmat) #matrix with information content of each response
  return(IM)
}