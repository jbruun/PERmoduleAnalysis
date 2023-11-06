simResVec<-function(k,resp,mydata,cols,infmat,par){
  #Run registerDoParallel(detectCores()) before this function
  #resp is a list of respondents
  #infmat is a matrix of information content
  #mydata is the data
  #cols are the columns on which the similarity analysis is done
  if(par==1){
  n<-length(resp)
  simVec<-vector()
  simVec<-foreach (i=1:n, .combine=c) %dopar% {
    simRes(k,i,mydata,cols,infmat)
  }
  }else{
    simVec<-vector()
    for(i in 1:length(resp)){
    simVec[i]<-simRes(k,i,mydata,cols,infmat)
    }
   }
  return(simVec)
}

