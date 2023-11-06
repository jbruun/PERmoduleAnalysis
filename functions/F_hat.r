F_hat<-function(Q){
  #THis is the Foti et al function for selecting links
  x<-vector()
  for(j in 1:length(Q)){
    x[j]<-length(which(Q!=0 & Q<=Q[j]))/length(which(Q>0))
  }
  return(x)
}

F_hatPar<-function(Q){
  #THis is the Foti et al function for selecting links
  #with parallel processing (remember the right incantations)
  #This will likely reduce computation significantly
  x<-vector()
  n<-length(Q)
  x<-foreach (i=1:n, .combine=c) %dopar% {
    length(which(Q!=0 & Q<=Q[i]))/length(which(Q>0))
  }
  return(x)
}
