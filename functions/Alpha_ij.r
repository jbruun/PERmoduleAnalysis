Alpha<-function(Q){
  #this is the disparity filter
  x<-vector()
  for(j in 1:length(Q)){
    x[j]<-(1-Q[j])**(length(which(Q>0))-1)
  }
  return(x)
}

AlphaPar<-function(Q){
  #this is the disparity filter
  #with parallel processing (remember the right incantations)
  #however, this will likely not produce reductions
  x<-vector()
  n<-length(Q)
  x<-foreach (i=1:n, .combine=c) %dopar% {
    (1-Q[i])**(length(which(Q>0))-1)
  }
  return(x)
}
