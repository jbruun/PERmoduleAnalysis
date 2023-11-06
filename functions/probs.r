probs<-function(mydata,n){
  #this function creates a vector 
  a<-names(table(mydata[,n])) 
  p<-table(mydata[,n])/sum(table(mydata[,21]))
  x<-vector()
  for(i in 1:length(a)){
    x[which(mydata[,n]==a[i])]<-p[i]
  }
  return(x)
}