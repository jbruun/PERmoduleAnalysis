entropy<-function(v){
  distr<-table(v)/sum(table(v)) #distribution
  H<--sum(distr*log2(distr))
  return(H)
  
}