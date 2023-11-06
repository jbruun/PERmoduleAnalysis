simRes<-function(i,j,mydata,cols,infmat){
  #calculates the similarity of two respondents
  #based on information measure (REF)
  y<-infmat[i,]
  overlap<-sum(y[which(mydata[i,cols]==mydata[j,cols])],na.rm=T)
  sinfi<-sum(infmat[i,],na.rm=T)
  sinfj<-sum(infmat[j,],na.rm=T)
  sim<-2*overlap/(sinfi+sinfj)
  return(sim)
}