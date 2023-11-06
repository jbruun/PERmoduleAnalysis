corItems<-function(k,bipmat){
corVec<-vector()
corVecP<-vector()
n<-dim(bipmat)[2]
for(i in 1:n){
  test<-cor.test(bipmat[,k],bipmat[,i])
  corVec[i]<-test$estimate
  corVecP[i]<-test$p.value
}  
result<-cbind(corVec,corVecP)
return(result)
}


