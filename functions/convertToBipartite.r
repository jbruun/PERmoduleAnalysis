convertToBipartite<-function(mydata,q,subSet){
  #q is vector. length of q is # questions. 
  #Value of each entry is # possible responses to each question
  #subSet is the subset of questions used to establish similarity
  cols<-q[subSet]
  n<-dim(mydata)[1]
  m<-sum(q[subSet])
  bipMat<-matrix(data=0,ncol=m,nrow=n)
  items<-list()
  for(i in 1:length(subSet)){
  items[[i]]<-paste("I",subSet[i],".",c(1:q[[subSet[i]]]),sep="")
  }
  items<-unlist(items)
  resp<-paste("R",c(1:n),sep="")  
  colnames(bipMat)<-c(items)
  rownames(bipMat)<-c(resp)
  #POPULATE bibMat!!
  
  return(bipMat)
  
}
myLetters <- names(table(fci_data[,21]))
match(fci_data[1,21], myLetters)

test<-matrix(0,nrow=10,ncol=5)

test[1:10,match(fci_data[1:10,21], myLetters)]<-1
q<-c(rep(5,30))
