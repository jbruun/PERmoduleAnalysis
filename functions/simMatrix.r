simMatrix<-function(mydata,cols,par){
  n<-dim(mydata)[1]
  resp<-paste("R",c(1:n),sep="")
  infMat<-infmat(mydata,cols)
  simMatrix<-matrix(data=0,ncol=n,nrow=n)
  #for(i in 1:n){
  #  simMatrix[,i]<-simResVec(i,resp,mydata,cols,infMat,par)
  #}
    colnames(simMatrix)<-resp
    rownames(simMatrix)<-resp
  return(simMatrix)
}






#simMatrix<-matrix(data=0,ncol=length(samletNy[,1]),nrow=length(samletNy[,1]))
#for(i in 1:length(samletNy[,1])){
#  simMatrix[,i]<-simResk(i,samletNy[,1],samletNy[,8:19])  
  
#}

#t1<-Sys.time()
#simResVec(1,resp,fci_data,cols,test)
#t2<-Sys.time()

#t2-t1
  