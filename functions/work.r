#This script creates a self-efficacy similarity network
setwd("E:/Dropbox/IND/ASSIST-ME/Self-Efficacy Networks/")

mydata<-read.csv("dataset.csv",sep=",")
mydata$X<-c(1:length(mydata$Name))
mydata[is.na(mydata)] <- 0

library(igraph)

#Respondent similarity network

resp<-paste("R",c(1:length(mydata$X)),sep="")
probs<-function(mydata,n){
  a<-as.numeric(names(table(mydata[,n])))
  p<-as.numeric(table(mydata[,n])/length(resp))
  x<-c(1:length(resp))
  for(i in 1:length(a)){
    x[which(mydata[,n]==a[i])]<-p[i]
  }
  return(x)
}

names(mydata[2:13]) #Columns for pre
names(mydata[14:25]) #Columns for post
pre<-mydata[2:13]
post<-mydata[14:25]

pmat<-function(data){
pmat<-matrix(0,ncol=length(data),nrow=length(data[,1]))
for(j in 1:length(data)){
  pmat[,j]<-probs(data,j)  
  
}
infmat<--log2(pmat)
return(infmat)
}

simRes<-function(i,j,infmat,d){
  y<-infmat[i,]
  overlap<-sum(y[which(d[i,1:12]==d[j,1:12])])
  sinfi<-sum(infmat[i,])
  sinfj<-sum(infmat[j,])
  sim<-2*overlap/(sinfi+sinfj)
  return(sim)
}

simResk<-function(k,inf,d){
    simVec<-vector()
  for(i in 1:length(resp)){
    simVec[i]<-simRes(k,i,inf,d)
  }
  return(simVec)
}

simMatrix<-function(d){
  inf<-pmat(pre)
  similarityMatrix<-matrix(data=0,ncol=length(d[,1]),nrow=length(d[,1]))
  for(i in 1:length(resp)){
    similarityMatrix[,i]<-simResk(i,inf,d)  
    
  }
  return(similarityMatrix)
}

preSim<-simMatrix(pre)
postSim<-simMatrix(post)

preNet<-graph.adjacency(preSim,diag=F,weighted=T)
postNet<-graph.adjacency(postSim,diag=F,weighted=T)

V(preNet)$id<-resp
V(postNet)$id<-resp

preNetBB<-backboneNetwork(preNet,0.015,2)
postNetBB<-backboneNetwork(postNet,0.015,2)
write.graph(preNetBB,"preNetBB.net",format=c("pajek"))
write.graph(postNetBB,"postNetBB.net",format=c("pajek"))


#Question Similarity Network

probsQ<-function(mydata,n){
  a<-as.numeric(names(table(as.numeric(mydata[n,1:12]))))
  p<-table(as.numeric(mydata[n,1:12]))/12
  x<-c(1:12)
  for(i in 1:length(a)){
    x[which(mydata[n,1:12]==a[i])]<-p[[i]]
  }
  return(x)
}

names(pre[1:12])
pmatQ<-matrix(0,ncol=93,nrow=12)
for(j in 1:93){
  pmatQ[,j]<-probsQ(post,j)  
  
}
infmatQ<--log2(pmatQ)
md<-t(post[1:12])

simResQ<-function(i,j){
  y<-infmatQ[i,]
  overlap<-sum(y[which(md[i,]==md[j,])]) 
  sinfi<-sum(infmatQ[i,])
  sinfj<-sum(infmatQ[j,])
  sim<-2*overlap/(sinfi+sinfj)
  return(sim)
}

simReskQ<-function(k){
  simVec<-vector()
  for(i in 1:12){
    simVec[i]<-simResQ(k,i)
  }
  return(simVec)
}

simMatrixQ<-matrix(data=0,ncol=12,nrow=12)
for(i in 1:12){
  simMatrixQ[,i]<-simReskQ(i)  
  
}


preQ<-graph.adjacency(simMatrixQ,diag=F,weighted=T,mode = "undirected")
postQ<-graph.adjacency(simMatrixQ,diag=F,weighted=T,mode = "undirected")
V(preQ)$id<-names(mydata[2:13])
V(postQ)$id<-names(mydata[14:25])

preQBB<-backboneNetwork(preQ,0.05,2)
postQBB<-backboneNetwork(postQ,0.05,2)
write.graph(preQBB,"preQBB.net",format=c("pajek"))
write.graph(postQBB,"postQBB.net",format=c("pajek"))
