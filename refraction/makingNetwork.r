#making bipartite network from csv
setwd("~/Dropbox/Papers/RefractionTest")
library(igraph)
ref<-read.csv("refraction.csv")

stud<-as.character(ref[,1])
ref$Student<-NULL
#Removing irrelevant columns
ref$Pre.question<-NULL
ref$T2a1witoutfault<-NULL
ref$T2a2witoutfault<-NULL
ref$T2a3transfer<-NULL
ref$T2a3witoutfault<-NULL
ref$T2a4transfer<-NULL
ref$T2a4witoutfault<-NULL
ref$T3awithoutfault<-NULL
ref$T3bwithoutfault<-NULL

#Removing questionts, which are too easy
#ref$T1Q13right<-NULL
#ref$T1Q11straight<-NULL
#ref$T1Q12left<-NULL
#ref$T1Q14left<-NULL
#ref$T3aii<-NULL

questions<-names(ref)
dimM<-length(stud)+length(questions)

AdjMat<-matrix(data=0,ncol=dimM,nrow=dimM)

colnames(AdjMat)<-c(stud,questions)
AdjMat[1:length(stud),(length(stud)+1):dimM]<-as.matrix(ref)
g<-graph.adjacency(AdjMat,mode = "upper")

type<-vector()
type[1:length(stud)]<-1
type[(length(stud)+1):dimM]<-0
V(g)$type<-type
bip<-bipartite.projection(g)
qs<-bip$proj1
st<-bip$proj2
V(qs)$id<-V(qs)$name

#Backbone extraction Questions

dd<-degree(g)

qsBB<-backboneNetwork(qs,0.02,2)


#Analysis


plot(dd[1369:length(dd)]/1369)


x<-colSums(ref,na.rm=T)/length(stud)
y<--log2(x)
refSim<-ref
for(i in 1:length(x)){
  refSim[which(refSim[,i]==1),i]<-y[i]
  
}

simStudij<-function(i,j){
  overlap<-ref[i,]*ref[j,]
  Infoverlap<-sum(y[which(overlap==1)])
  Infi<-sum(y[which(ref[i,]==1)])
  Infj<-sum(y[which(ref[j,]==1)])
  sim<-2*Infoverlap/(Infi+Infj)
  return(sim)

}

simStudk<-function(k){
  simVec<-vector()
  for(i in 1:length(ref[,1])){
    simVec[i]<-simStudij(k,i)
  }
  return(simVec)
}

simMatrix<-matrix(data=0,ncol=length(stud),nrow=length(stud))
for(i in 1:length(stud)){
simMatrix[,i]<-simStudk(i)  
  
}
write.csv(simMatrix,"simMatrix.csv")
sm<-read.csv("simMatrix.csv")
simMatrix<-as.matrix(sm[,-1])
s<-graph.adjacency(simMatrix,diag=F,weighted=T)
V(s)$id<-stud
sBB<-backboneNetwork(s,0.05,2)
length(stud)
