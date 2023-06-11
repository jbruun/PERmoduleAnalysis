#This document describes the implementation of the MAMCR procedure of Brewe, Bruun, and Bearden (2016)
#(henceforth BBB2016)
#on a refraction conceptual test adminitstered to 1366 students from different institutions.
#The MAMCR procedure is expanded/refined in a number of ways, which will be noted along the way.
#These analyses are done in R using the igraph package. 
#Comments that explain the code appear througout the document

#Preliminaries. Finding the right directory, loading package and reading in the data.
setwd("~/Dropbox/IND/Papers/RefractionTest")
library(igraph)
ref<-read.csv("refraction.csv")

#In BBB2016, we found that particular answers were very popular. They provided a very strong signal,
#which turned out to obscure the underlying community structure and had to be removed. In this
#first part of the current analysis, we try to find a threshold frequency for the selection of answers
#and then remove answers with frequencies above that threshold. To start out, we first
#create a bipartite network with students and answers begin the two types of nodes.

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

#Making the bipartite network
questions<-names(ref)
dimM<-length(stud)+length(questions)

AdjMat<-matrix(data=0,ncol=dimM,nrow=dimM)

colnames(AdjMat)<-c(stud,questions)
AdjMat[1:length(stud),(length(stud)+1):dimM]<-as.matrix(ref)
g<-graph.adjacency(AdjMat,mode = "upper") #This is the bipartite network

#The first 1366 nodes in g are students. The remaining nodes represent the quesions. 
#We now plot the frequencies of questions. 
dd<-degree(g)
V(g)$freq<-dd
plot(dd[1369:length(dd)]/1369)
#Some answers are chosen with a very high frequency, and we now search for 
#the threshold. We do this by running the following anlaysis until we
#can observe comunity structure in the questions. First, collapse the bipartite network 
#into a network of particular answers:

type<-vector()
type[1:length(stud)]<-1
type[(length(stud)+1):dimM]<-0
V(g)$type<-type
bip<-bipartite.projection(g)
qs<-bip$proj1
V(qs)$id<-V(qs)$name
#Overview of the network: It as 81 nodes and 3000 connections
qs
#Node names correspond to the individual answers to each task
#E.g. T1 represent task 1, and so on
V(qs)$name

#Now, we employ the Local Adaptive Network Sparsification (LANS) method by Foti et al. (2011) 
#to remove insignificant links. We aim choose the alpha-level so that the network is just connected;
#a lower alpha level would result in a disconnected network. 
qsBB<-backboneNetwork(qs,0.0411,2) #This turns out to be the desired alpha-level
is.connected(qsBB)

#overview of the backbone network: Still 81 nodes, but now only 283 connections.
qsBB
#However, the network degree distribution doesn't look like a power law (or it's a very sharp power
#with a weird tail).
plot(degree.distribution(qsBB),log="xy") 
#Infomap doesn't really find a modular structure:
infomap.community(qsBB)
#Notice that "mod:0.031". In order for us to claim that there is modular structure, we must obersve mod>0.3
#Also, most of the answers are grouped into a one big cluster:
table(infomap.community(qsBB)$membership)

#So now we iteratively remove answers with frequencies above a certain threshold
#We do this until we get community structure (mod>0.3)
dd<-degree(g)
V(g)[which(dd[1369:length(dd)]/1369>0.8)+1368] #This identifies the answer "T1Q13right"
g_cand<-delete.vertices(g,V(g)[which(dd[1369:length(dd)]/1369>0.8)+1368]) #This deletes "T1Q13right"

bip<-bipartite.projection(g_cand)
qs<-bip$proj1
V(qs)$id<-V(qs)$name
qsBB<-backboneNetwork(qs,0.045,2) #This turns out to be the desired alpha-level
is.connected(qsBB)
plot(degree.distribution(qsBB),log="xy") 
infomap.community(qsBB)
#Still no community structure. We continue...

dd<-degree(g)
tf<-0.3
V(g)[which(dd[1369:length(dd)]/1369>tf)+1368] #This identifies the answer "T1Q13right"
g_cand<-delete.vertices(g,V(g)[which(dd[1369:length(dd)]/1369>tf)+1368]) #This deletes "T1Q13right"

bip<-bipartite.projection(g_cand)
qs<-bip$proj1
V(qs)$id<-V(qs)$name
qsBB<-backboneNetwork(qs,0.045,2) #This turns out to be the desired alpha-level
is.connected(qsBB)
components(qsBB)
plot(degree.distribution(qsBB),log="xy") 
icl<-infomap.community(qsBB)
icl

#We seem to reach a nice level with a threshold of 0.4
#In this process, we identified a cluster which was always disconnected to the rest
#Apart from that cluster, the backbone is connected for this threshold and alpha level.
#Here, mod=0.62
#A plot allow us to see some of the structure as found by infomap in R
#Node size proportional to 1% of the sum of links
#link width proportional to 1% of weight of link
plot(qsBB,vertex.size=0.01*strength(qsBB),mark.groups = communities(icl),edge.width=0.01*E(qsBB)$weight)
communities(icl)

refNet<-qsBB #saving the network to another name
#This has removed 13 answers
#We can try to make the same type of network but only for answers with frequencies >0.3

V(g)[which(dd[1369:length(dd)]/1369<=tf)+1368] #This identifies the answer "T1Q13right"
g_cand<-delete.vertices(g,V(g)[which(dd[1369:length(dd)]/1369<=tf)+1368]) #This deletes "T1Q13right"

bip<-bipartite.projection(g_cand)
qs<-bip$proj1
V(qs)$id<-V(qs)$name
qsBB<-backboneNetwork(qs,0.06,2) #This turns out to be the desired alpha-level
is.connected(qsBB)
components(qsBB)
plot(degree.distribution(qsBB),log="xy") 
icp<-infomap.community(qsBB)
plot(qsBB,vertex.size=0.01*strength(qsBB),mark.groups = communities(icp),edge.width=0.01*E(qsBB)$weight)


#We now work with refNet as our standard network

#First of all, plot the degree distribution to see if it looks ok
plot(degree.distribution(refNet,cumulative=T),log="xy")
#This looks like the one I found for BBB16, so that's ok.

#Second, we look at the communities found by infomap. The names correspond to the 
#names in the file refraction.csv (which in turn is built on the data in 
#Originalcoded answers FINAL.xlsx). 
communities(icl)

#Now plot each group as a separete network
m1<-induced_subgraph(refNet,icl$membership==1)
plot(m1,vertex.size=0.03*strength(m1),edge.width=0.03*E(m1)$weight)
m2<-induced_subgraph(refNet,icl$membership==2)
plot(m2,vertex.size=0.03*strength(m2),edge.width=0.03*E(m2)$weight)
m3<-induced_subgraph(refNet,icl$membership==3)
plot(m3,vertex.size=0.1*strength(m3),edge.width=0.1*E(m3)$weight)
m4<-induced_subgraph(refNet,icl$membership==4)
plot(m4,vertex.size=0.03*strength(m4),edge.width=0.03*E(m4)$weight)
m5<-induced_subgraph(refNet,icl$membership==5)
plot(m5,vertex.size=0.03*strength(m5),edge.width=0.03*E(m5)$weight)
m6<-induced_subgraph(refNet,icl$membership==6)
plot(m6,vertex.size=0.1*strength(m6),edge.width=0.1*E(m6)$weight)
m7<-induced_subgraph(refNet,icl$membership==7)
plot(m7,vertex.size=0.1*strength(m7),edge.width=0.1*E(m7)$weight)
m8<-induced_subgraph(refNet,icl$membership==8)
plot(m8,vertex.size=0.03*strength(m8),edge.width=0.03*E(m8)$weight)
m9<-induced_subgraph(refNet,icl$membership==9)
plot(m9,vertex.size=0.03*strength(m9),edge.width=0.03*E(m9)$weight)
m10<-induced_subgraph(refNet,icl$membership==10)
plot(m10,vertex.size=0.3*strength(m10),edge.width=0.3*E(m10)$weight)
m11<-induced_subgraph(refNet,icl$membership==11)
plot(m11,vertex.size=0.03*strength(m11),edge.width=0.03*E(m11)$weight)
m12<-induced_subgraph(refNet,icl$membership==12)
plot(m12,vertex.size=0.03*strength(m12),edge.width=0.03*E(m12)$weight)
#Now run Infomap 1000 times and see how stable the solution is using R's builtin
#version of Infomap. In BBB16, we used Rosvall's newest version of Infomap in a 
#shell. That version produces more stable clusters

#NEW FUNCTION
#calculates infomap nit times and lists the results as a matrix
runIMbb<-function(g,nit){
  IM<-matrix(data=0,nrow=nit,ncol=vcount(g)+3)
  for(i in 1:nit){
    set.seed(i)
    a<-infomap.community(g,e.weights = E(g)$weight)
    mem<-a$membership
    mod<-a$modularity
    L<-a$codelength
    gr<-length(unique(a$membership))
    #size<-table(a$membership)
    #sz<-length(mem)+length(size)+3
    IM[i,]<-c(mem,mod,L,gr)
  }
  return(IM)
}

#a matrix that holds information about which items are grouped together
a<-runIMbb(refNet,1000)
b<-a[,(length(V(refNet))+1):length(a[1,])]
a<-a[,1:length(V(refNet))]
testo<-matrix(data=0,nrow=length(V(refNet)),ncol=length(V(refNet)))
for(i in 1:1000){
  testo<-testo+1*(outer(a[i,],a[i,],FUN="=="))
}

rownames(testo)<-V(refNet)$id
colnames(testo)<-V(refNet)$id

mm<-icl$membership
gr<-c(which(mm==1),which(mm==2),which(mm==3),which(mm==4),which(mm==5),which(mm==6),which(mm==7),which(mm==8),which(mm==9),which(mm==10),which(mm==11),which(mm==12))

block<-testo[gr,gr]

library(lattice)
rgb.palette <- colorRampPalette(c("blue", "red"), space = "rgb")
#As can be seen, the overlap is perfect.

levelplot(block/1000, main="Item co-module matrix", xlab="Item", ylab="Item", col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01))


######NEW IDEA######
###NOT FINISHED####
###DISREGARD FROM HERE####
#The next idea is to identify groups of students that "load" on each of the
#clusters found. First we create a similarity network of students.
#YOU DO NOT NEED TO UNDERSTAND THIS:
#x<-colSums(ref,na.rm=T)/length(stud)
#y<--log2(x)
#refSim<-ref
#for(i in 1:length(x)){
 # refSim[which(refSim[,i]==1),i]<-y[i]
  
#}

#simStudij<-function(i,j){
  #overlap<-ref[i,]*ref[j,]
  #Infoverlap<-sum(y[which(overlap==1)])
  #Infi<-sum(y[which(ref[i,]==1)])
  #Infj<-sum(y[which(ref[j,]==1)])
  #sim<-2*Infoverlap/(Infi+Infj)
  #return(sim)
  
#}

#simStudk<-function(k){
 # simVec<-vector()
  #for(i in 1:length(ref[,1])){
   # simVec[i]<-simStudij(k,i)
  #}
  #return(simVec)
#}

#simMatrix<-matrix(data=0,ncol=length(stud),nrow=length(stud))
#for(i in 1:length(stud)){
 # simMatrix[,i]<-simStudk(i)  
  
#}
#s<-graph.adjacency(simMatrix,diag=F,weighted=T)
#V(s)$id<-stud
#sBB<-backboneNetwork(s,0.05,2)
#length(stud)

#The network sBB is our student similarity network. 
#We now run infomap on this network to find groups
#ics<-infomap.community(sBB)

#For each group we now make a backbone question network

#NEW FUNCTIONS DEFINED
#makeBB<-function(ref,gnumber,lvl,tf){
 # samp<-ref[ics$membership==gnumber,]
#  questions<-names(samp)
#  stud<-as.character(samp[,1])
#  dimM<-length(stud)+length(questions)
#  AdjMat<-matrix(data=0,ncol=dimM,nrow=dimM)
#  colnames(AdjMat)<-c(stud,questions)
#  AdjMat[1:length(stud),(length(stud)+1):dimM]<-as.matrix(samp)
#  g<-graph.adjacency(AdjMat,mode = "upper") #This is the bipartite network
#  dd<-degree(g)
#  freq<-dd[length(stud)+1:length(dd)]/length(stud)
  #plot(dd[length(stud)+1:length(dd)]/length(stud),xlim=c(1,81))
#  type<-vector()
#  type[1:length(stud)]<-1
#  type[(length(stud)+1):dimM]<-0
#  V(g)$type<-as.logical(type)
  
#  deleted<-V(g)[which(dd[(length(stud)+1):length(dd)]/length(stud)>tf)+length(stud)] #This identifies the answer "T1Q13right"
#  g<-delete.vertices(g,V(g)[which(dd[(length(stud)+1):length(dd)]/length(stud)>tf)+length(stud)])
  
#  bip<-bipartite.projection(g)
#  qs<-bip$proj1
#  V(qs)$id<-V(qs)$name
#  qsBB<-backboneNetwork(qs,alpha=lvl,2) #This turns out to be the desired alpha-level
#  qsBB$connected<-is.connected(qsBB)
#  #plot(degree.distribution(qsBB),log="xy")
#  icBB<-infomap.community(qsBB)
#  qsBB$mod<-icBB$modularity
#  V(qsBB)$membership<-icBB$membership
#  result<-list(qsBB,icBB,deleted,freq[1:81])
  
#  return(result)
}
#Now make group based question backbone and get community structure
#qsBB1<-makeBB(ref,1,0.02,0.4)





