#This document describes the implementation of the MAMCR procedure of Brewe, Bruun, and Bearden (2016)
#(henceforth BBB2016)
#on a refraction conceptual test adminitstered to 1366 students from different institutions.
#The MAMCR procedure is expanded/refined in a number of ways, which will be noted along the way.
#These analyses are done in R using the igraph package. 
#Comments that explain the code appear througout the document

#Preliminaries. Finding the right directory, loading package and reading in the data.

library(igraph)
source("functions/backboneExtraction.R")
ref<-read.csv("data/refraction.csv")
studAtt<-read.csv("data/studentAttributes.csv")
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
g<-graph.adjacency(AdjMat) #This is the bipartite network
V(g)$id<-V(g)$name
write.graph(g,"bipartiteRefraction.net",format="pajek")

#The first 1366 nodes in g are students. The remaining nodes represent the questions. 
#We now plot the frequencies of questions. 
dd<-degree(g)
V(g)$freq<-dd
plot(dd[1369:length(dd)]/1369, ylab="selection frequency",xlab="item number")
abline(h=0.5)
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
qsBB<-backboneNetwork(qs,0.001,2) #This turns out to be the desired alpha-level
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
V(g)[which(dd[1369:length(dd)]/1369>0.5)+1368] #This identifies the answer "T1Q13right"
g_cand<-delete.vertices(g,V(g)[which(dd[1369:length(dd)]/1369>0.5)+1368]) #This deletes "T1Q13right"

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

#alpha-level determination function
findAlpha<-function(ref,j,iter){#ref =ref, j=alpha, iter= iterations
N_M<-vector()
largest<-vector()
mod<-vector()
L<-vector()
cl<-vector()
n2<-vector()
n3<-vector()
con<-vector()
for (i in 1:iter){
bab<-makeBB(ref,sampSize = length(ref[,1]),i*(0.1/iter),j)
ic<-bab[[2]]
N_M[i]<-length(unique(ic$membership))
largest[i]<-max(table(ic$membership))
mod[i]<-ic$modularity
L[i]<-length(E(bab[[1]]))
cl[i]<-ic$codelength
n2[i]<-length(which(table(ic$membership)<=2))/length(table(ic$membership))
n3[i]<-length(which(table(ic$membership)<=3))/length(table(ic$membership))
con[i]<-as.numeric(is.connected(bab[[1]]))
}
result<-data.frame(N_M,largest,mod,L,cl,n2,n3,con)
return(result)
}
fA09<-findAlpha(0.9,1000)
par(mfrow=c(2,2))
plot(x,fA09$mod,main="Modularity", xlab="alpha")
plot(x,fA09$n3,main="Fraction of duos and trios", xlab="alpha")
plot(x,fA09$cl,main="Minimum description length", xlab="alpha")
plot(x,fA09$largest,main="Nodes in largest cluster", xlab="alpha")

fA08<-findAlpha(0.8,1000)
par(mfrow=c(2,2))
plot(x,fA08$mod,main="Modularity", xlab="alpha")
plot(x,fA08$n3,main="Fraction of duos and trios", xlab="alpha")
plot(x,fA08$cl,main="Minimum description length", xlab="alpha")
plot(x,fA08$largest,main="Nodes in largest cluster", xlab="alpha")
fA07<-findAlpha(0.7,1000)
par(mfrow=c(2,2))
plot(x,fA07$mod,main="Modularity", xlab="alpha")
plot(x,fA07$n3,main="Fraction of duos and trios", xlab="alpha")
plot(x,fA07$cl,main="Minimum description length", xlab="alpha")
plot(x,fA07$largest,main="Nodes in largest cluster", xlab="alpha")
fA06<-findAlpha(0.6,1000)

plot(x,fA06$mod,main="Modularity", xlab="alpha")
plot(x,fA06$n3,main="Fraction of duos and trios", xlab="alpha")
plot(x,fA06$cl,main="Minimum description length", xlab="alpha")
plot(x,fA06$largest,main="Nodes in largest cluster", xlab="alpha")
fA05<-findAlpha(0.5,1000)
plot(x,fA05$mod,main="Modularity", xlab="alpha")
plot(x,fA05$n3,main="Fraction of duos and trios", xlab="alpha")
plot(x,fA05$cl,main="Minimum description length", xlab="alpha")
plot(x,fA05$largest,main="Nodes in largest cluster", xlab="alpha")
fA04<-findAlpha(0.4,1000)
plot(x,fA04$mod,main="Modularity", xlab="alpha")
plot(x,fA04$n3,main="Fraction of duos and trios", xlab="alpha")
plot(x,fA04$cl,main="Minimum description length", xlab="alpha")
plot(x,fA04$largest,main="Nodes in largest cluster", xlab="alpha")
fA03<-findAlpha(0.3,1000)
plot(x,fA03$mod,main="Modularity", xlab="alpha")
plot(x,fA03$n3,main="Fraction of duos and trios", xlab="alpha")
plot(x,fA03$cl,main="Minimum description length", xlab="alpha")
plot(x,fA03$largest,main="Nodes in largest cluster", xlab="alpha")
fA02<-findAlpha(0.2,1000)
plot(x,fA02$mod,main="Modularity", xlab="alpha")
plot(x,fA02$n3,main="Fraction of duos and trios", xlab="alpha")
plot(x,fA02$cl,main="Minimum description length", xlab="alpha")
plot(x,fA02$largest,main="Nodes in largest cluster", xlab="alpha")
fA01<-findAlpha(0.1,1000)
plot(x,fA01$mod,main="Modularity", xlab="alpha")
plot(x,fA01$n3,main="Fraction of duos and trios", xlab="alpha")
plot(x,fA01$cl,main="Minimum description length", xlab="alpha")
plot(x,fA01$largest,main="Nodes in largest cluster", xlab="alpha")
fA005<-findAlpha(0.05,1000)
plot(x,fA005$mod,main="Modularity", xlab="alpha")
plot(x,fA005$n3,main="Fraction of duos and trios", xlab="alpha")
plot(x,fA005$cl,main="Minimum description length", xlab="alpha")
plot(x,fA005$largest,main="Nodes in largest cluster", xlab="alpha")

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
#This has removed 13 items
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

a<-IM
length(a[,1])
testo<-matrix(data=0,nrow=length(a[1,]),ncol=length(a[1,]))
for(i in 1:length(a[,1])){
  testo<-testo+1*(outer(a[i,],a[i,],FUN="=="))
}

rownames(testo)<-V(f90)$id
colnames(testo)<-V(f90)$id

#mm<-icl$membership
mm<-icl$membership
table(a[1,])
mm<-a[2,]
#gr<-c(which(mm==1),which(mm==2),which(mm==3),which(mm==4),which(mm==5),which(mm==6),which(mm==7),which(mm==8),which(mm==9),which(mm==9))
gr<-c(which(mm==1),which(mm==2),which(mm==3),which(mm==4),which(mm==5),which(mm==6),which(mm==7),which(mm==8),which(mm==9),which(mm==10),which(mm==11),which(mm==12),which(mm==13),which(mm==14),which(mm==15),which(mm==16))
block<-testo[gr,gr] 

library(lattice) 
rgb.palette <- colorRampPalette(c("blue", "red"), space = "rgb")
#As can be seen, the overlap is perfect.

levelplot(block/length(a[,1]), main="Item co-module matrix", xlab="Item", ylab="Item", col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01))
compare(a[1,],f50[[2]]$membership,method="nmi")
nmi<-vector()
for(i in 1:length(a[,1])){
  nmi[i]<-compare(a[i,],f50[[2]]$membership,method = "nmi")
}
#nmim<-vector()
#nmisd<-vector()
#n=100
nmim[1]<-mean(nmi)
nmisd[1]<-sd(nmi)

#n=200
nmim[2]<-mean(nmi)
nmisd[2]<-sd(nmi)
#n=300
nmim[3]<-mean(nmi)
nmisd[3]<-sd(nmi)
#n=400
nmim[4]<-mean(nmi)
nmisd[4]<-sd(nmi)
#n=500
nmim[5]<-mean(nmi)
nmisd[5]<-sd(nmi)
#n=600
nmim[6]<-mean(nmi)
nmisd[6]<-sd(nmi)
#n=700
nmim[7]<-mean(nmi)
nmisd[7]<-sd(nmi)

#n=800
nmim[8]<-mean(nmi)
nmisd[8]<-sd(nmi)
#n=900
nmim[9]<-mean(nmi)
nmisd[9]<-sd(nmi)

#n=1000
nmim[10]<-mean(nmi)
nmisd[10]<-sd(nmi)

#n=1100
nmim[11]<-mean(nmi)
nmisd[11]<-sd(nmi)

#n=1200
nmim[12]<-mean(nmi)
nmisd[12]<-sd(nmi)

#n=1300
nmim[13]<-mean(nmi)
nmisd[13]<-sd(nmi)

#n=1350
nmim[14]<-mean(nmi)
nmisd[14]<-sd(nmi)


#n=1368
nmim[15]<-mean(nmi)
nmisd[15]<-sd(nmi)

######NEW IDEA######
###NOT FINISHED####
###DISREGARD FROM HERE####
#The next idea is to identify groups of students that "load" on each of the
#clusters found. First we create a similarity network of students.
#YOU DO NOT NEED TO UNDERSTAND THIS:

n_cores<-detectCores()-1 #this is the number of cores we'll use
cl <- makeCluster(n_cores)  
registerDoParallel(cl)   #use doParallel package to engage the cores we want to use

n<-length(stud)
SimMatRef<-matrix(0,nrow=n,ncol=n)
diag(SimMatRef)<-1
time<-vector()

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
## PARALLEL
SimMat<-matrix(0,nrow=n,ncol=n)
time<-vector()
for(k in 1:n-1){
  ptm<-proc.time()
  simVec<-vector()
  
  
  simVec<-foreach (i=(k+1):n, .combine=c) %dopar% {
    simStudij(k,i)
  }
  
  SimMat[(k+1):n,k]<-simVec
  time[k]<-(proc.time()-ptm)[3]
  time[k]
}


simStudk<-function(k){
  simVec<-vector()
  for(i in 1:length(ref[,1])){
    simVec[i]<-simStudij(k,i)
  }
  return(simVec)
}

time<-vector()
ptm<-proc.time()
simStudk(1)
time[1]<-(proc.time()-ptm)[3]
time[1]


simMatrix<-matrix(data=0,ncol=length(stud),nrow=length(stud))
for(i in 1:length(stud)){
  simMatrix[,i]<-simStudk(i)  
  
}


write.csv(simMatrix,"simMatrix.csv")
sm<-read.csv("simMatrix.csv")
simMatrix<-as.matrix(sm[,-1])
s<-graph.adjacency(SimMat,diag=F,weighted=T)

source("functions/F_hat.r")
timeL<-vector()
LANSMatRef<-matrix(0,nrow=n,ncol=n)
for(k in 1:n){
  ptm<-proc.time()
  LANSMatRef[,k]<-F_hatPar(SimMat[,k])
  timeL[k]<-(proc.time()-ptm)[3]
  print(c(k,timeL[k]))
}

write.csv(LANSMatRef,"LANSMatRef.csv")

alpha<-0.001
sigMat<-LANSMatRef >= 1 - alpha
mode(sigMat)<-"numeric"
sigMat[is.na(sigMat)] <- 0
SimMatBB<-sigMat*SimMat

sBB<-graph.adjacency(SimMatBB,diag=F,weighted=T,mode = "lower")
V(sBB)$id<-stud
V(s)$id<-stud
sBB<-backboneNetwork(s,0.05,2)
length(stud)
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
#}
#Now make group based question backbone and get community structure
#qsBB1<-makeBB(ref,1,0.02,0.4)



###ANALYSIS OF CORRECT ANSWERS###
names(ref)
correct<-c(2,6,13,16,24,31,35,36,41,44,45,47,58,61,62,67,72,79)
all<-c(1:81)

bip<-bipartite.projection(g)
ca<-bip$proj1
ca<-induced.subgraph(ca,correct)
V(ca)$id<-V(ca)$name
caBB<-backboneNetwork(ca,0.05,2) #This turns out to be the desired alpha-level
is.connected(qsBB)
plot(degree.distribution(qsBB),log="xy") 
infomap.community(qsBB)



###TESTING DIFFERENT UNIVERSITIES
fArut<-findAlpha(test,0.3,1000)
par(mfrow=c(2,2))

plot(x,fA05$mod,main="Modularity", xlab="alpha")
plot(x,fA05$n3,main="Fraction of duos and trios", xlab="alpha")
plot(x,fA05$N_M,main="Number of clusters", xlab="alpha")
plot(x,fA05$largest,main="Nodes in largest cluster", xlab="alpha")

###Finding distance of particular cohort to ground truth and comparing to random
iter<-100
dist1<-vector()
dist2<-vector()
for (i in 1:iter){
  bb<-makeBB(ref,207,0.025,0.5)
  dist1[i]<-Distance(bb[[1]],rutBB[[1]])
  dist2[i]<-Distance(bb[[1]],f50[[1]])
}

