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
V(g)[which(dd[1369:length(dd)]/1369>0.3)+1368] #This identifies the answer "T1Q13right"
g_cand<-delete.vertices(g,V(g)[which(dd[1369:length(dd)]/1369>0.3)+1368]) #This deletes "T1Q13right"

bip<-bipartite.projection(g_cand)
qs<-bip$proj1
V(qs)$id<-V(qs)$name
qsBB<-backboneNetwork(qs,0.02,2) #This turns out to be the desired alpha-level
is.connected(qsBB)
components(qsBB)
plot(degree.distribution(qsBB),log="xy") 
icl<-infomap.community(qsBB)
icl

#We seem to reach a nice level with a threshold of 0.3
#In this process, we identified a cluster which was always disconnected to the rest
#Apart from that cluster, the backbone is connected for this threshold and alpha level.
#Here, mod=0.62
#A plot allow us to see some of the structure as found by infomap in R
plot(qsBB,vertex.size=0.01*strength(qsBB),mark.groups = communities(icl),edge.width=0.01*E(qsBB)$weight)
communities(icl)
#This has removed 13 answers
#We can try to make the same type of network but only for answers with frequencies >0.3

V(g)[which(dd[1369:length(dd)]/1369<=0.3)+1368] #This identifies the answer "T1Q13right"
g_cand<-delete.vertices(g,V(g)[which(dd[1369:length(dd)]/1369<=0.3)+1368]) #This deletes "T1Q13right"

bip<-bipartite.projection(g_cand)
qs<-bip$proj1
V(qs)$id<-V(qs)$name
qsBB<-backboneNetwork(qs,0.06,2) #This turns out to be the desired alpha-level
is.connected(qsBB)
components(qsBB)
plot(degree.distribution(qsBB),log="xy") 
icl<-infomap.community(qsBB)
plot(qsBB,vertex.size=0.01*strength(qsBB),mark.groups = communities(icl),edge.width=0.01*E(qsBB)$weight)



