#This code part of the Supplementary Material for the paper 
#Bruun, J. & Brewe, E. (2013): Talking and learning physics: Predicting future grades from network measures and FCI pre-test scores, PRST-PER XXX
#Please cite this paper, if you use this code. 
#To use this script you need to install the R-environment (http://www.r-project.org/) 
#and download the additional package iGraph 0.6 (http://igraph.sourceforge.net/download.html)
#Questions and comments: jbruun@ind.ku.dk

#This script defines the Target Entropy function. 

library(igraph)
#To try out this function, load a network using for example: "dummy_RAW<-read.graph('dummy.net', format = c("pajek"))"
#To use the function, first load this script. Then write TargetEntropy(g), where g is the name of your network.  
#Depending on the size of your network and your computer, it may take som time to compute. 

########TARGET ENTROPY#########
TargetEntropy<-function(network){
TarEnt<-vector()
for (i in 1:length(V(network))){
	if (degree(network, v=i, mode=c("in"))<2) TarEnt[i]<-0
	else{
	TarEnt[i]<-targetEntropy(network,i)} 
	TarEnt<-TarEnt
	}
	return(TarEnt)
}

#This function inputs a network (g) and a vertex number (v) and outputs the target entropy of v. 
targetEntropy<-function(g,v){

sp<-get.all.shortest.paths(g, v, to = V(g)[V(g)!=v], mode = c("in"), weights=NA) # list of all paths - each path corresponds to a message
#sp holds the informtion about sources, shortest paths and in degree we need, so we proceed to make relevant vectors  from sp
if(length(sp$res)==0){TE<-0} #If no messages reach node v, then target entropy is set to 0

else {
sources<-sapply(sp$res,tail,1) #vector listing the source of each path (the first entry of each path in sp. If degenerate paths, then more entries
adjacentNodes<-sapply(sp$res, function(x) x[2]) #vector with last node of each path. This vector contains the nodes pointing at v. If n shortest paths pass through node w, then node w is listed n times.
weight=NULL #This will be a vector of the weights of each path. 
for(i in 1:length(sources)){weight[i]<-1/length(sources[sources==sources[i]])} #Each path is assigned a weight depending on the degeneracy of the path
a<-data.frame(adjacentNodes,weight)#A data frame with adjacent nodes and the weight of each path passing through adjacent nodes.
b<-unique(adjacentNodes) #The adjacent nodes to v without multiplicity
nMessage=NULL
#The average number of messages going through adjacent nodes
#nMessage[j] will be the average number of messages passing through neighbour j. 
for(j in 1:length(b)){nMessage[j]<-sum(a$weight[b[j]==a[,1]])} 
#This expression: "[b[j]==a[,1]]" means the entries in the first column of a which are equal to the j'th entry in b. 
#So a$weight[b[j]==a[,1]]) means the entries in a$weight which corresponds to those entries. 
#The fraction over all possible messages
fractions<-nMessage[!nMessage==0]/length(unique(sources)) 
TE<--sum(fractions*log2(fractions))
}
return(TE) #the targetEntropy for node v
}

