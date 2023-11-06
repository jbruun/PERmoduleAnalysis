#Bruun, J. & Brewe, E. (2013): Talking and learning physics: Predicting future grades from network measures and FCI pre-test scores, PRST-PER XXX
#Please cite this paper, if you use this code. 
#To use this script you need to install the R-environment (http://www.r-project.org/) 
#and download the additional package iGraph 0.6 (http://igraph.sourceforge.net/download.html)
#Questions and comments: jbruun@ind.ku.dk

#This script defines a set of functions used to calculate the search information in a network. 

library(igraph)
#To try out this function, load a network using for example: "dummy_RAW<-read.graph('dummy.net', format = c("pajek"))"
#To use the function, first load this script. Then write e.g. "SI<-sInfMatrix(g)", where g is the name of your network.
#SI is then the search information matrix. Summing the columns will yield the Hide (summing the rows will yield the related measure Access.  
#Depending on the size of your network and your computer, it may take som time to compute. 

########SEARCH INFORMATION######
 

searchInformation<-function(g,n,m,W){
if(m==n) SI<-0
else{
 sp<-get.all.shortest.paths(g,from=n,m,mode=c("out"),weights=NA) #get list of all shortest path from node i to node j
 pathMatrix<-sp$res
 if(length(pathMatrix)==0) SI<-NA
	else{

   	if(n==m){return(0)}
   	 else{
    	x<-nominator(W,do.call(rbind,pathMatrix))
    	y<-denominator(g,do.call(rbind,pathMatrix))
   	 a<-x/y
   	   SI<--log2(sum(a))
  	  }
   
    }
}    
     return(SI)
    }
    
weightedAdjacency<-function(g){
#weighted adjacency matrix. Entry [i,j] corresponds to the weight of the link from node i to node j.
W<-get.adjacency(g,type=c("both"),attr=NULL) 
return(W)}

denominator<-function(g,pathMatrix){
#creates a vector with values i= the denominator in the probabilities
den<-vector()
#Make demoninator vector  
for(i in 1:length(pathMatrix[,1])){den[i]<-prod(graph.strength(g,vids=pathMatrix[i,-length(pathMatrix[i,])], mode=c("out"),weights=NULL))} 
return(den)
}

nominator<-function(W,pathMatrix){
nom<-vector()
x<-vector()
       for(j in 1:length(pathMatrix[,1])){
     
         for(i in 1:length(pathMatrix[1,])-1){
           x[i]<-W[pathMatrix[j,i],pathMatrix[j,i+1]]
         }  
         nom[j]<-prod(x)
       }
return(nom)
}


sInfMatrix<-function(g){
g<-simplify(g)
W<-weightedAdjacency(g)
iota<-data.frame()

for(l in 1:length(V(g))){
  for(m in 1:length(V(g))){
  iota[l,m]<-searchInformation(g,l,m,W) 
  }
}
return(iota)
}  

