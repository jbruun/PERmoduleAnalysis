####How much does a particular text load on a module/factor in the combined material?

loads<-function(k,graphs,mem,modules){
  #graph is a list of graphs, which has been used to make a combined graph
  #k is the number of the graph in the list of graphs
  #modules is a list of graphs, which represent modules found by community detection in the combined graph
  #mem is the membership vector for the community detection solution
  #Overlap of links and words
  linkweightOverlap<-vector()
  wordOverlap<-vector()
  #Sum of words and sum of link weights in each module
  sumLinkWeight<-vector()
  sumWords<-vector()
  
  for(i in 1:length(unique(mem))){
    linkweightOverlap[i]<-sum(E(graphs[[k]] %s% modules[[i]])$weight_1)
    #the %s% operator produces two weight attributes - one for each network
    #we use the weight of the first network here
    wordOverlap[i]<-length(which(V(graphs[[k]])$name%in%V(modules[[i]])$name))
    sumLinkWeight[i]<-sum(E(modules[[i]])$weight)
    sumWords[i]<-vcount(modules[[i]])
  }
  
  
  
  linkLoad<-linkweightOverlap/sumLinkWeight
  wordLoad<-wordOverlap/sumWords
  result<-data.frame(wordLoad,linkLoad,linkweightOverlap,sumLinkWeight,wordOverlap,sumWords)
  return(result)
}