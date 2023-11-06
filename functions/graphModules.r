
graphModules<-function(combinedNet,mem){
  #makes networks based on a community detection solution on a network
modGraphs<-list()
for(i in 1:length(unique(mem))){
  modGraphs[[i]]<-induced.subgraph(combinedNet,vids=V(combinedNet)[mem==i])
}
return(modGraphs)
}