correctFCI<-function(g){
correct<-c(which(V(g)$name=="I1c"),which(V(g)$name=="I2a"), which(V(g)$name=="I3c"),which(V(g)$name=="I4e"),which(V(g)$name=="I5b"),which(V(g)$name=="I6b"),
           which(V(g)$name=="I7b"),which(V(g)$name=="I8b"),which(V(g)$name=="I9e"),which(V(g)$name=="I10a"),which(V(g)$name=="I11d"),which(V(g)$name=="I12b"),
           which(V(g)$name=="I13d"),which(V(g)$name=="I14d"),which(V(g)$name=="I15a"),which(V(g)$name=="I16a"),which(V(g)$name=="I17b"),which(V(g)$name=="I18b"),
           which(V(g)$name=="I19e"),which(V(g)$name=="I20d"),which(V(g)$name=="I21e"),which(V(g)$name=="I22b"),which(V(g)$name=="I23b"),which(V(g)$name=="I24a"),
           which(V(g)$name=="I25c"),which(V(g)$name=="I26e"),which(V(g)$name=="I27c"),which(V(g)$name=="I28e"),which(V(g)$name=="I29b"),which(V(g)$name=="I30c"))
return(correct)
}