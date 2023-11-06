#Pre
sApre<-read.csv("CopenhagenFCIPre2010affiliation.csv")
studentPre<-as.character(sApre[,1])
questionPre<-names(sApre)[2:length(names(sApre))]
studentQuestionPre<-c(studentPre,questionPre)
comb<-matrix(data=0,nrow = length(sApre[1,])+length(sApre[,1])-1,ncol=length(sApre[1,])+length(sApre[,1])-1,)
colnames(comb)<-studentQuestionPre
a<-length(sApre[,1])+1
b<-length(comb[,1])
sApre<-as.matrix(sApre)
length(sApre[1,2:length(sApre[1,])])
for (i in 1:length(sApre[,1])){
  comb[i,a:b]<-sApre[i,2:length(sApre[i,])]
  }

library(igraph)
#color<-c(0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,2,0,0,0,0,2,0,0,2,1,0,2,0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0)
g<-graph.adjacency(comb,mode = "upper")
#Remove correct answers
correct<-c(which(V(g)$name=="X1c"),which(V(g)$name=="X2a"), which(V(g)$name=="X3c"),which(V(g)$name=="X4e"),which(V(g)$name=="X5b"),which(V(g)$name=="X6b"),
           which(V(g)$name=="X7b"),which(V(g)$name=="X8b"),which(V(g)$name=="X9e"),which(V(g)$name=="X10a"),which(V(g)$name=="X11d"),which(V(g)$name=="X12b"),
           which(V(g)$name=="X13d"),which(V(g)$name=="X14d"),which(V(g)$name=="X15a"),which(V(g)$name=="X16a"),which(V(g)$name=="X17b"),which(V(g)$name=="X18b"),
           which(V(g)$name=="X19e"),which(V(g)$name=="X20d"),which(V(g)$name=="X21e"),which(V(g)$name=="X22b"),which(V(g)$name=="X23b"),which(V(g)$name=="X24a"),
           which(V(g)$name=="X25c"),which(V(g)$name=="X26e"),which(V(g)$name=="X27c"),which(V(g)$name=="X28e"),which(V(g)$name=="X29b"),which(V(g)$name=="X30c"))
V(g)$rightAnswer<-0
V(g)$rightAnswer[correct]<-1
g<-delete.vertices(g,correct)
#1,C;2,A;3,C
#4,E 5,B 6,B 7,B 8,B 9,E 10,A 11,D
#12,B 13,D 14,D 15,A 16,A 17,B 18,B 19,E
#20,D 21,E 22,B 23,B 24,A 25,C 26,E 27,C 28,E
#29,B 30,C
#
bip<-bipartite.mapping(g)
V(g)$type<-bip$type
bip_g<-bipartite.projection(g)
student<-bip_g$proj1
question<-bip_g$proj2

V(student)$id<-V(student)$name

V(question)$id<-V(question)$name
#V(question)$color<-color

#write.graph(student,"student.net",format=c("pajek"))
#write.graph(question,"question.net",format=c("pajek"))
qPre_bb_10<-backboneNetwork(question,0.1,2)
#write.graph(question_bb,"question_bb10.net",format=c("pajek"))
qPre_bb_05<-backboneNetwork(question,0.05,2)
#write.graph(question_bb,"question_bb5.net",format=c("pajek"))
qPre_bb_01<-backboneNetwork(question,0.01,2)
#write.graph(question_bb,"question_bb1.net",format=c("pajek"))
qPre_bb_001<-backboneNetwork(question,0.001,2)
#write.graph(question_bb,"question_bb01.net",format=c("pajek"))
qPre_bb<-backboneNetwork(question,0.0001,2)
#write.graph(question_bb,"question_bb001.net",format=c("pajek"))


#write.csv(color,"color.csv")

#Post
sApost<-read.csv("CopenhagenFCIPost2010affiliation.csv")
studentPost<-as.character(sApost[,1])
questionPost<-names(sApost)[2:length(names(sApost))]
studentQuestionPost<-c(studentPost,questionPost)
comb<-matrix(data=0,nrow = length(sApost[1,])+length(sApost[,1])-1,ncol=length(sApost[1,])+length(sApost[,1])-1,)
colnames(comb)<-studentQuestionPost
a<-length(sApost[,1])+1
b<-length(comb[,1])
sApost<-as.matrix(sApost)
length(sApost[1,2:length(sApost[1,])])
for (i in 1:length(sApost[,1])){
  comb[i,a:b]<-sApost[i,2:length(sApost[i,])]
}

library(igraph)
#color<-c(0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,2,0,0,0,0,2,0,0,2,1,0,2,0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0)
g<-graph.adjacency(comb,mode = "upper")
correct<-c(which(V(g)$name=="X1c"),which(V(g)$name=="X2a"), which(V(g)$name=="X3c"),which(V(g)$name=="X4e"),which(V(g)$name=="X5b"),which(V(g)$name=="X6b"),
           which(V(g)$name=="X7b"),which(V(g)$name=="X8b"),which(V(g)$name=="X9e"),which(V(g)$name=="X10a"),which(V(g)$name=="X11d"),which(V(g)$name=="X12b"),
           which(V(g)$name=="X13d"),which(V(g)$name=="X14d"),which(V(g)$name=="X15a"),which(V(g)$name=="X16a"),which(V(g)$name=="X17b"),which(V(g)$name=="X18b"),
           which(V(g)$name=="X19e"),which(V(g)$name=="X20d"),which(V(g)$name=="X21e"),which(V(g)$name=="X22b"),which(V(g)$name=="X23b"),which(V(g)$name=="X24a"),
           which(V(g)$name=="X25c"),which(V(g)$name=="X26e"),which(V(g)$name=="X27c"),which(V(g)$name=="X28e"),which(V(g)$name=="X29b"),which(V(g)$name=="X30c"))
V(g)$rightAnswer<-0
V(g)$rightAnswer[correct]<-1
g<-delete.vertices(g,correct)
bip<-bipartite.mapping(g)
V(g)$type<-bip$type
bip_g<-bipartite.projection(g)
student<-bip_g$proj1
question<-bip_g$proj2

V(student)$id<-V(student)$name

V(question)$id<-V(question)$name
#V(question)$color<-color

#write.graph(student,"student.net",format=c("pajek"))
#write.graph(question,"question.net",format=c("pajek"))
qPost_bb_10<-backboneNetwork(question,0.1,2)
#write.graph(question_bb,"question_bb10.net",format=c("pajek"))
qPost_bb_05<-backboneNetwork(question,0.05,2)
#write.graph(question_bb,"question_bb5.net",format=c("pajek"))
qPost_bb_01<-backboneNetwork(question,0.01,2)
#write.graph(question_bb,"question_bb1.net",format=c("pajek"))
qPost_bb_001<-backboneNetwork(question,0.001,2)
#write.graph(question_bb,"question_bb01.net",format=c("pajek"))
qPost_bb<-backboneNetwork(question,0.0001,2)
#write.graph(question_bb,"question_bb001.net",format=c("pajek"))


#write.csv(color,"color.csv")

