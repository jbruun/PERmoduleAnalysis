library(igraph)
responsesAll<-read.csv("data/refraction.csv")
studAtt<-read.csv("data/studentAttributes.csv",sep = ";")
#Removing irrelevant columns
responsesAll$Pre.question<-NULL
responsesAll$T2a1witoutfault<-NULL
responsesAll$T2a2witoutfault<-NULL
responsesAll$T2a3transfer<-NULL
responsesAll$T2a3witoutfault<-NULL
responsesAll$T2a4transfer<-NULL
responsesAll$T2a4witoutfault<-NULL
responsesAll$T3awithoutfault<-NULL
responsesAll$T3bwithoutfault<-NULL
responsesAll$Student<-NULL
above50<-which(colSums(responsesAll,na.rm=T)/1368>0.5)
responses<-responsesAll[-above50]

module1Names<-c("T1Q11right","T2a1e","T2a1f","T2a2e","T2a3d","T2a3f","T2a3g","T2a4d","T2a4e","T2a4f","T2bnon1","T2bP1","T2bP2","T2bP1P2","T2bP1P3","T3ai","T3aiii","T3bspearA","T3bspearD","T3blaserA","T3blaserD","T3cleft")
module1Pos <- match(module1Names, names(responses))
module2Names<-c("T1Q11left","T1Q11other","T1Q12right","T1Q13left","T1Q14right","T2a1d","T2a2d","T2a2f","T2a3a","T2a3e","T2a4c","T2a4g","T2bP3","T2bP2P3","T3aiv","T3bspearB","T3bspearE","T3blaserC","T3blaserE","T3cstraight","T3cright","T3cother")
module2Pos <- match(module2Names, names(responses))
module3Names<-c("T2a1a","T2a1b","T2a2a","T2a2b","T2a3b","T2a4b","T2bP1P2P3","T3bspearC","T3blaserB")
module3Pos <- match(module3Names, names(responses))
moduleOtherNames<-c("T2a1non","T2a2non","T2a3non","T2a4non","T2bnon2","T1Q11non","T1Q12non","T1Q13non","T1Q14non","T3anon","T3bspearnon","T3blasernon","T3cnon","T2a1c","T2a2c","T1Q12other","T1Q13other","T1Q124other","T2a3c","T2a4a")
moduleOtherPos <- match(moduleOtherNames, names(responses))

simNetBB<-read.graph("studentRefraction.net",format="pajek")
fgc<-fastgreedy.community(simNetBB)
V(simNetBB)$number<-c(1:1368)
N<-10000
fgcMat<-matrix(0,ncol = 1368,nrow=N)
for(i in 1:10000){
 fgcMat[i,]<-fastgreedy.community(simNetBB)$membership
  
}



meanSim<-vector()
sdSim<-vector()
N<-as.numeric(table(fgc$membership))

for(i in 1:length(unique(fgc$membership))){
  meanSim[i]<-mean(E(subgraph(simNet,fgc$membership==i))$weight)
  sdSim[i]<-sd(E(subgraph(simNet,fgc$membership==i))$weight)
  
}

meanSimIM<-vector()
sdSimIM<-vector()
N<-as.numeric(table(fgc$membership))

for(i in 1:length(unique(imc$membership))){
  meanSimIM[i]<-mean(E(subgraph(simNet,imc$membership==i))$weight)
  sdSimIM[i]<-sd(E(subgraph(simNet,imc$membership==i))$weight)
  
}

M<-length(unique(fgc$membership))
loadModule1<-vector()
loadModule2<-vector()
loadModule3<-vector()
loadModuleOther<-vector()

for(i in 1:M){
loadModule1[i]<-sum(responses[fgc$membership==i,module1Pos],na.rm=T)/sum(responses[fgc$membership==i,],na.rm=T)
loadModule2[i]<-sum(responses[fgc$membership==i,module2Pos],na.rm=T)/sum(responses[fgc$membership==i,],na.rm=T)
loadModule3[i]<-sum(responses[fgc$membership==i,module3Pos],na.rm=T)/sum(responses[fgc$membership==i,],na.rm=T)
loadModuleOther[i]<-sum(responses[fgc$membership==i,moduleOtherPos],na.rm=T)/sum(responses[fgc$membership==i,],na.rm=T)
}

loads<-data.frame(loadModule1,loadModule2,loadModule3,loadModuleOther)

source("functions/segregation27June2017.r")
ageSeg<-resampleX(fgc$membership,studAtt$age,1,1000)
courseSeg<-resampleX(fgc$membership,studAtt$Course,1,1000)
uniSeg<-resampleX(fgc$membership,studAtt$University,1,1000)
countrySeg<-resampleX(fgc$membership,studAtt$Country,1,1000)
majorSeg<-resampleX(fgc$membership,studAtt$code,1,1000)

seg<-data.frame(courseSeg,uniSeg,countrySeg,majorSeg,ageSeg)

ageSegTot<-resampleX(fgc$membership,studAtt$age,2,1000)
courseSegTot<-resampleX(fgc$membership,studAtt$Course,2,1000)
uniSegTot<-resampleX(fgc$membership,studAtt$University,2,1000)
countrySegTot<-resampleX(fgc$membership,studAtt$Country,2,1000)
majorSegTot<-resampleX(fgc$membership,studAtt$code,2,1000)


ageSegMax<-segMax(fgc$membership,studAtt$age,2,1000)
courseSegMax<-segMax(fgc$membership,studAtt$Course,2,1000)
uniSegMax<-segMax(fgc$membership,studAtt$University,2,1000)
countrySegMax<-segMax(fgc$membership,studAtt$Country,2,1000)
majorSegMax<-segMax(fgc$membership,studAtt$code,2,1000)

meanAge<-vector()
for(i in 1:M){
  meanAge[i]<-mean(studAtt$age[fgc$membership==i],na.rm=T)
  
}

groupCharac<-data.frame(N=as.numeric(table(fgc$membership)),loads,seg)
