setwd("~/OneDrive - K??benhavns Universitet/DGS_2015_karakter/Baggrundsmateriale/analyzed")
library(psych)
data15<-read.csv("DGS2015raa.csv",sep=";")
data17<-read.csv("DGS2017raa.csv",sep=";")
describe(data15)
#DATASET from 2015

date<-as.Date(data15$Dato)
plot(table(as.Date(data15$Dato))[1:30]/length(data15$Dato)) #Most of the answers came within a week
time<-xxx(data15$Dato) # GET LIBRARY LUBRIDATE FOR THIS

koen<-data15$X2.Hvilket.k.n.er.du. #1 er hank??n, 2 er hunk??n og 3 er andet
alder<-data15$X3.Hvad.er.din.alder. #
aargang<-data15$X4.Hvilken..rgang.g.r.du.p..
studRet<-data15$X5.Hvilken.studierretning.g.r.du.p..
foraelUdd<-data15$X6.Hvad.er.den.h.jeste.gennemf.rte.uddannelse.blandt.dine.for.ldre.
foraelSki<-data15$X8.Er.dine.for.ldre.skilt.bor.hver.for.dig.
foraelDK<-data15$X9.Er.dine.for.ldre.f.dt.i.Danmark.

baggrund<-data.frame(koen,alder,aargang,studRet,foraelUdd,foraelSki,foraelDK)



#karaktersp??rgsm??l
betydPerson<-data15$X14.I.hvilken.grad.har.dine.karakterer.betydning.for.dig.
betydStudRet<-data15$X15.I.hvilken.grad.har.karakterer.haft.betydning.for.dit.valg.af.studieretning.
bestemHaandOp<-data15$X16.I.hvilken.grad.er.karakterer.bestemmende.for..om.du.r.kker.h.nden.op.i.klasseundervisningen.
undHaandOpFrygt<-data15$X17.Har.du.oplevet.at.undlade.at.r.kke.h.nden.op.af.frygt.for.at.sige.noget.forkert.
undHaandOpFrygtFreq<-data15$X18.Hvor.ofte.har.du.undladt.at.r.kke.h.nden.op.af.frygt.for.at.sige.noget.forkert.
FrygtGrMedelever<-data15$X19.1.Min.medelevers.opfattelse.af.mig
FrygtGrKarakter<-data15$X19.2.Af.frygt.for.at.det.p.virker.mine.karakterer
FrygtGrForb<-data15$X19.3.Jeg.f.ler.mig.ikke.velforberedt
FrygtGrIkkeProb<-data15$X19.4.Det.er.ikke.et.problem
FrygtGrAndet<-data15$X19.5.Andet
MestAf<-data15$X20.Hvad.f.r.du.mest.ud.af..n.r.du.f.r.afleveringer.tilbage.
MotSkole<-data15$X21.Hvilket.af.f.lgende.giver.dig.st.rst.motivation.til.at.m.de.i.skole.
###QUESTIONS ABOUT ABSENSE AND SICKNESS
sygISkoleResp<-data15$X10.Hvor.ofte.m.der.du.i.skole..hvis.du.er.syg.
sygIskoleKlas<-data15$X11.Hvor.ofte.m.der.dine.klassekammerater.syge.i.skole.
sygeEleverLaerMilj<-data15$X12.F.ler.du..at.syge.elever.i.undervisningen.p.virker.l.ringsmilj.et.
sygISkoleUndgaaFravaer<-data15$X13.Hvor.enig.er.du.i.udsagnet...Jeg.er.m.dt.syg.i.skole.for.at.undg..frav.r..
fravSyg<-data.frame(sygISkoleResp,sygIskoleKlas,sygeEleverLaerMilj,sygISkoleUndgaaFravaer)

#####
karFak<-data.frame(betydPerson,betydStudRet,bestemHaandOp,undHaandOpFrygt,undHaandOpFrygtFreq,MestAf,MotSkole)
frygt<-data.frame(FrygtGrMedelever,FrygtGrKarakter,FrygtGrForb,FrygtGrIkkeProb,FrygtGrAndet)

samlet<-data.frame(baggrund,karFak,frygt,fravSyg)
samletNy<-samlet[rowSums(is.na(karFak)) != ncol(karFak), ]

samletNy[is.na(samletNy)]<-0

###QUESTIONS ABOUT ABSENSE AND SICKNESS
sygISkoleResp<-data15$X10.Hvor.ofte.m.der.du.i.skole..hvis.du.er.syg.
sygIskoleKlas<-data15$X11.Hvor.ofte.m.der.dine.klassekammerater.syge.i.skole.
sygeEleverLaerMilj<-data15$X12.F.ler.du..at.syge.elever.i.undervisningen.p.virker.l.ringsmilj.et.
sygISkoleUndgaaFravaer<-data15$X13.Hvor.enig.er.du.i.udsagnet...Jeg.er.m.dt.syg.i.skole.for.at.undg..frav.r..
fravSyg<-data.frame(sygISkoleResp,sygIskoleKlas,sygeEleverLaerMilj,sygISkoleUndgaaFravaer)
fravSygNy<-fravSyg[rowSums(is.na(karFak)) != ncol(karFak), ]

#### RESPONDENT SIMILARITY NETWORK
resp<-paste("R",c(1:length(mydata[,1])),sep="")

probs<-function(mydata,n){
  a<-as.numeric(names(table(mydata[,n])))
  p<-as.numeric(table(mydata[,n])/length(mydata[,1]))
  x<-c(1:length(mydata[,1]))
  for(i in 1:length(a)){
    x[which(mydata[,n]==a[i])]<-p[i]
  }
  return(x)
}

names(samletNy[,8:19])

pmat<-matrix(0,ncol=length(names(samletNy[,8:19])),nrow=length(samletNy[,1]))
for(j in 1:length(names(samletNy[,8:19]))){
  pmat[,j]<-probs(samletNy[,8:19],j)  
  
}
infmat<--log2(pmat)

simRes<-function(i,j,mydata){
  y<-infmat[i,]
  overlap<-sum(y[which(mydata[i,]==mydata[j,])])
  sinfi<-sum(infmat[i,])
  sinfj<-sum(infmat[j,])
  sim<-2*overlap/(sinfi+sinfj)
  return(sim)
}

simResk<-function(k,resp,mydata){
  simVec<-vector()
  for(i in 1:length(resp)){
    simVec[i]<-simRes(k,i,mydata)
  }
  return(simVec)
}

simMatrix<-matrix(data=0,ncol=length(samletNy[,1]),nrow=length(samletNy[,1]))
for(i in 1:length(samletNy[,1])){
  simMatrix[,i]<-simResk(i,samletNy[,1],samletNy[,8:19])  
  
}

library(igraph)
resp<-paste("R",c(1:length(samletNy[,1])),sep="")

s<-graph.adjacency(simMatrix,diag=F,weighted=T,mode = "undirected")
V(s)$id<-resp
source("~/Downloads/DGS_undersoegelser/analysis/ScriptsAndPrograms/PER-R/Creating Networks/Backbone Extraction/backboneExtraction.r")
sBB<-backboneNetwork(s,0.001,2)
write.graph(sBB,"sBB001.graphml",format="graphml")
write.graph(sBB,"sBB001.net",format="pajek")

IM<-infomap.community(sBB)


###TESTING SIMILARITY WEIGHT INSIDE VS. OUTSIDE

relSW<-function(g,x,i){
 #x is a file describing the communities
 nb<-as.vector(neighborhood(g,order=1,nodes=i)[[1]])
 cvec<-vector()
 for(j in 1:length(nb)){
   cvec[j]<-x$lvl1[x$Node==nb[j]]
 }
 nodegroup<-cvec[1]
 N_group<-length(cvec[cvec==nodegroup])-1
 N_neigh<-length(cvec)-1
 belonging<-N_group/N_neigh
 return(belonging)
}
belongVec<-vector()
for(i in 1:length(V(sBB001))){
  belongVec[i]<-relSW(sBB001,x,i)
}

#Making network to plot in igraph
samletNy$FrygtSamlet<-paste(samletNy$FrygtGrMedelever,samletNy$FrygtGrKarakter,samletNy$FrygtGrForb,samletNy$FrygtGrAndet,samletNy$FrygtGrIkkeProb)


V(sBB001)$betydPerson<-samletNy$betydPerson
    V(sBB001)$betydPerson[V(sBB001)$betydPerson==0]<-"no ans"
    V(sBB001)$betydPerson[V(sBB001)$betydPerson==1]<-"high degree"
    V(sBB001)$betydPerson[V(sBB001)$betydPerson==2]<-"some degree"
    V(sBB001)$betydPerson[V(sBB001)$betydPerson==3]<-"low degree"
    V(sBB001)$betydPerson[V(sBB001)$betydPerson==4]<-"not at all"
V(sBB001)$betydStudRet<-samletNy$betydStudRet
    V(sBB001)$betydStudRet[V(sBB001)$betydStudRet==0]<-"no ans"
    V(sBB001)$betydStudRet[V(sBB001)$betydStudRet==1]<-"high degree"
    V(sBB001)$betydStudRet[V(sBB001)$betydStudRet==2]<-"some degree"
    V(sBB001)$betydStudRet[V(sBB001)$betydStudRet==3]<-"low degree"
    V(sBB001)$betydStudRet[V(sBB001)$betydStudRet==4]<-"not at all"
V(sBB001)$bestemHaandOp<-samletNy$bestemHaandOp
    V(sBB001)$bestemHaandOp[V(sBB001)$bestemHaandOp==0]<-"no ans"
    V(sBB001)$bestemHaandOp[V(sBB001)$bestemHaandOp==1]<-"high degree"
    V(sBB001)$bestemHaandOp[V(sBB001)$bestemHaandOp==2]<-"some degree"
    V(sBB001)$bestemHaandOp[V(sBB001)$bestemHaandOp==3]<-"low degree"
    V(sBB001)$bestemHaandOp[V(sBB001)$bestemHaandOp==4]<-"not at all"
#V(sBB001)$undHaandOpFrygt<-samletNy$undHaandOpFrygt
V(sBB001)$undHaandOpFrygtFreq<-samletNy$undHaandOpFrygtFreq
    V(sBB001)$undHaandOpFrygtFreq[V(sBB001)$undHaandOpFrygtFreq==0]<-"no ans"
    V(sBB001)$undHaandOpFrygtFreq[V(sBB001)$undHaandOpFrygtFreq==1]<-"every class"
    V(sBB001)$undHaandOpFrygtFreq[V(sBB001)$undHaandOpFrygtFreq==2]<-"daily"
    V(sBB001)$undHaandOpFrygtFreq[V(sBB001)$undHaandOpFrygtFreq==3]<-"weekly"
    V(sBB001)$undHaandOpFrygtFreq[V(sBB001)$undHaandOpFrygtFreq==4]<-"montly"
    V(sBB001)$undHaandOpFrygtFreq[V(sBB001)$undHaandOpFrygtFreq==5]<-"couple of times"
    V(sBB001)$undHaandOpFrygtFreq[V(sBB001)$undHaandOpFrygtFreq==6]<-"never"
V(sBB001)$MestAf<-samletNy$MestAf
  V(sBB001)$MestAf[V(sBB001)$MestAf==0]<-"no ans"
  V(sBB001)$MestAf[V(sBB001)$MestAf==1]<-"grades"
  V(sBB001)$MestAf[V(sBB001)$MestAf==2]<-"written"
  V(sBB001)$MestAf[V(sBB001)$MestAf==3]<-"verbal"
  V(sBB001)$MestAf[V(sBB001)$MestAf==4]<-"nothing"
  V(sBB001)$MestAf[V(sBB001)$MestAf==5]<-"other"
V(sBB001)$MotSkole<-samletNy$MotSkole
  V(sBB001)$MotSkole[V(sBB001)$MotSkole==0]<-"no ans"
  V(sBB001)$MotSkole[V(sBB001)$MotSkole==1]<-"social"
  V(sBB001)$MotSkole[V(sBB001)$MotSkole==2]<-"absense"
  V(sBB001)$MotSkole[V(sBB001)$MotSkole==3]<-"grades"
  V(sBB001)$MotSkole[V(sBB001)$MotSkole==4]<-"learning"
  V(sBB001)$MotSkole[V(sBB001)$MotSkole==5]<-"don't know"
  V(sBB001)$MotSkole[V(sBB001)$MotSkole==6]<-"other"
V(sBB001)$FrygtGrMedelever<-samletNy$FrygtGrMedelever
V(sBB001)$FrygtGrKarakter<-samletNy$FrygtGrKarakter
V(sBB001)$FrygtGrForb<-samletNy$FrygtGrForb
V(sBB001)$FrygtGrAndet<-samletNy$FrygtGrAndet
V(sBB001)$FrygtGrIkkeProb<-samletNy$FrygtGrIkkeProb
V(sBB001)$FrygtSamlet<-samletNy$FrygtSamlet

V(sBB001)$aargang[V(sBB001)$aargang==0]<-"no ans"
V(sBB001)$aargang[V(sBB001)$aargang==1]<-"1.g"
V(sBB001)$aargang[V(sBB001)$aargang==2]<-"2.g"
V(sBB001)$aargang[V(sBB001)$aargang==3]<-"3.g"
V(sBB001)$aargang[V(sBB001)$aargang==4]<-"1.hf"
V(sBB001)$aargang[V(sBB001)$aargang==5]<-"2.hf"
V(sBB001)$aargang[V(sBB001)$aargang==6]<-"pre-IB"
V(sBB001)$aargang[V(sBB001)$aargang==7]<-"1.IB"
V(sBB001)$aargang[V(sBB001)$aargang==8]<-"2.IB"

V(sBB001)$alder[V(sBB001)$alder==0]<-"no ans"
V(sBB001)$alder[V(sBB001)$alder==1]<-"15"
V(sBB001)$alder[V(sBB001)$alder==2]<-"16"
V(sBB001)$alder[V(sBB001)$alder==3]<-"17"
V(sBB001)$alder[V(sBB001)$alder==4]<-"18"
V(sBB001)$alder[V(sBB001)$alder==5]<-"19"
V(sBB001)$alder[V(sBB001)$alder==6]<-"20+"

V(sBB001)$studRet[V(sBB001)$studRet==0]<-"no ans"
V(sBB001)$studRet[V(sBB001)$studRet==1]<-"science"
V(sBB001)$studRet[V(sBB001)$studRet==2]<-"language"
V(sBB001)$studRet[V(sBB001)$studRet==3]<-"math"
V(sBB001)$studRet[V(sBB001)$studRet==4]<-"socio"
V(sBB001)$studRet[V(sBB001)$studRet==5]<-"crea/body"
V(sBB001)$studRet[V(sBB001)$studRet==6]<-"other"

V(sBB001)$foraelDK[V(sBB001)$foraelDK==0]<-"no ans"
V(sBB001)$foraelDK[V(sBB001)$foraelDK==1]<-"one"
V(sBB001)$foraelDK[V(sBB001)$foraelDK==2]<-"both"
V(sBB001)$foraelDK[V(sBB001)$foraelDK==3]<-"none"

V(sBB001)$foraelSki[V(sBB001)$foraelSki==0]<-"no ans"
V(sBB001)$foraelSki[V(sBB001)$foraelSki==1]<-"yes"
V(sBB001)$foraelSki[V(sBB001)$foraelSki==2]<-"no"

V(sBB001)$foraelUdd[V(sBB001)$foraelUdd==0]<-"no ans"
V(sBB001)$foraelUdd[V(sBB001)$foraelUdd==1]<-"primary"
V(sBB001)$foraelUdd[V(sBB001)$foraelUdd==2]<-"secondary"
V(sBB001)$foraelUdd[V(sBB001)$foraelUdd==3]<-"vocational"
V(sBB001)$foraelUdd[V(sBB001)$foraelUdd==4]<-"short tertiary"
V(sBB001)$foraelUdd[V(sBB001)$foraelUdd==5]<-"medium tertiary"
V(sBB001)$foraelUdd[V(sBB001)$foraelUdd==6]<-"long tertiary"

V(sBB001)$sygISkoleResp<-samletNy$sygISkoleResp
V(sBB001)$sygISkoleKlas<-samletNy$sygIskoleKlas
V(sBB001)$sygeEleverLaerMilj<-samletNy$sygeEleverLaerMilj
V(sBB001)$sygISkoleUndgaaFravaer<-samletNy$sygISkoleUndgaaFravaer
##Characterization of groups
g1<-induced_subgraph(sBB001,x$Node[x$lvl1==1])
g1<-induced_subgraph(sBB001,V(sBB001)[V(sBB001)$lvl1==1])

g2<-induced_subgraph(sBB001,x$Node[x$lvl1==2])
g2<-induced_subgraph(sBB001,V(sBB001)[V(sBB001)$lvl1==2])

g3<-induced_subgraph(sBB001,x$Node[x$lvl1==3])
g4<-induced_subgraph(sBB001,x$Node[x$lvl1==4])
g5<-induced_subgraph(sBB001,x$Node[x$lvl1==5])
g6<-induced_subgraph(sBB001,x$Node[x$lvl1==6])
g7<-induced_subgraph(sBB001,x$Node[x$lvl1==7])
g8<-induced_subgraph(sBB001,x$Node[x$lvl1==8])
g9<-induced_subgraph(sBB001,x$Node[x$lvl1==9])
g10<-induced_subgraph(sBB001,x$Node[x$lvl1==10])
g11<-induced_subgraph(sBB001,x$Node[x$lvl1==11])
g12<-induced_subgraph(sBB001,x$Node[x$lvl1==12])
g13<-induced_subgraph(sBB001,x$Node[x$lvl1==13])
g14<-induced_subgraph(sBB001,x$Node[x$lvl1==14])
g15<-induced_subgraph(sBB001,x$Node[x$lvl1==15])
g16<-induced_subgraph(sBB001,x$Node[x$lvl1==16])
g17<-induced_subgraph(sBB001,x$Node[x$lvl1==17])
g18<-induced_subgraph(sBB001,x$Node[x$lvl1==18])
g19<-induced_subgraph(sBB001,x$Node[x$lvl1==19])


###Testing for magnitude of differences from expected

p<-table(V(sBB001)$koen)/vcount(sBB001)
q<-table(V(g1)$koen)/vcount(g1)

attrDiff<-function(g,sb,att="id"){
  gatt<-vertex_attr(g, att, index = V(g))
  sbatt<-vertex_attr(sb, att, index = V(sb))
  q<-table(sbatt)/vcount(sb)
  p<-table(gatt)/vcount(g)
  a<-round(vcount(g)*q)
  y<-table(gatt)
  x<-names(a)
  mat<-matrix(0,nrow = 6,ncol = length(x))
  colnames(mat)<-x
  mat[1,]<-as.numeric(a)
  
  mat[2,x%in%names(y)]<-as.numeric(y)
  mat[3,]<-mat[2,]-mat[1,]
  mat[4,]<-q
  mat[5,x%in%names(p)]<-as.numeric(p)
  mat[6,]<-mat[5,]*log2(mat[5,]/mat[4,])
  
  rownames(mat)<-c("expected","actual","delta","cohort freq", "group freq", "Divergence contribution")
  return(mat)
}

genderDiff<-function(g,sb){
  q<-table(V(sb)$koen)/vcount(sb)
  p<-table(V(g)$koen)/vcount(g)
  a<-round(vcount(g)*q)
  y<-table(V(g)$koen)
  x<-names(a)
  mat<-matrix(0,nrow = 6,ncol = length(x))
  colnames(mat)<-x
  mat[1,]<-as.numeric(a)
  
  mat[2,x%in%names(y)]<-as.numeric(y)
  mat[3,]<-mat[2,]-mat[1,]
  mat[4,]<-q
  mat[5,x%in%names(p)]<-as.numeric(p)
  mat[6,]<-mat[5,]*log2(mat[5,]/mat[4,])
 
  rownames(mat)<-c("expected","actual","delta","cohort freq", "group freq", "Divergence contribution")
  return(mat)
}



