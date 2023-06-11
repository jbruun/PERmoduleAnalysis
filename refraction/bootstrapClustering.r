#Bootstrap clustering
#run analysisRefraction until line 32 before this
#need to load igraph
#need to load Backbone function
#need to load variationInformation.r

makeBB<-function(ref,sampSize,lvl,tf){
  samp<-ref[sample(nrow(ref),size=sampSize,replace=F),]
  questions<-names(samp)
  stud<-as.character(samp[,1])
  dimM<-length(stud)+length(questions)
  AdjMat<-matrix(data=0,ncol=dimM,nrow=dimM)
  colnames(AdjMat)<-c(stud,questions)
  AdjMat[1:length(stud),(length(stud)+1):dimM]<-as.matrix(samp)
  g<-graph.adjacency(AdjMat,mode = "upper") #This is the bipartite network
  dd<-degree(g)
  freq<-dd[length(stud)+1:length(dd)]/length(stud)
  #plot(dd[length(stud)+1:length(dd)]/length(stud),xlim=c(1,81))
  type<-vector()
  type[1:length(stud)]<-1
  type[(length(stud)+1):dimM]<-0
  V(g)$type<-as.logical(type)
  
  deleted<-V(g)[which(dd[(length(stud)+1):length(dd)]/length(stud)>tf)+length(stud)] #This identifies the answer "T1Q13right"
  g<-delete.vertices(g,V(g)[which(dd[(length(stud)+1):length(dd)]/length(stud)>tf)+length(stud)])
  
  bip<-bipartite.projection(g)
  qs<-bip$proj1
  V(qs)$id<-V(qs)$name
  qsBB<-backboneNetwork(qs,alpha=lvl,2) #This turns out to be the desired alpha-level
  qsBB$connected<-is.connected(qsBB)
  #plot(degree.distribution(qsBB),log="xy")
  icBB<-infomap.community(qsBB)
  qsBB$mod<-icBB$modularity
  V(qsBB)$membership<-icBB$membership
  result<-list(qsBB,icBB,deleted,freq[1:81])

  return(result)
}

####################


hep<-function(alp,t_f,nit){
z<-vector()
comx<-vector()
comy<-vector()
modx<-vector()
mody<-vector()
Nx<-vector()
Ny<-vector()
Lx<-vector()
Ly<-vector()
p<-vector()
conx<-vector()
cony<-vector()
largestModulex<-vector()
largestModuley<-vector()
for (i in 1:nit){
  x<-makeBB(ref,1000,alp,t_f)
  y<-makeBB(ref,1000,alp,t_f)
  z[i]<-Distance(x[[1]],y[[1]])
  comx[i]<-length(unique(V(x[[1]])$membership))
  comy[i]<-length(unique(V(y[[1]])$membership))
  modx[i]<-x[[1]]$mod
  mody[i]<-y[[1]]$mod
  Nx[i]<-length(V(x[[1]]))
  Ny[i]<-length(V(y[[1]]))
  Lx[i]<-length(E(x[[1]]))
  Ly[i]<-length(E(y[[1]]))
  p[i]<-cor.test(x[[4]],y[[4]],method="kendall")$p.value
  conx[i]<-as.integer(x[[1]]$connected)
  cony[i]<-as.integer(y[[1]]$connected)
  largestModulex[i]<-max(table(x[[2]]$membership))
  largestModuley[i]<-max(table(y[[2]]$membership))
}
result<-data.frame(z,Nx,Ny,Lx,Ly,comy,comx,modx,mody,conx,cony,p,largestModulex,largestModuley)
return(result)
}



meanValues<-function(t_f,niter){
  zm<-vector()
  zsd<-vector()
  Nm<-vector()
  Nsd<-vector()
  Lm<-vector()
  Lsd<-vector()
  comm<-vector()
  comsd<-vector()
  conm<-vector()
  consd<-vector()
  pm<-vector()
  psd<-vector()
  modm<-vector()
  modsd<-vector()
  modulemaxm<-vector()
  modulemaxsd<-vector()
  for (i in 1:10){
  calc<-hep(0.01*i,t_f,niter)
  zm[i]<-mean(calc$z)
  zsd[i]<-sd(calc$z)
  Nm[i]<-mean(c(calc$Nx,calc$Ny))
  Nsd[i]<-sd(c(calc$Nx,calc$Ny))
  Lm[i]<-mean(c(calc$Lx,calc$Ly))
  Lsd[i]<-sd(c(calc$Lx,calc$Ly))
  modm[i]<-mean(c(calc$modx,calc$mody))
  modsd[i]<-sd(c(calc$modx,calc$mody))
  comm[i]<-mean(c(calc$comx,calc$comy))
  comsd[i]<-sd(c(calc$comx,calc$comy))
  conm[i]<-mean(c(calc$conx,calc$cony))
  consd[i]<-sd(c(calc$conx,calc$cony))
  pm[i]<-mean(calc$p)
  psd[i]<-sd(calc$p)
  modulemaxm[i]<-mean(c(calc$largestModulex,calc$largestModuley))
  modulemaxsd[i]<-sd(c(calc$largestModulex,calc$largestModuley))
  
  }
  result<-data.frame(zm,zsd,Nm,Nsd,Lm,Lsd,modm,modsd,comm,comsd,conm,consd,pm,psd,modulemaxm,modulemaxsd)
 return(result) 
}

tf05<-meanValues(0.05,100)
tf10<-meanValues(0.10,100)
tf15<-meanValues(0.15,100)
tf20<-meanValues(0.20,100)
tf25<-meanValues(0.25,100)
tf30<-meanValues(0.30,100)
tf35<-meanValues(0.35,100)
tf40<-meanValues(0.40,100)
tf45<-meanValues(0.45,100)
tf50<-meanValues(0.50,100)
tf55<-meanValues(0.55,100)
tf60<-meanValues(0.60,100)
tf65<-meanValues(0.65,100)
tf70<-meanValues(0.70,100)
tf75<-meanValues(0.75,100)
tf80<-meanValues(0.80,100)
tf85<-meanValues(0.85,100)
tf90<-meanValues(0.90,100)

write.csv(tf05,"tf05a.csv")
write.csv(tf10,"tf10a.csv")
write.csv(tf15,"tf15a.csv")
write.csv(tf20,"tf20a.csv")
write.csv(tf25,"tf25a.csv")
write.csv(tf30,"tf30a.csv")
write.csv(tf35,"tf35a.csv")
write.csv(tf40,"tf40a.csv")
write.csv(tf45,"tf45a.csv")
write.csv(tf50,"tf50a.csv")
write.csv(tf55,"tf55a.csv")
write.csv(tf60,"tf60a.csv")
write.csv(tf65,"tf65a.csv")
write.csv(tf70,"tf70a.csv")
write.csv(tf75,"tf75a.csv")
write.csv(tf80,"tf80a.csv")
write.csv(tf85,"tf85a.csv")
write.csv(tf90,"tf90a.csv")


plot(x,tf05$comm,type="o",ylim=c(0,21))
points(x,tf10$comm,type="o",pch=2)
points(x,tf15$comm,type="o",pch=3)
points(x,tf20$comm,type="o",pch=4)
points(x,tf25$comm,type="o",pch=5)
points(x,tf30$comm,type="o",pch=6)
points(x,tf35$comm,type="o",pch=7)
points(x,tf40$comm,type="o",pch=8)
points(x,tf45$comm,type="o",pch=9)
points(x,tf50$comm,type="o",pch=10)
points(x,tf55$comm,type="o",pch=11)
points(x,tf60$comm,type="o",pch=12)
points(x,tf65$comm,type="o",pch=13)
points(x,tf70$comm,type="o",pch=14)
points(x,tf75$comm,type="o",pch=15)
points(x,tf80$comm,type="o",pch=16)

x<-0.01*c(1:10)
plot(x,tf30$modulemaxm,ylim=c(0,81),xlab="alpha-level",ylab="<Distance>",type="o")
arrows(x, tf30$modulemaxm-tf30$modulemaxsd/5, x, tf30$modulemaxm+tf30$modulemaxsd/5, length=0.05, angle=90, code=3)
points(x,tf40$modulemaxm,type="o",pch=9)
arrows(x, tf40$modulemaxm-tf40$modulemaxsd/5, x, tf40$modulemaxm+tf40$modulemaxsd/5, length=0.05, angle=90, code=3)
points(x,tf45$modulemaxm,type="o",pch=8)
arrows(x, tf45$modulemaxm-tf45$modulemaxsd/5, x, tf45$modulemaxm+tf45$modulemaxsd/5, length=0.05, angle=90, code=3)
points(x,tf35$modulemaxm,type="o",pch=10)
arrows(x, tf35$modulemaxm-tf35$modulemaxsd/5, x, tf35$modulemaxm+tf35$modulemaxsd/5, length=0.05, angle=90, code=3)

plot(tf80$zm,ylim=c(0,2),xlab="alpha-level",ylab="<Distance>",type="o")
points(tf75$zm,type="o",pch=2)
points(tf70$zm,type="o",pch=3)
points(tf65$zm,type="o",pch=4)
points(tf60$zm,type="o",pch=5)
points(tf55$zm,type="o",pch=6)
points(tf50$zm,type="o",pch=7)
points(tf45$zm,type="o",pch=8)
points(tf40$zm,type="o",pch=9)
points(tf35$zm,type="o",pch=10)
points(tf30$zm,type="o",pch=11)


plot(tf80$Nm,ylim=c(0,100),xlab="alpha-level",ylab="<Distance>",type="o")
points(tf75$Nm,type="o",pch=2)
points(tf70$Nm,type="o",pch=3)
points(tf65$Nm,type="o",pch=4)
points(tf60$Nm,type="o",pch=5)
points(tf55$Nm,type="o",pch=6)
points(tf50$Nm,type="o",pch=7)
points(tf45$Nm,type="o",pch=8)
points(tf40$Nm,type="o",pch=9)
points(tf35$Nm,type="o",pch=10)
points(tf30$Nm,type="o",pch=11)

plot(tf80$Lm,ylim=c(0,600),xlab="alpha-level",ylab="Links",type="o")
points(tf75$Lm,type="o",pch=2)
points(tf70$Lm,type="o",pch=3)
points(tf65$Lm,type="o",pch=4)
points(tf60$Lm,type="o",pch=5)
points(tf55$Lm,type="o",pch=6)
points(tf50$Lm,type="o",pch=7)
points(tf45$Lm,type="o",pch=8)
points(tf40$Lm,type="o",pch=9)
points(tf35$Lm,type="o",pch=10)
points(tf30$Lm,type="o",pch=11)

plot(tf80$comm,ylim=c(0,20),xlab="alpha-level",ylab="<#communities>",type="o")
points(tf75$comm,type="o",pch=2)
points(tf70$comm,type="o",pch=3)
points(tf65$comm,type="o",pch=4)
points(tf60$comm,type="o",pch=5)
points(tf55$comm,type="o",pch=6)
points(tf50$comm,type="o",pch=7)
points(tf45$comm,type="o",pch=8)
points(tf40$comm,type="o",pch=9)
points(tf35$comm,type="o",pch=10)
points(tf30$comm,type="o",pch=11)

plot(tf80$conm,ylim=c(0,1),xlab="alpha-level",ylab="<connected>",type="o")
points(tf75$conm,type="o",pch=2)
points(tf70$conm,type="o",pch=3)
points(tf65$conm,type="o",pch=4)
points(tf60$conm,type="o",pch=5)
points(tf55$conm,type="o",pch=6)
points(tf50$conm,type="o",pch=7)
points(tf45$conm,type="o",pch=8)
points(tf40$conm,type="o",pch=9)
points(tf35$conm,type="o",pch=10)
points(tf30$conm,type="o",pch=11)

plot(tf80$Nm/tf80$comm,ylim=c(0,30),xlab="alpha-level",ylab="<nodes/community",type="o")
points(tf75$Nm/tf75$comm,type="o",pch=2)
points(tf70$Nm/tf70$comm,type="o",pch=3)
points(tf65$Nm/tf65$comm,type="o",pch=4)
points(tf60$Nm/tf60$comm,type="o",pch=5)
points(tf55$Nm/tf55$comm,type="o",pch=6)
points(tf50$Nm/tf50$comm,type="o",pch=7)
points(tf45$Nm/tf45$comm,type="o",pch=8)
points(tf40$Nm/tf40$comm,type="o",pch=9)
points(tf35$Nm/tf35$comm,type="o",pch=10)
points(tf30$Nm/tf30$comm,type="o",pch=11)

iter<-1000
IM<-matrix(0,ncol=68,nrow=iter)
for (i in 1:iter){
bb<-makeBB(ref,200,0.0411,0.3)
if(length(V(bb[[1]]))!=68){
  IM[i,]<-rep(0,68)
}else{
IM[i,]<-bb[[2]]$membership
}
}

iter<-1000
IM<-matrix(0,ncol=78,nrow=iter)
for (i in 1:iter){
  bb<-makeBB(test,207,0.025,0.5)
  if(length(V(bb[[1]]))!=78){
    IM[i,]<-rep(0,78)
  }else{
    IM[i,]<-bb[[2]]$membership
  }
}


iter<-1000
IM<-matrix(0,ncol=length(V(f90)),nrow=iter)
#bb<-makeBB(ref,800,0.03,0.5)
for (i in 1:iter){
  set.seed(i)
  bic<-infomap.community(f90,nb.trials = 3)
    IM[i,]<-bic$membership
  }

