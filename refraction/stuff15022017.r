cenEig<-vector()
cenEig[1]<-centr_eigen(f10)$centralization
cenEig[2]<-centr_eigen(f20)$centralization
cenEig[3]<-centr_eigen(f30)$centralization
cenEig[4]<-centr_eigen(f40)$centralization
cenEig[5]<-centr_eigen(f50)$centralization
cenEig[6]<-centr_eigen(f60)$centralization
cenEig[7]<-centr_eigen(f70)$centralization
cenEig[8]<-centr_eigen(f80)$centralization
cenEig[9]<-centr_eigen(f90)$centralization


cenPR<-vector()
cenPR[1]<-centralize(page.rank(f10)$vector, theoretical.max = 0, normalized = F)/centralize(page.rank(make_star(length(V(f10)), mode = "undirected"))$vector, theoretical.max = 0, normalized = F)
cenPR[2]<-centralize(page.rank(f20)$vector, theoretical.max = 0, normalized = F)/centralize(page.rank(make_star(length(V(f20)), mode = "undirected"))$vector, theoretical.max = 0, normalized = F)
cenPR[3]<-centralize(page.rank(f30)$vector, theoretical.max = 0, normalized = F)/centralize(page.rank(make_star(length(V(f30)), mode = "undirected"))$vector, theoretical.max = 0, normalized = F)
cenPR[4]<-centralize(page.rank(f40)$vector, theoretical.max = 0, normalized = F)/centralize(page.rank(make_star(length(V(f40)), mode = "undirected"))$vector, theoretical.max = 0, normalized = F)
cenPR[5]<-centralize(page.rank(f50)$vector, theoretical.max = 0, normalized = F)/centralize(page.rank(make_star(length(V(f50)), mode = "undirected"))$vector, theoretical.max = 0, normalized = F)
cenPR[6]<-centralize(page.rank(f60)$vector, theoretical.max = 0, normalized = F)/centralize(page.rank(make_star(length(V(f60)), mode = "undirected"))$vector, theoretical.max = 0, normalized = F)
cenPR[7]<-centralize(page.rank(f70)$vector, theoretical.max = 0, normalized = F)/centralize(page.rank(make_star(length(V(f70)), mode = "undirected"))$vector, theoretical.max = 0, normalized = F)
cenPR[8]<-centralize(page.rank(f80)$vector, theoretical.max = 0, normalized = F)/centralize(page.rank(make_star(length(V(f80)), mode = "undirected"))$vector, theoretical.max = 0, normalized = F)
cenPR[9]<-centralize(page.rank(f90)$vector, theoretical.max = 0, normalized = F)/centralize(page.rank(make_star(length(V(f90)), mode = "undirected"))$vector, theoretical.max = 0, normalized = F)

cenPR<-vector()
cenPR[1]<-centralize(page.rank(f10)$vector, theoretical.max = 0, normalized = F)
cenPR[2]<-centralize(page.rank(f20)$vector, theoretical.max = 0, normalized = F)
cenPR[3]<-centralize(page.rank(f30)$vector, theoretical.max = 0, normalized = F)
cenPR[4]<-centralize(page.rank(f40)$vector, theoretical.max = 0, normalized = F)
cenPR[5]<-centralize(page.rank(f50)$vector, theoretical.max = 0, normalized = F)
cenPR[6]<-centralize(page.rank(f60)$vector, theoretical.max = 0, normalized = F)
cenPR[7]<-centralize(page.rank(f70)$vector, theoretical.max = 0, normalized = F)
cenPR[8]<-centralize(page.rank(f80)$vector, theoretical.max = 0, normalized = F)
cenPR[9]<-centralize(page.rank(f90)$vector, theoretical.max = 0, normalized = F)

z_cenPR<-function(g,iter){
  v<-vector()
  vx<-centralize(page.rank(g)$vector, theoretical.max = 0, normalized = F)
  for(i in 1:iter){
    h<-rewire(g, with = keeping_degseq(niter = vcount(g) * 10))
  v[i]<-centralize(page.rank.old(h), theoretical.max = 0, normalized = F)
  }
  z<-(vx-mean(v,na.rm=T))/sd(v,na.rm=T)
  return(z)
}



###10000IM####
files<- list.files("../../ScriptsAndPrograms/Infomap/refractionTest/1000IM/",pattern="*clu")
paths <- paste("../../ScriptsAndPrograms/Infomap/refractionTest/1000IM/",files,sep="/")
clus<- lapply(paths,read.csv,sep= " ",skip = 1 )

for (i in 1:1000){
clus[[i]]<-clus[[i]][order(clus[[i]]$X.),]
}
a<-matrix(0,ncol=76,nrow=1000)
for (i in 1:1000){
a[i,]<-clus[[i]]$node
}

Q<-vector()
N<-vector()
nmi<-vector()
for (i in 1:1000){
  Q[i]<-modularity(f50,a[i,])
  N[i]<-length(unique(a[i,]))
  nmi[i]<-compare(icl$membership,a[i,],method="nmi")
  
}

cohensD<-function(x1,x2){
  s1<-sd(x1)
  s2<-sd(x2)
  n1<-length(x1)
  n2<-length(x2)
  s<-sqrt(((n1-1)*s1^2+(n2-1)*s2^2)/(n2+n1-2))
  d<-(mean(x1)-mean(x2))/s
  return(d)
}