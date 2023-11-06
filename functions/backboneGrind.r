##Make a matrix of normalized weights
#do this once, and then save the resulting matrix
#Load SimMat30Q from local directory
#load("/Volumes/rmn845/Forskning/MAMCR_extension/output/simMat30Q.RData")
source("functions/F_hat.r")
source("functions/Alpha_ij.r")
n_cores<-detectCores()-1 #this is the number of cores we'll use
cl <- makeCluster(n_cores)  
registerDoParallel(cl)  

n<-dim(SimMat8Q)[1]
norWSimMat8Q<-matrix(0,nrow=n,ncol=n)
timeW<-vector()
for(i in 1:n){
  ptm<-proc.time()
  norWSimMat8Q[,i]<-SimMat8Q[,i]/sum(SimMat8Q[,i])
  timeW[i]<-(proc.time()-ptm)[3]
}
save(norWSimMat8Q, file = "~/Documents/MAMCR_extension/norWSimMat8Q.RData")
m<-dim(norWSimMat8Q)[1]

LANSMat8Q<-matrix(0,nrow=m,ncol=m)
dispMat8Q<-matrix(0,nrow=m,ncol=m)


load("~/Documents/MAMCR_extension/norWSimMat8Q.RData")
load("~/Documents/MAMCR_extension/LANSMat8Q.RData")
timeL<-read.csv("timeLANS8Q_calcs.csv")
timeL<-timeL[,2]



#The idea here is to calculate A_ij and F_hat for the SimMat
#These are set, and then we can change the alpha-level to extract different backbones. 

#Make the F_hat calculations
#use F_hat par 
l<-match(0,colSums(LANSMat8Q))
timeL<-vector()
for(k in l:33502){
  ptm<-proc.time()
  LANSMat8Q[,k]<-F_hatPar(norWSimMat8Q[,k])
  timeL[k]<-(proc.time()-ptm)[3]
  print(c(k,timeL[k]))
}

save(LANSMat8Q,file = "~/Documents/MAMCR_extension/LANSMat8Q.RData")
write.csv(timeL,"timeLANS8Q_calcs.csv")
#Make the Alpha_ij calculations
#parallel processing will not help, but the calcs are faster than above
load("/Volumes/rmn845/Forskning/MAMCR_extension/output/dispMat30Q.RData")
l<-match(0,colSums(dispMat8Q))
timeA<-vector()
for(k in l:33502){
  ptm<-proc.time()
  dispMat8Q[,k]<-AlphaPar(norWSimMat8Q[,k])
  timeA[k]<-(proc.time()-ptm)[3]
  print(c(k,timeA[k]))
}

save(dispMat8Q,file = "~/Documents/MAMCR_extension/dispMat8Q.RData")
write.csv(timeA,"timeDisp_calcs.csv")



ptm<-proc.time()
dispVec<-F_hat(norWSimMat30Q[,1])
time<-proc.time()-ptm

ptm<-proc.time()
test1par<-F_hatPar(p)
FtimePar<-proc.time()-ptm

ptm<-proc.time()
test2<-Alpha(p)
Atime<-proc.time()-ptm

ptm<-proc.time()
test2par<-AlphaPar(p)
AtimePar<-proc.time()-ptm

alpha<-c(1:100)/10000

a<-vector()
f<-vector()
for(i in 1:100){
  a[i]<-length(which(sigTest(test2,alpha[i])))
  f[i]<-length(which(sigTest(test1,alpha[i])))
}


