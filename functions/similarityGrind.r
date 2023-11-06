n_cores<-detectCores()-1 #this is the number of cores we'll use
cl <- makeCluster(n_cores)  
registerDoParallel(cl)   #use doParallel package to engage the cores we want to use
subset<-which(H>2)
IM<-infmat(FCI_data,subset)

I_resp<-rowSums(IM) #the information content per respondent

SimMat8Q<-matrix(0,nrow=n,ncol=n)
diag(SimMat8Q)<-1

#Find local folder with data..
load("/Volumes/rmn845/Forskning/MAMCR_extension/output/simMat8Q.RData")

l<-match(0,colSums(SimMat8Q)-1)

time<-vector()
for(k in l:33502){
  ptm<-proc.time()
  simVec<-vector()
  

simVec<-foreach (i=(k+1):n, .combine=c) %dopar% {
  simRes(k,i,FCI_data,c(1:30),IM)
}

SimMat8Q[(k+1):n,k]<-simVec
time[k]<-(proc.time()-ptm)[3]
time[k]
}

save(SimMat8Q, file = "~/Documents/MAMCR_extension/simMat8Q.RData")
SimMat8Q<-SimMat8Q+t(SimMat8Q)
diag(SimMat8Q)<-0
save(SimMat8Q, file = "~/Documents/MAMCR_extension/simMat8Q.RData")

