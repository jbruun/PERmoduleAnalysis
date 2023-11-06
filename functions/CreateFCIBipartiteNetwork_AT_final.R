#My plan is to construct a bipartite network from FCI data.
#FCI data has 30 questions, with 5 levels A-E
# I want to be able to construct a loop that takes a FCI data file and creates a nx150 matrix which will be the bipartite matrix 
# This file is my effort to construct such a loop.  
#As of now I can get my data into r, and I can evaluate an element of the FCIData dataframe and make an assignment to FCINet

#setwd("C:/Users/Adrienne/Documents/School/Research/Paperwork and comments/thefirstrprogramireallywroteonmyown/")

#FCIData <- read.csv("~/Dropbox/R Work/FCI Network Analysis/ExampleFCINetworkAnalysis.csv")
FCIData <- read.csv("data/ExampleFCINetworkAnalysis.csv")#This is where my data is
FCIData2 <- read.csv("data/ExampleFCINetworkAnalysis.csv",colClasses=c(rep("character",30)))

#I need to first set up the FCINet dataframe and add in the 0 and 1 values  
NFCI <- length(FCIData[,1])  #This tells me how many rows there are in the FCIData Dataframe
FCINet <- matrix(data = NA, nrow = NFCI, ncol = 150)  #This sets up my matrix
FCINet2 <- matrix(data = NA, nrow = NFCI, ncol = 150)  #This sets up my matrix
FCINet3 <- matrix(data = NA, nrow = NFCI, ncol = 150)  #This sets up my matrix
#Currently I need to start filling in my data, which means a for loop
i<-1 #dummy index starts at 1
j<-1 #dummy index starts at 1
k<-1

ptm <- proc.time()
for(k in 1:30) 
{

for(i in 1:NFCI) 
  {
  if(FCIData[i,k] == "A") FCINet[i,j]<-1 
  else FCINet[i,j] <- 0
  i=1+1
  } #I can make this fill the first vector in the dataframe with anyone who responded "A"
i<-1 #dummy index starts at 1
j=j+1
for(i in 1:NFCI) 
{
  if(FCIData[i,k] == "B") FCINet[i,j]<-1 
  else FCINet[i,j] <- 0
  i=1+1
} 
i<-1 #dummy index starts at 1
j=j+1
for(i in 1:NFCI) 
{
  if(FCIData[i,k] == "C") FCINet[i,j]<-1 
  else FCINet[i,j] <- 0
  i=1+1
}
i<-1 #dummy index starts at 1
j=j+1
for(i in 1:NFCI) 
{
  if(FCIData[i,k] == "D") FCINet[i,j]<-1 
  else FCINet[i,j] <- 0
  i=1+1
}
i<-1 #dummy index starts at 1
j = j+1
for(i in 1:NFCI) 
{
  if(FCIData[i,k] == "E") FCINet[i,j]<-1 
  else FCINet[i,j] <- 0
  i=1+1
}
j=j+1
k=k+1
}#This closes the k for loop.

time1 <- proc.time() - ptm

###

i<-1 #dummy index starts at 1
j<-1 #dummy index starts at 1
k<-1

ptm <- proc.time()
for(k in 1:30) 
{
  FCINet2[,j] <- as.numeric(FCIData2[,k]=="A")
  j=j+1
  FCINet2[,j] <- as.numeric(FCIData2[,k]=="B")
  j=j+1
  FCINet2[,j] <- as.numeric(FCIData2[,k]=="C")
  j=j+1
  FCINet2[,j] <- as.numeric(FCIData2[,k]=="D")
  j=j+1
  FCINet2[,j] <- as.numeric(FCIData2[,k]=="E")
  j=j+1
}#This closes the k for loop.

time2 <- proc.time() - ptm

# Check for identical matrices (yes if FCINet2 is correct)
identical(FCINet,FCINet2)

###

# Third and final version
ptm <- proc.time() # start timer for loop
for(k in 1:30) 
{
  kmin <- k*5-4
  kmax <- k*5 
  FCINet3[,kmin:kmax] <- t(sapply(FCIData[,k],function (x) as.numeric(x==c("A","B","C","D","E"))))
}
time3 <- proc.time() - ptm # stop timer for loop
identical(FCINet,FCINet3) # check that my result matches
