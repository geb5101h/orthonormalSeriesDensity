rm(list=ls())
source("./orthonormalSeriesDensity.R")

#set.seed(12324)
J=5
n=1000
#simulate a mixture of two Gaussians
y= rbinom(n,1,.3)
x= rnorm(n,.3,.1)*y + rnorm(n,.7,.1)*(1-y)
x[x>=1] = 1; x[x<=0]=0

par(mfcol=c(3,1))
predictions<- orthonormalSeriesDensity(x,highFreq=F,confBand=T,alpha=.90)
density(x)%>%plot(.,xlim=c(0,1),main="Kernel density estimate")
hist(x,breaks=15)

