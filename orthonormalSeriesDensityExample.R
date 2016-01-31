rm(list=ls())
source("./orthonormalSeriesDensity.R")

set.seed(12324)
J=5
n=1000
#simulate a mixture of two Gaussians
y= rbinom(n,1,.3)
x= rnorm(n,.1,.1)*y + rnorm(n,.7,.1)*(1-y)
x[x>=1] = 1; x[x<=0]=0

par(mfcol=c(2,1))
predictions<- orthonormalSeriesDensity(x)
hist(x,breaks=25)