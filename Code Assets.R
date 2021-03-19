library(readxl)
library(YieldCurve)
library(ggplot2)
library(dplyr)

assets<-read_excel("Emp Data.xlsx",sheet = 2)
gb<-read_excel("Actuarial Assumptions.xlsx",sheet = 3)
gb$months<-gb$Term*12

p<-Svensson( gb$Yield, gb$Term )
p

#The Nielson Siegel Svensson parametric formula 

Smooth<-function(p,Term)
{
  R<-p[1]+((p[2]+p[3])*((1-exp(-gb$Term/p[5]))/(gb$Term/p[5]))-p[3]*exp(-gb$Term/p[5])+p[4]*((1-exp(-gb$Term/p[6]))/(gb$Term/p[6]))-p[4]*exp(-gb$Term/p[6]))
  return(R)
}  

ggplot()

gb$SmoothRates<-Smooth(p,gb$Term)

gb$forwardYields<-gb$SmoothRates[1]
for (i in 1:43)
{
  gb$forwardYields[i]<-(1+gb$SmoothRates[i])^(i)/(1+gb$SmoothRates[i])^(i-1)-1
}
#Assuming no return from the investment in the BANK ACCOUNT
#The amount at the begining of the period

Projected_Assets<-matrix(0,nrow = 43,ncol = 8)
Projected_Assets[1,]=t(assets[,2])
#At the begining of i th period
for (j in c(1,2,3,4,5,6,8)) 
{  
  for (i in 2:43)
  {
    Projected_Assets[i,j]=Projected_Assets[i-1,j]*(1+gb$forwardYields[i-1])
  }
}
# For the Bank account

Projected_Assets[,7]=rep(Projected_Assets[1,7],43)

Projected_Assets

Projected_Assets<-as.data.frame(Projected_Assets) 

colnames(Projected_Assets)=t(assets[,1])





