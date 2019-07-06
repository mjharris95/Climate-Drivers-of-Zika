library(readxl)
library(lmerTest)
library(mosaic)
library(MASS)
library(rcompanion)
library(coefplot)
library(plm)  
library(lme4)

load("cov0701.RData")
load("data126")
spatial.chars<-read.csv("provinceleveldata.csv")
weather.data<-excel_sheets("manualweather.xlsx")


cov$cases<-lapply(cov$names, function(x) all.data[[x]]$cases)
# Finding the weights for the serial interval
shape = (20.0/7.4)^2
rate = 20.0/(7.4^2)

gamma_dist<-function(t){
  (pgamma(t+7, shape, rate)-pgamma(t, shape, rate))/7
}

times<-0:6*7

weights<-rep(NA, 5)
for(i in 1:5){
  weights[i]<-integrate(gamma_dist, times[i], times[i+1])$value/7
}

weights<-weights/sum(weights)  

#TSIR Model - Solve for beta and values of all compartmental variables
compartment.vals<-function(i){
  len<-length(cov$cases[[i]])
  Ieff<-vector(length=len-5)
  for(j in 1:length(Ieff)){
    Ieff[j]<-sum(cov$cases[[i]][(j+4):j]*weights,na.rm=T)
  }
  I<-cov$cases[[i]][6:len]
  sumI<-cumsum(cov$cases[[i]])
  S<-cov$pop[[i]]-sumI[5:(len-1)]
  dates<-cov$time[[i]]
  months<-substr(dates, 6, 7)
  
  N<-rep(cov$pop[[i]], length.out=length(I))
  
  #logged beta
  betas<-ifelse(Ieff==0, -Inf, log(I)-log(S)+log(N)-(.74)*log(Ieff))
  
  #presence/absence of local transmission
  pa<-ifelse(is.infinite(betas), 0, 1)
  imported<-ifelse(I>0 & Ieff==0, 1, 0)
  return(list("S"=S, "Ieff"=Ieff, "I"=I, "Y"=log(I)-log(S)+log(N), "months"=tail(months,-5), "N"=N, "pa"=pa, "betas"=betas, "dates"=tail(dates,-5), "imported"=imported))
}



vals<-lapply(1:127, function(x) compartment.vals(x))

#the initial weather data 
all_S<-lapply(vals, function(x) head(x[["S"]],-1)) 
all_Ieff<-lapply(vals, function(x) head(x[["Ieff"]],-1)) 
all_I<-lapply(vals, function(x) head(x[["I"]],-1)) 
all_N<-lapply(vals, function(x) head(x[["N"]],-1)) 
all_pa<-lapply(vals, function(x) head(x[["pa"]],-1)) 
all_dates<-lapply(vals, function(x) head(x[["dates"]],-1)) 
all_Y<-lapply(vals, function(x) head(x[["Y"]],-1)) 
all_betas<-lapply(vals, function(x) head(x[["betas"]],-1))
all_months<-lapply(vals, function(x) head(x[["months"]],-1))


cov$pa<-all_pa
cov$betas<-all_betas
cov$months<-all_months
cov$dates<-all_dates

#storing spatial variables
cov$local.trans<-sapply(cov$pa, function(x) ifelse(sum(x)>0, 1, 0))
cov$max.b<-sapply(cov$betas, function(x) max(x, na.rm=TRUE))
#cov$max.b[cov$max.b<0]<-0
cov$avg.b<-sapply(1:length(cov$names), function(i) log(mean(exp(cov$betas[[i]]))))
cov$avg.b[is.infinite(cov$avg.b)]<-0
cov$cum.cases<-sapply(cov$cases, function(x) sum(x, na.rm=TRUE), USE.NAMES=FALSE)
cov$avg.cases<-sapply(cov$cases, function(x) mean(x[x!=0], na.rm=TRUE), USE.NAMES=FALSE)
cov$mean.rain<-sapply(cov$rain, function(x) mean(x, na.rm=TRUE))
cov$mean.range<-sapply(cov$range, function(x) mean(x, na.rm=TRUE))
cov$mean.hum<-sapply(cov$hum, function(x) mean(x, na.rm=TRUE))
cov$mean.mean<-sapply(cov$avg, function(x) mean(x, na.rm=TRUE))
cov$mean.mean.sq<-cov$mean.mean*cov$mean.mean
cov$city.pop<-sapply(cov$names, function(x) spatial.chars$city.pop[which(spatial.chars$V1==x)], USE.NAMES=TRUE)

cov.prov<-cov[c("country", "city.pop", "mean.range", "mean.rain", "mean.hum", "mean.mean", "cum.cases", "avg.cases", "max.b", "avg.b", "local.trans", "mean.mean.sq")]
cov.prov<-as.data.frame(cov.prov)
write.csv(cov.prov, file="covprov")


#total number of imported cases
total_imported<-sum(sapply(vals, function(x) sum(head(x[["imported"]],-1))))
