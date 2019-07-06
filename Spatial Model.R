library(readxl)
library(lmerTest)
library(mosaic)
library(MASS)
library(rcompanion)
library(coefplot)
library(plm)  
library(lme4)

cov.prov<-read.csv("covprov")
rv<-c("local.trans","max.b","avg.b","cum.cases","avg.cases")

#list of spatial models on their own (spatial.models) and with additional features (spatial.models.ext)
#the tag for each model is its response variable, following the vector "rv"

spatial.models.ext<-list()
spatial.models<-list()

#initialize matrix of coefficients (cf), with explanatory variables across the 
#columns and response variables across the rows
cf<-matrix(nrow=5,ncol=6)
colnames(cf)<-c("city.pop","mean.rain","mean.hum","mean.range","mean.mean", "mean.mean.sq")
rownames(cf)<-rv[1:5]

for(i in 1:5){
  if(i ==1){
    fam<-"binomial"
  }
  if(i %in% 2:5){
    fam<-"gaussian"
  }
  if(i %in% c(2:3)){
    data<-lapply(cov.prov, function(x) x[cov.prov$local.trans==1])
    
    data$resp<-as.numeric(unlist(data[rv[i]]))
    org<-data
    data[3:14]<-lapply(data[3:14], zscore)
  }
  
  else{
    data<-cov.prov
    data$resp<-as.numeric(unlist(data[rv[i]]))
    org<-data
    if(i==1){
      data[c(3:11,13)]<-lapply(data[c(3:11,13)], zscore)
    }
    else{
      data[3:14]<-lapply(data[3:14], zscore)
    }
  }
  
  if(i==1){
    df<-data[c("city.pop","mean.rain","mean.hum","mean.range","mean.mean", "mean.mean.sq","country")]
    df$resp<-data$resp
    
    mod<-glmer(resp~city.pop+mean.hum+mean.range+mean.mean+(1|country)-1, data=df, family=fam,nAGQ=0)
    
    spatial.models.ext[[rv[i]]]<-summary(mod)
    spatial.models.ext[[rv[i]]]$data<-data
    spatial.models[[rv[i]]]<-mod
  }
  
  else{  
    mod<-stepAIC(glm(resp~city.pop+mean.rain+mean.hum+mean.mean+mean.range+mean.mean.sq+as.factor(country)-1,family=fam,na.action="na.omit",data=data),trace=F)
    spatial.models.ext[[rv[i]]]<-summary(mod)
    spatial.models.ext[[rv[i]]]$data<-data
    spatial.models[[rv[i]]]<-mod
  }
  
  spatial.models.ext[[rv[i]]][["org"]]<-org
  
  for(c in rownames(spatial.models.ext[[i]]$coefficients)){
    if(c %in% colnames(cf)){
      ind<-which(colnames(cf)==c)
      cf[i,ind]<-spatial.models.ext[[i]]$coefficients[c, 1]
    }
  }
}


#pseudo R2 for local.trans model
mF<-glmer(resp~city.pop+mean.hum+mean.range+mean.mean+(1|country), fam=binomial, data=spatial.models.ext$local.trans$data)
VarF <- var(as.vector(fixef(mF) %*% t(model.matrix(mF))))
lt_rsq_mar<-VarF/(VarF + VarCorr(mF)$'country'[1] + pi^2/3)
lt_rsq_con<-(VarF + VarCorr(mF)$'country'[1])/(VarF + VarCorr(mF)$'country'[1] + pi^2/3)


#pseudo R2 for four other spatial.models
sapply(spatial.models, function(x) nagelkerke(x)$Pseudo.R.squared.for.model.vs.null[3])


#FINDING CONFIDENCE INTERVALS
#response variable
cov.prov<-read.csv("covprov")
rv<-c("local.trans","max.b","avg.b","cum.cases","avg.cases")
ci.models<-list()
cf<-matrix(nrow=5,ncol=6)
colnames(cf)<-c("city.pop","mean.rain","mean.hum","mean.range","mean.mean", "mean.mean.sq")
rownames(cf)<-rv[1:5]


#Start by refitting models without normalizing 
for(i in 2:5){
  if(i %in% 2:5){
    fam<-"gaussian"
  }
  if(i %in% c(2:3)){
    data<-lapply(cov.prov, function(x) x)
    data<-lapply(data, function(x) x[data$local.trans==1])
    data$resp<-as.numeric(unlist(data[rv[i]]))
    org<-data
  }
  
  else{
    data<-cov.prov
    data$resp<-as.numeric(unlist(data[rv[i]]))
    org<-data
  }
  
  data$mean.mean.sq<-data$mean.mean.sq/2
  mod<-stepAIC(glm(resp~city.pop+mean.rain+mean.hum+mean.mean+mean.range+mean.mean.sq+as.factor(country)-1,family=fam,na.action="na.omit",data=data),trace=F)
    
  ci.models[[rv[i]]]<-mod
  ci.model[[rv[i]]][["org"]]<-org
  
  for(c in rownames(ci.model[[i]]$coefficients)){
    if(c %in% colnames(cf)){
      ind<-which(colnames(cf)==c)
      cf[i,ind]<-ci.model[[i]]$coefficients[c, 1]
    }
  }
}


#calculate confidence interval using code by cmbarbu (https://stats.stackexchange.com/users/72156/cmbarbu), How to compute the confidence interval of the ratio of two normal means, URL (version: 2018-08-24): https://stats.stackexchange.com/q/363786
FiellerRatioCI <- function (x, ...) { 
  UseMethod("FiellerRatioCI", x)
}
FiellerRatioCI_basic <- function(a,b,V,alpha=0.05){
  theta <- a/b
  v11 <- V[1,1]
  v12 <- V[1,2]
  v22 <- V[2,2]
  
  z <- qnorm(1-alpha/2)
  g <- z*v22/b^2
  C <- sqrt(v11 - 2*theta*v12 + theta^2 * v22 - g*(v11-v12^2/v22))
  minS <- (1/(1-g))*(theta- g*v12/v22 - z/b * C)
  maxS <- (1/(1-g))*(theta- g*v12/v22 + z/b * C)
  return(c(ratio=-theta,min=-minS,max=-maxS))
}
FiellerRatioCI.glmerMod <- function(model,aname,bname){
  V <- vcov(model)
  a<-as.numeric(unique(coef(model)$culture[aname]))
  b<-as.numeric(unique(coef(model)$culture[bname]))
  return(FiellerRatioCI_basic(a,b,V[c(aname,bname),c(aname,bname)]))
}
FiellerRatioCI.glm <- function(model,aname,bname){
  V <- vcov(model)
  a <- coef(model)[aname]
  b <- coef(model)[bname]
  return(FiellerRatioCI_basic(a,b,V[c(aname,bname),c(aname,bname)]))
}


FiellerRatioCI(ci.models$max.b, "mean.mean", "mean.mean.sq")
FiellerRatioCI(ci.models$avg.b, "mean.mean", "mean.mean.sq")
FiellerRatioCI(ci.models$avg.cases, "mean.mean", "mean.mean.sq")
FiellerRatioCI(ci.models$cum.cases, "mean.mean", "mean.mean.sq")

