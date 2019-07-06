load("cov0701.RData")
library(lme4)

#extract covariates for spatiotemporal model
clim<-c("range","avg","max","min","rain","hum", "avg.sq", "max.sq", "min.sq")

st.vars<-lapply(cov[clim], function(x) unlist(x))
st.vars$pa<-unlist(all_pa)
st.vars$country<-unlist(lapply(1:length(cov$names), function(i) rep(cov$country[[i]],length(all_pa[[i]]))))
st.vars$province<-unlist(lapply(1:length(cov$names), function(i) rep(cov$names[[i]],length(all_pa[[i]]))))
st.vars$betas<-unlist(all_betas)
st.vars$months<-unlist(all_months)
st.vars$dates<-unlist(all_dates)


#returns the best fit model spatiotemporal model for each method of model fitting
#special variable gives the effect of province, where months includes a fixed effect of month, months_country
backwardselect<-function(resp, special){
  vars<-c("rain","hum","avg","avg.sq", "range")

  #initialize dataframe, normalize variables
  if(resp=="pa"){
    fam="binomial"
    data<-as.data.frame(st.vars[c("pa","range", "rain", "hum","max","max.sq","min","min.sq", "avg", "avg.sq","country","province","months","dates")])
    data<-data[apply(data, 1, function(x) sum(is.na(x)))==0,]
    org<-data
    data[,2:10]<-sapply(data[,2:10], zscore)
  } else{
    data<-as.data.frame(st.vars[c("pa", "betas", "range", "rain", "hum","max","max.sq","min","min.sq", "avg", "avg.sq","country","province","months", "dates")])
    data<-data[apply(data, 1, function(x) sum(is.na(x)))==0,]
    data<-data[which(data$pa==1),]
    org<-data
    data[,2:11]<-sapply(data[,2:11,], zscore)
    fam="gaussian"
  }
  
  #fit a mixed effect model with nested effect of province in country with remaining response variables
  
  if(special=="months"){
    df<-data[c(vars, "months")]
    df$resp<-data[[resp]]
    if(resp=="pa"){
      mod<-stepAIC(glm(resp~.+as.factor(months)-1, data=df, family=fam), trace=F)
      nullmod<-glm(resp~1, data=df, family=fam)
      mod$rsquared<-1-logLik(mod)/logLik(nullmod)
    } else {
      mod<-stepAIC(glm(resp~.+as.factor(months)-1, data=df, family=fam), trace=F)
    }
    return(mod)
  }
  
  if(special=="months_country"){
    df<-data[c(vars, "months", "country")]
    df$resp<-data[[resp]]
    if(resp=="pa"){
      mod<-stepAIC(glm(resp~.+(country/months)-1, data=df, family=fam), trace=F)
      nullmod<-glm(resp~1, data=df, family=fam)
      mod$rsquared<-1-logLik(mod)/logLik(nullmod)
    } else {
      mod<-stepAIC(glm(resp~.+(country/months)-1, data=df, family=fam), trace=F)
    }
    return(mod)
  }
  
  if(special=="plm"){
    df<-data[c(vars, "province", "country","dates")]
    df$resp<-data[[resp]]
    df<-pdata.frame(df,index=c("province", "dates"))
    if(resp=="pa"){
      mod<-plm(resp~hum+avg-1, data=df, model="random", effect="twoways", family=fam, index=c("province", "dates"))
      mod$df<-df
      mod$org<-org
    } else {
      mod<-plm(resp~hum+avg+rain-1, data=df, model="random", effect="twoways", family=fam, index=c("province", "dates"))
      mod$df<-df
      mod$org<-org
    }
    return(mod)
  }
}

mod.pa<-backwardselect("pa", "plm")  
phtest(resp~hum+avg-1, data=mod.pa$df, model=c("random", "within"), effect="twoways", family="binomial", index=c("province", "dates"))
mod.inc<-backwardselect("betas", "plm")  
phtest(resp~hum+avg+rain-1, data=mod.inc$df, model=c("random", "within"), effect="twoways", family="gaussian", index=c("province", "dates"))

month.pa<-backwardselect("pa", "months")
month.inc<-backwardselect("betas", "months")
mc.pa<-backwardselect("pa", "months_country")
mc.inc<-backwardselect("betas", "months_country")




