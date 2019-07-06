load("C:/Users/Mallory/OneDrive/SUMMER17/MordecaiLab/cov0701.RData")

#Figure 2, S2 HEATMAPS
library(ggplot2)
library(cowplot)
library(mosaic)
library(gridGraphics)
library(grid)
library(gplots)

p<-list()

cov$cases<-lapply(cov$names, function(x) all.data[[x]]$cases)
cov$dates<-lapply(cov$names, function(x) all.data[[x]]$time)

#month and country labels in order
months<-c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
countries<-c("DOM","SLV","COL","ECU","MEX","GTM")

case.mat<-matrix(nrow=length(cov$names), ncol=(max(unlist(cov$dates))-min(unlist(cov$dates)))/7+1)

#line to demarcate country groupings
country.line<-sapply(unique(cov$country), function(x) max(which(cov$country==x)))

#generating the country labels as a vector of NAs with the country names centered within the country groupings
country.lab<-rep(NA, nrow(case.mat))
country.lab.at<-floor(head(c(0, country.line), -1)+diff(c(0,country.line))/2)
country.lab[country.lab.at]<-countries

#line to demarcate monthly groupings
myr<-format(as.Date(seq(from=min(unlist(cov$dates)),to=max(unlist(cov$dates),"ARG"),by=7), "1970-01-01"),"%Y-%m")
mon.line<-sapply(unique(myr), function(x) max(which(myr==x)))

#generating the month labels
mon.lab<-rep("", n=floor(.66*nrow(case.mat)))
mon.lab.at<-floor(head(c(0, mon.line), -1)+diff(c(0,mon.line))/3)+1
mon.lab.at[1]<-1
mon.lab[mon.lab.at]<-months[sapply(myr[mon.line], function(x) as.integer(substr(toString(x),6,7)))]
mon.lab[c(1, length(mon.lab))]<-NA
mon.lab<-c(NA,NA,mon.lab)

#putting case numbers into matrix form by looping through provinces
for(i in 1:length(cov$names)){
  if(!is.null(cov$cases[[i]])){
    start.wk<-as.numeric(cov$dates[[i]][1]-min(unlist(cov$dates)))/7+1
    case.mat[i,start.wk:(start.wk+length(cov$cases[[i]])-1)]<-cov$cases[[i]]
  }
}

case.mat<-log(case.mat)
case.mat[is.infinite(case.mat)]<--.05
#color settings specific to this project
color.palette  <- c("#F8F8F8", colorRampPalette(c("blue", "purple", "red"))(n=19))
col_breaks = c(-.1,  # for white
               seq(0,6.5,length=20))

#setting dimnesions of heatmap and text size
#png(filename=paste("hgCases.png"))

lmat=rbind(c(0,0,3,0),c(0,1,1,1),c(0,2,4,0)) 
lwid = c(.1,1.5,3,1.5)
lhei = c(.5,4,1)
par(cex.main=1.5)
par(mfrow=c(1,1))



heatmap.2(case.mat, Rowv=FALSE, Colv=FALSE,symkey=FALSE,key.par=list(cex=1.0, cex.lab=1.2, cex.axis=1.2, mar=c(4,1.5,3,5.3)),density.info="none",lmat=lmat, lwid=lwid, lhei=lhei, cexRow=1.8, cexCol=1.8,keysize=1,na.col="gray",dendrogram="none",trace="none",labCol=mon.lab,labRow=country.lab,sepcolor="black",sepwidth=c(.01,.01),rowsep=c(0,country.line), colsep=c(0, mon.line), col=color.palette, breaks=col_breaks, main="A. ln(Weekly Case Incidence)")



###HEATMAP OF FOI###
library(ggplot2)
library(cowplot)
library(mosaic)
library(gridGraphics)
library(grid)
library(gplots)


cov$dates<-lapply(cov$names, function(x) all.data[[x]]$time+35)

#month and country labels in order
months<-c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
countries<-c("DOM","SLV","COL","ECU","MEX","GTM")

beta.mat<-matrix(nrow=length(cov$names), ncol=(max(unlist(cov$dates))-min(unlist(cov$dates)))/7+1)

#line to demarcate country groupings
country.line<-sapply(unique(cov$country), function(x) max(which(cov$country==x)))

#generating the country labels as a vector of NAs with the country names centered within the country groupings
country.lab<-rep(NA, nrow(case.mat))
country.lab.at<-floor(head(c(0, country.line), -1)+diff(c(0,country.line))/2)
country.lab[country.lab.at]<-countries

#line to demarcate monthly groupings
myr<-format(as.Date(seq(from=min(unlist(cov$dates)),to=max(unlist(cov$dates)),by=7), "1970-01-01"),"%Y-%m")
mon.line<-sapply(unique(myr), function(x) max(which(myr==x)))

#generating the month labels
mon.lab<-rep("", n=floor(.66*nrow(case.mat)))
mon.lab.at<-floor(head(c(0, mon.line), -1)+diff(c(0,mon.line))/3)+1
mon.lab.at[1]<-1
mon.lab[mon.lab.at]<-months[sapply(myr[mon.line], function(x) as.integer(substr(toString(x),6,7)))]
mon.lab<-c(NA,NA,mon.lab)

#putting case numbers into matrix form by looping through provinces
for(i in 1:length(cov$names)){
  if(!is.null(cov$betas[[i]])){
    start.wk<-as.numeric(cov$dates[[i]][1]-min(unlist(cov$dates)))/7+1
    beta.mat[i,start.wk:(start.wk+length(cov$betas[[i]])-1)]<-cov$betas[[i]]
  }
}

beta.mat[is.infinite(beta.mat)]<--2.65

beta.mat<-round(beta.mat, digits=2)

#color settings specific to this project
color.palette  <- c("#F8F8F8", colorRampPalette(c("blue", "purple", "red"))(n=69))
col_breaks = c(-2.65,  # for white
               seq(-2.599,-1.6,length=10),
               seq(-1.599, -.6,length=10),
               seq(-.599,.599,length=10),
               seq(.6,1.599,length=10),
               seq(1.6,2.599,length=10),
               seq(2.6,3.599,length=10),
               seq(3.6,4.599,length=10))

#beta.mat<-t(apply(beta.mat, 1, function(x) x/max(x,na.rm=TRUE)))

#setting dimnesions of heatmap and text size
lmat=rbind(c(0,0,3,0),c(0,1,1,1),c(0,2,4,0)) 
lwid = c(.1,1.5,3,1.5)
lhei = c(.5,4,1)
par(cex.main=1.5)


heatmap.2(beta.mat, Rowv=FALSE, Colv=FALSE,symkey=FALSE,key.par=list(cex=1.0, cex.lab=1.2, cex.axis=1.2, mar=c(4,1.5,3,5.3)),density.info="none",lmat=lmat, lwid=lwid, lhei=lhei, cexRow=1.8, cexCol=1.8,keysize=1,na.col="gray",dendrogram="none",trace="none",labCol=mon.lab,labRow=country.lab,sepcolor="black",sepwidth=c(.01,.01),rowsep=c(0,country.line), colsep=c(0, mon.line), col=color.palette, breaks=col_breaks, main=expression(bold(paste("B. ln(Weekly ",beta[t], ")", sep=""))))

#loop through climate variables
clim_var<-c("hum", "range", "rain", "avg")
clim_names<-c("A. Weekly Humidity", "B. Weekly Temperature Range", "C. ln(Weekly Rainfall)", "D.  Weekly Mean Temperature")
for(clim in clim_var){
  clim.mat<-matrix(nrow=length(cov$names), ncol=(max(unlist(cov$dates))-min(unlist(cov$dates)))/7+1)
  
  #putting case numbers into matrix form by looping through provinces
  for(i in 1:length(cov$names)){
    if(!is.null(cov[[clim]][[i]])){
      start.wk<-as.numeric(cov$dates[[i]][1]-min(unlist(cov$dates)))/7+1
      clim.mat[i,start.wk:(start.wk+length(cov[[clim]][[i]])-1)]<-cov[[clim]][[i]]
    }
  }
  
  if(clim=="rain"){
    clim.mat<-log(clim.mat+min(clim.mat[clim.mat>0], na.rm=TRUE))
  }
  
  
  #clim_mat<-zscore(clim.mat, na.rm=TRUE)
  
  #setting dimnesions of heatmap and text size
  lmat=rbind(c(0,0,3,0),c(0,1,1,1),c(0,2,4,0)) 
  lwid = c(.1,1.5,3,1.5)
  lhei = c(.5,4,1)
  par(cex.main=1.5)
  par(mfrow=c(1,1))
  clim_name<-clim_names[which(clim_var==clim)]
  
  heatmap.2(clim.mat, Rowv=FALSE, Colv=FALSE,key.par=list(cex=1.0, cex.lab=1.2, cex.axis=1.2, mar=c(4,1.5,3,5.3)), density.info="none",lmat=lmat, lwid=lwid, lhei=lhei, cexRow=1.8, cexCol=1.8,keysize=1,na.col="gray",dendrogram="none",trace="none",labCol=mon.lab,labRow=country.lab,sepcolor="black",sepwidth=c(.01,.01),rowsep=c(0,country.line), colsep=c(0, mon.line), col=colorpanel(20,"blue", "purple", "red"), main=clim_name)
}

#MAPS FOR SPATIAL MODEL (FIGURE 3)

require(maps)
require(rgdal)
map("world", xlim=c(-120, -60), ylim=c(-5, 35), fill=TRUE, col="gray",border=NA)
var<-c("local.trans", "avg.b","mean.mean")
title<-c("Local Transmission", "ln(Mean(  ))", "Mean Temperature")
country.codes<-c("DOM","SLV","COL","ECU","MEX","GTM")

map("world", xlim=c(-72, -68), ylim=c(17.5, 20.5), fill=TRUE, col="gray",border=NA)

map("world", xlim=c(-120, -65), ylim=c(-5, 35), fill=TRUE, col="gray",border=NA)




for(j in 1:3){
  p<-list()
  l<-list()
  png(filename=paste("3",var[j],".png", sep=""), width=1800, height=1320, res=144)
  map("world", xlim=c(-120, -65), ylim=c(-5, 35), fill=TRUE, col="gray",border=NA)
  
  
  for(i in 1:6){
    #shapefiles are ordered differently from epidemiological data we've generated. Use switch statement to reorder.
    correct.order<-switch(country.codes[i],
                          "ARG"=c(1,6,2,21,13,4,7,9,17,24,5),
                          "COL"=c(22,17,7,30,29,12,28,19,2,20,27,9,14,18,3,25,10,23,8,1,26,6,24,21,31,13), 
                          "DOM"=c(5,10,3,21,30,1,25,28,8,14,17,20,23,32,15,27,6,19,13,11,2,4,12,7,24,22,29,16,18,9,26),
                          "ECU"=c(22,21,20,19,14,13,10,9,8,7,2,11,5,1),
                          "GTM"=c(22,20,9,13,5,6,4,1,18,15,8,16,11,10,17,2,19,3),
                          "MEX"=c(1,3,2,4,5,7,8,10,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30,31,32),
                          "SLV"=c(2:7, 9:14),      
                          "VIR"=c(3,1,2)
    )
    #read in shapefile, identify centroid, and store coordinates. we will use this to find nearest weather station.
    readOGR(dsn=paste("C:/Users/Mallory/OneDrive/SUMMER17/MordecaiLab/map/",country.codes[i],"_adm1.shp",sep="")) ->map
    
    if(j==3){
      col_fun<-colorRampPalette( c( "blue", "red" ) )(2000)
      my_col<-col_fun[sapply(cov$mean.mean, function(x) round(x*100-950,digits=0))]
      letr="C"
    }
    if(j==2){
      col_fun<-colorRampPalette( c( "blue", "red" ) )(1140)
      my_col<-col_fun[sapply(cov$avg.b, function(x) round((x+4)*200,digits=0))]
      my_col[cov$local.trans==0]<-"white"
      letr="B"
    }
    
    if(j==1){
      my_col<-ifelse(cov$local.trans==0, "blue", "red")
      letr="A"
    }
    
    plot(map[correct.order,], col=my_col[cov$country==unique(cov$country)[i]], add=TRUE, lwd=2)
    
  }
  rect(-110,3,-97.5, 13, lwd=4)
  rect(-75.5, 23, -65.5, 30.5, lwd=4)
  
  rect(-92.5, 12.5, -87.5, 16.5, lwd=2)
  rect(-72, 17.5, -68, 20.5, lwd=2)
  segments(-110, 13, -92.5, 16.5, lwd=2)
  segments(-97.5, 3, -87.5, 12.5, lwd=2)
  segments(-75.5, 23, -72, 20.5, lwd=2)
  segments(-65.5, 23, -68, 20.5, lwd=2)
  title(paste(letr,title[j], sep=". "), cex.main=3, adj=0)
  dev.off()
  
  png(filename=paste("3",var[j],"inset1.png", sep=""),width=384, height=312)
  par(oma=c(0,0,0,0), mai=c(0,0,0,0))
  map("world", xlim=c(-92.5, -87.5), ylim=c(12.5, 16.5), fill=TRUE, col="gray",border=NA)
  for(i in c(2,6)){
    #shapefiles are ordered differently from epidemiological data we've generated. Use switch statement to reorder.
    correct.order<-switch(country.codes[i],
                          "ARG"=c(1,6,2,21,13,4,7,9,17,24,5),
                          "COL"=c(22,17,7,30,29,12,28,19,2,20,27,9,14,18,3,25,10,23,8,1,26,6,24,21,31,13), 
                          "DOM"=c(5,10,3,21,30,1,25,28,8,14,17,20,23,32,15,27,6,19,13,11,2,4,12,7,24,22,29,16,18,9,26),
                          "ECU"=c(22,21,20,19,14,13,10,9,8,7,2,11,5,1),
                          "GTM"=c(22,20,9,13,5,6,4,1,18,15,8,16,11,10,17,2,19,3),
                          "MEX"=c(1,3,2,4,5,7,8,10,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30,31,32),
                          "SLV"=c(2:7, 9:14)   
    )
    #read in shapefile, identify centroid, and store coordinates. we will use this to find nearest weather station.
    readOGR(dsn=paste("C:/Users/Mallory/OneDrive/SUMMER17/MordecaiLab/map/",country.codes[i],"_adm1.shp",sep="")) ->map
    
    if(j==3){
      col_fun<-colorRampPalette( c( "blue", "red" ) )(2000)
      my_col<-col_fun[sapply(cov$mean.mean, function(x) round(x*100-950,digits=0))]
    }
    if(j==2){
      col_fun<-colorRampPalette( c( "blue", "red" ) )(1140)
      my_col<-col_fun[sapply(cov$avg.b, function(x) round((x+4)*200,digits=0))]
      my_col[cov$local.trans==0]<-"white"
    }
    
    if(j==1){
      my_col<-ifelse(cov$local.trans==0, "blue", "red")
    }
    
    plot(map[correct.order,], col=my_col[cov$country==unique(cov$country)[i]], add=TRUE, lwd=3)
    
  }
  dev.off()
  
  png(filename=paste("3",var[j],"inset2.png", sep=""), height=240, width=312)
  par(oma=c(0,0,0,0), mai=c(0,0,0,0))
  map("world", xlim=c(-72, -68), ylim=c(17.5, 20.5), fill=TRUE, col="gray",border=NA)
  
  for(i in 1){
    #shapefiles are ordered differently from epidemiological data we've generated. Use switch statement to reorder.
    correct.order<-switch(country.codes[i],
                          "ARG"=c(1,6,2,21,13,4,7,9,17,24,5),
                          "COL"=c(22,17,7,30,29,12,28,19,2,20,27,9,14,18,3,25,10,23,8,1,26,6,24,21,31,13), 
                          "DOM"=c(5,10,3,21,30,1,25,28,8,14,17,20,23,32,15,27,6,19,13,11,2,4,12,7,24,22,29,16,18,9,26),
                          "ECU"=c(22,21,20,19,14,13,10,9,8,7,2,11,5,1),
                          "GTM"=c(22,20,9,13,5,6,4,1,18,15,8,16,11,10,17,2,19,3),
                          "MEX"=c(1,3,2,4,5,7,8,10,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30,31,32),
                          "SLV"=c(2:7, 9:14)   
    )
    #read in shapefile, identify centroid, and store coordinates. we will use this to find nearest weather station.
    readOGR(dsn=paste("C:/Users/Mallory/OneDrive/SUMMER17/MordecaiLab/map/",country.codes[i],"_adm1.shp",sep="")) ->map
    
    if(j==3){
      col_fun<-colorRampPalette( c( "blue", "red" ) )(2000)
      my_col<-col_fun[sapply(cov$mean.mean, function(x) round(x*100-950,digits=0))]
    }
    if(j==2){
      col_fun<-colorRampPalette( c( "blue", "red" ) )(1140)
      my_col<-col_fun[sapply(cov$avg.b, function(x) round((x+4)*200,digits=0))]
      my_col[cov$local.trans==0]<-"white"
    }
    
    if(j==1){
      my_col<-ifelse(cov$local.trans==0,"blue", "red")
    }
    
    plot(map[correct.order,], col=my_col[cov$country==unique(cov$country)[i]], add=TRUE, lwd=3)
    
  }
  dev.off()
}



png(filename=paste("3Ckey.png"), height=585, width=280)
col_fun<-colorRampPalette( c( "blue", "red" ) )(2000)
z=matrix(1:2000,nrow=1)
x=1
y=seq(9.5,29.5,len=2000) 
image(x,y,z,col=col_fun,axes=FALSE,xlab="Mean Temp(°C)",ylab="", cex.lab=3)
axis(2, cex.axis=3)
dev.off()


png(filename="3Bkey.png", height=585, width=280)
col_fun<-colorRampPalette( c( "blue", "red" ) )(1140)
z=matrix(1:1140,nrow=1)
x=1
y=seq(-4,1.7,len=1140)
image(x,y,z,col=col_fun,axes=FALSE,xlab=expression(paste("Mean(FOI)")),ylab="", cex.lab=3)
axis(2, cex.axis=3)
dev.off()

png(filename="3Akey.png", height=585, width=340)
z=matrix(1:2, nrow=1)
x=1
y=seq(0,1)
col_fun<-colorRampPalette(c("blue", "red"))(2)
image(x,y,z, col=col_fun, axes=FALSE, xlab="Local Transmission", ylab=NA, cex.lab=3)
axis(2, cex.axis=3, at=c(0,1), las=2)
dev.off()
#FIG 4: SPATIAL MODEL COEFFICIENT PLOT
library(coefplot)
#get key & plots here
par(mai=c(8,8,8,8), cex.main=5)
facs<-sapply(unique(cov.prov$country), function(x) paste("as.factor(country)",x,sep=""))
facnames<-c("as.factor(country)Dominican_Republic"="Country - Dominican Republic","as.factor(country)El_Salvador"="Country - El Salvador","as.factor(country)Colombia"="Country - Colombia", "as.factor(country)Ecuador"="Country - Ecuador","as.factor(country)Mexico"="Country - Mexico","as.factor(country)Guatemala"="Country - Guatemala")
varnames<-c(city.pop="Population", mean.rain="Rainfall", mean.hum="Humidity",mean.range="Range",mean.mean="Mean Temperature",mean.mean.sq="Mean Temperature²")


coefplot::multiplot(spatial.models[[rv[1]]], spatial.models[[rv[2]]], spatial.models[[rv[3]]],spatial.models[[rv[4]]],spatial.models[[rv[5]]], title="Spatial Model Coefficients",ylab="",sort="magnitude", innerCI=0, lwdOuter=2, numberAngle=90, pointSize=5, coefficients=c("city.pop", "mean.rain","mean.hum","mean.range","mean.mean","mean.mean.sq",facs), names=c("E", "D", "C","B","A"), newNames=c(varnames,facnames))+ theme(panel.background = element_rect(fill = 'white', colour="black", size=3))+geom_hline(yintercept=c(1:11+.5))+theme(text = element_text(size=30), axis.text.x = element_text(angle=0, vjust=1, size=30), axis.text.y = element_text(size=30)) 


#get text for key here
coefplot::multiplot(spatial.models[[rv[1]]], spatial.models[[rv[2]]], spatial.models[[rv[3]]],spatial.models[[rv[4]]],spatial.models[[rv[5]]], title="",ylab="",innerCI=0,numberAngle=90, coefficients=c("city.pop", "mean.rain","mean.hum","mean.range","mean.mean","mean.mean.sq"), cex=1,names=c("Local Transmission", "ln(Max(  ))", "ln(Mean(  ))","Cumulative Cases","Mean Weekly Cases"),newNames=varnames)+ theme(panel.background = element_rect(fill = 'white', colour="black", size=1))+geom_hline(yintercept=c(1.5,2.5,3.5,4.5,5.5))+theme(text = element_text(size=30),axis.text.x = element_text(angle=0, vjust=1))



#FIG 5: CONFIDENCE INTERVAL PLOT

pred_temp<-seq(15, 35, by=.01)
resp_names<-c(NA,"A. ln(Max(  ))","B. ln(Mean(  ))", "Cumulative Cases", "Mean Weekly Cases")
png(filename=paste("5.png"), height=1100, width=2200, res=144)
par(mfrow=c(1,2), mai=c(1,1,.5,.5))
for(i in 2:3){
  respvar<-rv[i]
  
  temp_z<-(pred_temp-mean(spatial.models.ext[[respvar]]$org$mean.mean))/sd(spatial.models.ext[[respvar]]$org$mean.mean)
  temp_z.sq<-(pred_temp*pred_temp-mean(spatial.models.ext[[respvar]]$org$mean.mean.sq))/sd(spatial.models.ext[[respvar]]$org$mean.mean.sq)
  for(my_country in unique(cov$country)){
    newData<-data.frame(mean.mean=temp_z, mean.mean.sq=temp_z.sq, mean.range=rep(0, length.out=length(temp_z)), mean.hum=rep(0, length.out=length(temp_z)), mean.rain=rep(0, length.out=length(temp_z)), city.pop=rep(0, length.out=length(temp_z)), country=rep(my_country, length.out=length(temp_z)))
    my_predmat<-predict.lm(spatial.models[[respvar]], newData, level=.95, interval="confidence")
    if(my_country=="Dominican_Republic"){
      best_predmat<-my_predmat
    }
    else{
      best_predmat[,"lwr"]<-ifelse(best_predmat[,"lwr"]<my_predmat[,"lwr"], best_predmat[,"lwr"], my_predmat[,"lwr"])
      best_predmat[,"upr"]<-ifelse(best_predmat[,"upr"]>my_predmat[,"upr"], best_predmat[,"upr"], my_predmat[,"upr"])
    }
  }
  predmat<-(best_predmat-min(best_predmat))/diff(range(best_predmat))
  
  plot(pred_temp, predmat[,"fit"], ylim=range(predmat), col="blue", type="l", ylab=paste("ln(",ifelse(i==2, "Max", "Mean"), "(   ))-Scaled", sep=""), main=resp_names[i], cex.main=2, xlab=expression(paste("Mean Temperature (", degree, "C)")), cex.axis=2, lwd=2, cex.lab=2)
  if(i==2){
    temp.maxes<-c(22.02, 26.90)
    where.max<-sapply(temp.maxes, function(x) which(pred_temp<x+.009 & pred_temp>x-.009))
  }
  if(i==3){
    temp.maxes<-c(22.20, 25.49)
    where.max<-sapply(temp.maxes, function(x) which(pred_temp<x+.009 & pred_temp>x-.009))
  }
  
  
  polygon(c(seq(temp.maxes[1], temp.maxes[2], by=.01), seq(temp.maxes[2], temp.maxes[1], by=-.01)), c(predmat[where.max[1]:where.max[2],2], predmat[where.max[2]:where.max[1],3]), col="gray")
  
  lines(pred_temp, predmat[,"fit"], col="blue",lty=1, lwd=2)
  
  lines(pred_temp, predmat[,"lwr"], col="red", lty=2, lwd=2)
  lines(pred_temp, predmat[,"upr"], col="red", lty=2, lwd=2)
  
  if(respvar=="avg.b"){
    mod.null<-glm(resp~city.pop+as.factor(country)-1, data=spatial.models.ext[[respvar]]$data)
  }
  if(respvar=="max.b"){
    mod.null<-glm(resp~as.factor(country)-1, data=spatial.models.ext[[respvar]]$data)
  }

  F_stats<-anova(spatial.models[[respvar]], mod.null, test="F")
  text(x=25, y=.1, paste("F-statistic: ", signif(F_stats$F[2], digits=3), "\np<", ifelse(i==2, "0.01", "0.005"), sep=""), cex=2)
  #rect(18, 32, 41, .2)
}

dev.off()


#FIGURE S3: PREDICTED VS ACTUAL
actual_cases<-list()
for(i in 8){
  actual_cases<-tail(all.data[[cov$names[i]]]$cases, -5)
  pred_cases<-(exp(all_betas[[i]])*all_S[[i]]*all_Ieff[[i]]^.74)/all_N[[i]]
  dot_type<-ifelse(actual_cases==round(pred_cases), 1, 19)
  plot(all.data[[i]]$time[1:length(actual_cases)]+35, actual_cases, ylab="Cases", xlab="Time", type="l", pch=dot_type, col="red")
  lines(all.data[[i]]$time[1:length(pred_cases)]+35, pred_cases, col="black")
}
