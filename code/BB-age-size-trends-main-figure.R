##=====================================================================##
##                                                                     ##
##   Figure of changes in mean size, age composition, and size-at-age  ##
##                                                                     ##
##=====================================================================##
pkgs<-c("tidyverse","dplyr","rlist","pracma","mgcv","visreg")
if(length(setdiff(pkgs,rownames(installed.packages())))>0) { install.packages(setdiff(pkgs,rownames(installed.packages())),dependencies=T) }
invisible(lapply(pkgs,library,character.only=T))
homeDir<-here::here()
setwd(homeDir)
options(dplyr.summarise.inform=F)

##=====================================================================##
##================================================================## data
##=====================================================================##
rivers<-c("Igushik","Wood","Nushagak","Kvichak","Naknek","Egegik","Ugashik")
nS<-length(rivers)
##==================================================## age-length dataset
data<-read.csv("output/data_used_by_brood_year_all.csv")[,-1] 
years<-sort(unique(data$year))
ages<-unique(data$age)
##===========================================================## mean size
meansize_Y<-read.csv("output/Mean_size_BB_wide_by_brood_year_all.csv")[,-1]
meansize_YS<-read.csv("output/Mean_size_by_system_by_brood_year_all.csv")[,-1] %>% 
  dplyr::select(year,all_of(rivers))
##============================================## mean size-at-age anomaly
SaA_anom_Y<-read.csv("output/SaA_anomaly_BB_wide_by_brood_year_all.csv")[,-1]
SaA_anom_YS<-read.csv("output/SaA_anomaly_by_system_by_brood_year_all.csv")[,-1] %>%
  dplyr::select(year,all_of(rivers))
##======================================================## mean ocean age
meanSWage_Y<-data %>% group_by(year) %>% 
  summarize(meanage=mean(rep(as.numeric(Salt.Water.Age),Count),na.rm=T)) %>%
  data.frame()
##=================================================## mean freshwater age
meanFWage_Y<-data %>% group_by(year) %>% 
  summarize(meanage=mean(rep(as.numeric(Fresh.Water.Age),Count),na.rm=T)) %>%
  data.frame()
##============================================## mean ocean age by system
dat<-dplyr::select(data,year,Salt.Water.Age,Count,length,stock)
meansaltage_YS<-dat %>% 
  group_by(stock,year) %>% 
  summarize(meanage=mean(rep(Salt.Water.Age,Count))) %>% 
  pivot_wider(names_from=stock,values_from=meanage) %>% 
  dplyr::select(year,all_of(rivers)) %>%  
  arrange(year) %>%
  data.frame()
##=======================================## mean freshwater age by system
dat<-dplyr::select(data,year,Fresh.Water.Age,Count,length,stock)
meanfreshage_YS<-dat %>% 
  group_by(stock,year) %>% 
  summarize(meanage=mean(rep(Fresh.Water.Age,Count))) %>%
  pivot_wider(names_from=stock,values_from=meanage) %>% 
  dplyr::select(year,all_of(rivers)) %>% 
  arrange(year) %>%
  data.frame()

##=====================================================================##
##===============================================================## plots
##=====================================================================##
cols<-c("#855A27","#C7AC80","#8D9121","#629D9B","#178BC9","#536373","#AE2633") 

##=====================================================================##
##====================## main figure with all trends by rivers as columns
##=====================================================================##
pdf("plots/Size-and-age-trends-by-river.pdf",width=8.5,height=4) 
layout(matrix(seq((nS+1)*4),ncol=(nS+1),nrow=4,byrow=T))
par(mar=c(0.3,0.3,0,0),oma=c(3.5,4,2,0.5),mgp=c(1.5,0.5,0),tcl=-0.3,lwd=0.5)
xlab<-"Brood year"
xlim<-c(min(years),max(years))
lws.l<-0.5;cols.l<-cols ## raw data lines
lwd.g<-1.5;cols.gl<-alpha(cols,0.8);cols.gp<-alpha(cols,0.2);edge<-"white"
cols.bwl<-alpha(1,0.8);cols.bwp<-alpha(1,0.2)
cex.a<-0.8;cex.l<-0.7;cex.t<-0.75;cex.p<-0.4
alpha.ci<-0.05 ## 1-alpha is the confidence interval
gamma<-1.15 ## smoothing parameter in GAMs (default=1)
points<-FALSE ## circles/points for time series
lines<-TRUE ## lines for time series
##===========================================================## mean size
ylim<-c(485,600)
##------------------------------------------------------------## bay-wide
x<-meansize_Y[,1]
y<-meansize_Y[,2]
plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
gam_y<-gam(y~s(x),kmethod="REML",gamma=gamma)
gam_pred<-visreg(gam_y,plot=F,alpha=alpha.ci)
low<-gam_pred$fit$visregLwr
upp<-gam_pred$fit$visregUpr
fit<-gam_pred$fit$visregFit
yrs<-gam_pred$fit$x
poly.x<-c(yrs,rev(yrs))
poly.y<-c(low,rev(upp))
polygon(poly.x,poly.y,lwd=0.1,col=cols.bwp,border=edge)
lines(yrs,fit,lwd=lwd.g,col=cols.bwl)
if(points) points(x,y,pch=21,lwd=0.2,col=1,cex=cex.p,bg="black") 
if(lines) lines(x,y,lty=1,lwd=lws.l,col=1) 
axis(2,at=seq(500,600,20),label=T,cex.axis=cex.a,las=2,lwd=0.5)
mtext("Mean size\n(mm)",side=2,line=2,cex=cex.l)
mtext("bay-wide ",side=3,line=0.5,cex=cex.t,font=2,col=1)
box(lwd=1)
##--------------------------------------------------------------## rivers
for(s in 1:nS){
x<-meansize_YS[,1]
y<-meansize_YS[,s+1]
plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
gam_y<-gam(y~s(x),kmethod="REML",gamma=gamma)
gam_pred<-visreg(gam_y,plot=F,alpha=alpha.ci)
low<-gam_pred$fit$visregLwr
upp<-gam_pred$fit$visregUpr
fit<-gam_pred$fit$visregFit
yrs<-gam_pred$fit$x
poly.x<-c(yrs,rev(yrs))
poly.y<-c(low,rev(upp))
polygon(poly.x,poly.y,lwd=0.1,col=cols.gp[s],border=edge)
lines(yrs,fit,lwd=lwd.g,col=cols.gl[s])
if(points) points(x,y,pch=21,lwd=0.2,col=1,cex=cex.p,bg=cols.l[s]) 
if(lines) lines(x,y,lty=1,lwd=lws.l,col=cols.l[s])
mtext(paste0(rivers[s]," "),side=3,line=0.5,cex=cex.t,font=1,col=cols[s])
box()
}
##=========================================## average size-at-age anomaly
ylim<-c(-26,30)
##------------------------------------------------------------## bay-wide
x<-SaA_anom_Y[,1]
y<-SaA_anom_Y[,2]
plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
gam_y<-gam(y~s(x),kmethod="REML",gamma=gamma)
gam_pred<-visreg(gam_y,plot=F,alpha=alpha.ci)
low<-gam_pred$fit$visregLwr
upp<-gam_pred$fit$visregUpr
fit<-gam_pred$fit$visregFit
yrs<-gam_pred$fit$x
poly.x<-c(yrs,rev(yrs))
poly.y<-c(low,rev(upp))
polygon(poly.x,poly.y,lwd=0.1,col=cols.bwp,border=edge)
lines(yrs,fit,lwd=lwd.g,col=cols.bwl)
if(points) points(x,y,pch=21,lwd=0.2,col=1,cex=cex.p,bg="black")
if(lines) lines(x,y,lty=1,lwd=lws.l,col=1) 
axis(2,at=seq(-20,30,10),label=T,cex.axis=cex.a,las=2,lwd=0.5)
mtext("Mean size-at-age\nanomaly (mm)",side=2,line=2,cex=cex.l)
box(lwd=1)
##--------------------------------------------------------------## rivers
for(s in 1:nS){
x<-SaA_anom_YS[,1]
y<-SaA_anom_YS[,s+1]
plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
gam_y<-gam(y~s(x),kmethod="REML",gamma=gamma)
gam_pred<-visreg(gam_y,plot=F,alpha=alpha.ci)
low<-gam_pred$fit$visregLwr
upp<-gam_pred$fit$visregUpr
fit<-gam_pred$fit$visregFit
yrs<-gam_pred$fit$x
poly.x<-c(yrs,rev(yrs))
poly.y<-c(low,rev(upp))
polygon(poly.x,poly.y,lwd=0.1,col=cols.gp[s],border=edge)
lines(yrs,fit,lwd=lwd.g,col=cols.gl[s])
if(points) points(x,y,pch=21,lwd=0.2,col=1,cex=cex.p,bg=cols.l[s]) 
if(lines) lines(x,y,lty=1,lwd=lws.l,col=cols.l[s]) 
box()
}
##======================================================## mean ocean age
ylim<-c(1.9,3.1)
##------------------------------------------------------------## bay-wide
x<-meanSWage_Y[,1]
y<-meanSWage_Y[,2]
plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
gam_y<-gam(y~s(x),kmethod="REML",gamma=gamma)
gam_pred<-visreg(gam_y,plot=F,alpha=alpha.ci)
low<-gam_pred$fit$visregLwr
upp<-gam_pred$fit$visregUpr
fit<-gam_pred$fit$visregFit
yrs<-gam_pred$fit$x
poly.x<-c(yrs,rev(yrs))
poly.y<-c(low,rev(upp))
polygon(poly.x,poly.y,lwd=0.1,col=cols.bwp,border=edge)
lines(yrs,fit,lwd=lwd.g,col=cols.bwl)
if(points) points(x,y,pch=21,lwd=0.2,col=1,cex=cex.p,bg="black")
if(lines) lines(x,y,lty=1,lwd=lws.l,col=1)
axis(2,at=seq(1,4,0.2),label=T,cex.axis=cex.a,las=2,lwd=0.5)
mtext("Mean ocean\nage (years)",side=2,line=2,cex=cex.l)
box(lwd=1)
##--------------------------------------------------------------## rivers
for(s in 1:nS){
x<-meansaltage_YS[,1]
y<-meansaltage_YS[,s+1]
plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
gam_y<-gam(y~s(x),kmethod="REML",gamma=gamma)
gam_pred<-visreg(gam_y,plot=F,alpha=alpha.ci)
low<-gam_pred$fit$visregLwr
upp<-gam_pred$fit$visregUpr
fit<-gam_pred$fit$visregFit
yrs<-gam_pred$fit$x
poly.x<-c(yrs,rev(yrs))
poly.y<-c(low,rev(upp))
polygon(poly.x,poly.y,lwd=0.1,col=cols.gp[s],border=edge)
lines(yrs,fit,lwd=lwd.g,col=cols.gl[s])
if(points) points(x,y,pch=21,lwd=0.2,col=1,cex=cex.p,bg=cols.l[s]) 
if(lines) lines(x,y,lty=1,lwd=lws.l,col=cols.l[s])
box()
}
##=================================================## mean freshwater age
ylim<-c(0.4,2.1)
##------------------------------------------------------------## bay-wide
x<-meanFWage_Y[,1]
y<-meanFWage_Y[,2]
plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
gam_y<-gam(y~s(x),kmethod="REML",gamma=gamma)
gam_pred<-visreg(gam_y,plot=F,alpha=alpha.ci)
low<-gam_pred$fit$visregLwr
upp<-gam_pred$fit$visregUpr
fit<-gam_pred$fit$visregFit
yrs<-gam_pred$fit$x
poly.x<-c(yrs,rev(yrs))
poly.y<-c(low,rev(upp))
polygon(poly.x,poly.y,lwd=0.1,col=cols.bwp,border=edge)
lines(yrs,fit,lwd=lwd.g,col=cols.bwl)
if(points) points(x,y,pch=21,lwd=0.2,col=1,cex=cex.p,bg="black") 
if(lines) lines(x,y,lty=1,lwd=lws.l,col=1) 
axis(2,at=seq(0,3,0.5),label=T,cex.axis=cex.a,las=2,lwd=0.5)
mtext("Mean freshwater\nage (years)",side=2,line=2,cex=cex.l)
axis(1,at=seq(1960,2020,10),label=T,cex.axis=cex.a,las=2,lwd=0.5)
mtext("Brood year",side=1,line=2.5,cex=0.75)
box(lwd=1)
##--------------------------------------------------------------## rivers
for(s in 1:nS){
x<-meanfreshage_YS[,1]
y<-meanfreshage_YS[,s+1]
plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
gam_y<-gam(y~s(x),kmethod="REML",gamma=gamma)
gam_pred<-visreg(gam_y,plot=F,alpha=alpha.ci)
low<-gam_pred$fit$visregLwr
upp<-gam_pred$fit$visregUpr
fit<-gam_pred$fit$visregFit
yrs<-gam_pred$fit$x
poly.x<-c(yrs,rev(yrs))
poly.y<-c(low,rev(upp))
polygon(poly.x,poly.y,lwd=0.1,col=cols.gp[s],border=edge)
lines(yrs,fit,lwd=lwd.g,col=cols.gl[s])
if(points) points(x,y,pch=21,lwd=0.2,col=1,cex=cex.p,bg=cols.l[s]) 
if(lines) lines(x,y,lty=1,lwd=lws.l,col=cols.l[s]) 
axis(1,at=seq(1960,2020,10),label=T,cex.axis=cex.a,las=2,lwd=0.5)
mtext("Brood year",side=1,line=2.5,cex=0.75)
box()
}
##-----------------------------------------------------------## save plot
dev.off()

##=====================================================================##
##=====================================================================##
##=====================================================================##