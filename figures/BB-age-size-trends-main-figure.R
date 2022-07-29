##=====================================================================##
##                                                                     ##
##   Figure of changes in mean size, age composition, and size-at-age  ##
##                                                                     ##
##=====================================================================##
# rm(list=ls(all=T)) 
pkgs<-c("tidyverse","dplyr","rlist","nlme","MuMIn","visreg","effects", "RColorBrewer","brms","heavy","splitstackshape","cowplot","svglite", "pracma","mgcv","gratia")
if(length(setdiff(pkgs,rownames(installed.packages())))>0) { install.packages(setdiff(pkgs,rownames(installed.packages())),dependencies=T) }
invisible(lapply(pkgs,library,character.only=T))
if(exists("homeDir")) { } else { homeDir<-setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) }
options(dplyr.summarise.inform=F)

year.type<-"brood" ## "brood" or "run"
catch.type<-"all" ## 'esc' or 'harv' or 'all' 
dropAlagnak<-TRUE

##=====================================================================##
##================================================================## data
##=====================================================================##
if(dropAlagnak) { rivers<-c("Igushik","Wood","Nushagak","Kvichak","Naknek","Egegik","Ugashik") } else { rivers<-c("Igushik","Wood","Nushagak","Kvichak","Alagnak","Naknek","Egegik","Ugashik") } # "Togiak"
##===========================================================## directory
subDir<-paste(year.type,"year",catch.type,"data")
dir.create(file.path(paste0(homeDir,"/observed/",subDir)),showWarnings=F)
setwd(file.path(paste0(homeDir,"/observed/",subDir)))
##==================================================## age-length dataset
data<-read.csv("data_used.csv")[,-1]
stocks<-unique(data$stock);nS<-length(stocks)
ages<-unique(data$age);nA<-length(ages)
years<-unique(data$year);nY<-length(years)
##==================================================## mean size each year
meansize_Y<-read.csv("Mean_size_BB_wide.csv")[,-1]
meansize_YS<-read.csv("Mean_size_by_system.csv")[,-1] %>% select(year,all_of(rivers)) %>% data.frame
##===================================## mean size-at-age anomaly each year
SaA_anomaly_Y<-read.csv("SaA_anomaly_BB_wide.csv")[,-1]
SaA_anomaly_YS<-read.csv("SaA_anomaly_by_system.csv")[,-1] %>% select(year,all_of(rivers)) %>% data.frame
##===================================================## summarize BB-wide
## aggregate individual observations for all of BB
##==================================================## mean saltwater age
meanSWage_Y<-data %>% group_by(year) %>% summarize(meanage=mean(rep(as.numeric(Salt.Water.Age),Count),na.rm=T)) %>% data.frame
##=================================================## mean freshwater age
meanFWage_Y<-data %>% group_by(year) %>% summarize(meanage=mean(rep(as.numeric(Fresh.Water.Age),Count),na.rm=T)) %>% data.frame
##=================================================## mean total age
meanage_Y<-data %>% group_by(year) %>% summarize(meanage=mean(rep(as.numeric(Total.Age),Count),na.rm=T)) %>% data.frame
##=================================================## summarize by system
## aggregate by year and stock/system
##=====================================================## mean ocean age
dat<-dplyr::select(data,year,Salt.Water.Age,Count,length,stock)
meansaltage_YS_wide<-dat %>% group_by(stock,year) %>% summarize(meanage=mean(rep(Salt.Water.Age,Count))) %>% pivot_wider(names_from=stock,values_from=meanage) %>% select(year,all_of(rivers)) %>% data.frame
meansaltage_YS_wide<-meansaltage_YS_wide[order(meansaltage_YS_wide$year),]
##=================================================## mean freshwater age
dat<-dplyr::select(data,year,Fresh.Water.Age,Count,length,stock)
meanfreshage_YS_wide<-dat %>% group_by(stock,year) %>% summarize(meanage=mean(rep(Fresh.Water.Age,Count))) %>% pivot_wider(names_from=stock,values_from=meanage) %>% select(year,all_of(rivers)) %>% data.frame
meanfreshage_YS_wide<-meanfreshage_YS_wide[order(meanfreshage_YS_wide$year),]
##=================================================## mean total age
dat<-dplyr::select(data,year,Total.Age,Count,length,stock)
meanage_YS_wide<-dat %>% group_by(stock,year) %>% summarize(meanage=mean(rep(Total.Age,Count))) %>% pivot_wider(names_from=stock,values_from=meanage) %>% select(year,all_of(rivers)) %>% data.frame
meanage_YS_wide<-meanage_YS_wide[order(meanage_YS_wide$year),]

##=====================================================================##
##===============================================================## plots
##=====================================================================##
# pal<-colorRampPalette(brewer.pal(8,"Set1"));cols<-pal(nS) ## stock colors
# cols<-c("goldenrod1","darkblue","chocolate2","darkorchid3", "forestgreen","firebrick2","dodgerblue2","darkgray") # midnightblue/navyblue/darkblue

##---------------------## colors based on 'Sockeye2' photo by Jason Ching
cols<-c("#C37B7A","#855A27","#C7AC80","#8D9121","#629D9B","#178BC9", "#536373","#AE2633") 
if(dropAlagnak) cols<-c("#855A27","#C7AC80","#8D9121","#629D9B","#178BC9", "#536373","#AE2633") 

#cols<-rep("dimgray",7) 

##=====================================================================##
##====================## main figure with all trends by rivers as columns
##=====================================================================##
baywide<-TRUE ## plot bay-wide data and trends?
totalage<-FALSE ## add total age plot 
points<-FALSE ## circles/points for time series
lines<-TRUE ## lines for time series
alpha.ci<-0.05 ## 1-alpha is the confidence interval
gamma<-1.15 ## smoothing parameter in GAMs (default=1)
if(baywide) { pdf("Size-and-age-trends-by-river.pdf",width=8.5,height=4) } else { pdf("Size-and-age-trends-by-river.pdf",width=7.5,height=4) }
if(baywide) { layout(matrix(seq((nS+1)*4),ncol=(nS+1),nrow=4,byrow=T)) } else { layout(matrix(seq(nS*4),ncol=nS,nrow=4,byrow=T)) }
if(totalage) { pdf("Size-and-age-trends-by-river.pdf",width=8.5,height=5); layout(matrix(seq((nS+1)*5),ncol=(nS+1),nrow=5,byrow=T)) }
par(mar=c(0.3,0.3,0,0),oma=c(3.5,4,2,0.5),mgp=c(1.5,0.5,0),tcl=-0.3,lwd=0.5)
if(year.type=="brood"){ xlab<-"Brood year" } else { xlab<-"Run year" }
x<-years;nY<-length(years);xlim<-c(min(x),max(x))
lws.l<-0.5;cols.l<-cols ## raw data lines
lwd.g<-1.5;cols.gl<-alpha(cols,0.8);cols.gp<-alpha(cols,0.2);edge<-"white"
cols.bwl<-alpha(1,0.8);cols.bwp<-alpha(1,0.2)
cex.a<-0.8;cex.l<-0.7;cex.t<-0.75 ## axis; label; title
cex.p<-0.4
##===========================================================## mean size
ylim<-c(485,600)
##------------------------------------------------------------## bay-wide
if(baywide){
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
}
##--------------------------------------------------------------## rivers
for(s in 1:nS){
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
if(baywide){
y<-SaA_anomaly_Y[,2]
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
}
##--------------------------------------------------------------## rivers
for(s in 1:nS){
y<-SaA_anomaly_YS[,s+1]
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
if(baywide){
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
}
##--------------------------------------------------------------## rivers
for(s in 1:nS){
y<-meansaltage_YS_wide[,s+1]
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
if(baywide){
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
if(totalage==FALSE) axis(1,at=seq(1960,2020,10),label=T,cex.axis=cex.a,las=2,lwd=0.5)
if(totalage==FALSE) mtext("Brood year",side=1,line=2.5,cex=0.75)
box(lwd=1)
}
##--------------------------------------------------------------## rivers
for(s in 1:nS){
y<-meanfreshage_YS_wide[,s+1]
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
if(totalage==FALSE) axis(1,at=seq(1960,2020,10),label=T,cex.axis=cex.a,las=2,lwd=0.5)
if(totalage==FALSE) mtext("Brood year",side=1,line=2.5,cex=0.75)
box()
}
##=================================================## mean total age
if(totalage) {
ylim<-c(4.1,5.9)
##------------------------------------------------------------## bay-wide
if(baywide){
y<-meanage_Y[,2]
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
axis(2,at=seq(2,6,0.5),label=T,cex.axis=cex.a,las=2,lwd=0.5)
mtext("Mean total\nage (years)",side=2,line=2,cex=cex.l)
axis(1,at=seq(1960,2020,10),label=T,cex.axis=cex.a,las=2,lwd=0.5)
mtext("Brood year",side=1,line=2.5,cex=0.75)
box(lwd=1)
}
##--------------------------------------------------------------## rivers
for(s in 1:nS){
y<-meanage_YS_wide[,s+1]
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
} ## end if statement 
} ## end if statement total age
##-----------------------------------------------------------## save plot
dev.off()




##=====================================================================##
##===================## new main figure with all trends by rivers as rows
##=====================================================================##
baywide<-TRUE ## plot bay-wide data and trends?
# totalage<-FALSE ## add total age plot 
points<-FALSE ## circles/points for time series
lines<-TRUE ## lines for time series
alpha.ci<-0.05 ## 1-alpha is the confidence interval
gamma<-1.15 ## smoothing parameter in GAMs (default=1)
if(baywide) { pdf("Size-and-age-trends-by-river-rows.pdf",width=6,height=6) } else { pdf("Size-and-age-trends-by-river-rows.pdf",width=6,height=6) }
if(baywide) { layout(matrix(seq((nS+1)*4),nrow=(nS+1),ncol=4,byrow=F)) } else { layout(matrix(seq(nS*4),nrow=nS,ncol=4,byrow=F)) }
par(mar=c(0.5,3.5,0,0),oma=c(3.5,0.5,2,2),mgp=c(1,0.5,0),tcl=-0.3,lwd=0.5)
if(year.type=="brood"){ xlab<-"Brood year" } else { xlab<-"Run year" }
x<-years;nY<-length(years);xlim<-c(min(x),max(x))
lws.l<-0.5;cols.l<-cols ## raw data lines
lwd.g<-1.5;cols.gl<-alpha(cols,0.8);cols.gp<-alpha(cols,0.2);edge<-"white"
cols.bwl<-alpha(1,0.8);cols.bwp<-alpha(1,0.2)
cex.a<-0.8;cex.l<-0.75;cex.t<-0.75 ## axis; label; title
cex.p<-0.4
##===========================================================## mean size
ylim<-c(485,600)
##------------------------------------------------------------## bay-wide
if(baywide){
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
mtext("Body size ",side=3,line=0.5,cex=cex.t,font=1,col=1)
box()
}
##--------------------------------------------------------------## rivers
for(s in 1:nS){
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
axis(2,at=seq(500,600,20),label=T,cex.axis=cex.a,las=2,lwd=0.5)
if(s==4) mtext("Mean size (mm)",side=2,line=1.8,cex=cex.l)
if(s==nS) axis(1,at=seq(1960,2020,10),label=T,cex.axis=cex.a,las=2,lwd=0.5)
if(s==nS) mtext("Brood year",side=1,line=2.5,cex=0.75)
box()
}
##=========================================## average size-at-age anomaly
ylim<-c(-26,30)
##------------------------------------------------------------## bay-wide
if(baywide){
y<-SaA_anomaly_Y[,2]
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
mtext("Size-at-age anomaly ",side=3,line=0.5,cex=cex.t,font=1,col=1)
box()
}
##--------------------------------------------------------------## rivers
for(s in 1:nS){
y<-SaA_anomaly_YS[,s+1]
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
axis(2,at=seq(-20,30,10),label=T,cex.axis=cex.a,las=2,lwd=0.5)
if(s==4) mtext("Mean size-at-age anomaly (mm)",side=2,line=1.8,cex=cex.l)
if(s==nS) axis(1,at=seq(1960,2020,10),label=T,cex.axis=cex.a,las=2,lwd=0.5)
if(s==nS) mtext("Brood year",side=1,line=2.5,cex=0.75)
box()
}
##======================================================## mean ocean age
ylim<-c(1.9,3.1)
##------------------------------------------------------------## bay-wide
if(baywide){
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
mtext("Ocean age ",side=3,line=0.5,cex=cex.t,font=1,col=1)
box()
}
##--------------------------------------------------------------## rivers
for(s in 1:nS){
y<-meansaltage_YS_wide[,s+1]
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
axis(2,at=seq(1,4,0.2),label=T,cex.axis=cex.a,las=2,lwd=0.5)
if(s==4) mtext("Mean ocean age (years)",side=2,line=1.8,cex=cex.l)
if(s==nS) axis(1,at=seq(1960,2020,10),label=T,cex.axis=cex.a,las=2,lwd=0.5)
if(s==nS) mtext("Brood year",side=1,line=2.5,cex=0.75)
box()
}
##=================================================## mean freshwater age
ylim<-c(0.4,2.1)
##------------------------------------------------------------## bay-wide
if(baywide){
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
mtext("Freshwater age ",side=3,line=0.5,cex=cex.t,font=1,col=1)
mtext("bay-wide",side=4,line=0.5,cex=cex.t,font=2,col=1)
box()
}
##--------------------------------------------------------------## rivers
for(s in 1:nS){
y<-meanfreshage_YS_wide[,s+1]
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
axis(2,at=seq(0,3,0.5),label=T,cex.axis=cex.a,las=2,lwd=0.5)
if(s==4) mtext("Mean freshwater age (years)",side=2,line=1.8,cex=cex.l)
if(s==nS) axis(1,at=seq(1960,2020,10),label=T,cex.axis=cex.a,las=2,lwd=0.5)
if(s==nS) mtext("Brood year",side=1,line=2.5,cex=0.75)
mtext(paste0(rivers[s]," "),side=4,line=0.5,cex=cex.t,font=2,col=cols[s])
box()
}
##-----------------------------------------------------------## save plot
dev.off()





##=====================================================================##
##===========================================## calculate age proportions
##=====================================================================##
plot_ages<-c("1.2","2.2","1.3","2.3")
agedata<-data[data$age %in% plot_ages,] ## common ages
##-----------------------------------------------------------## bay-wide
age_count_YA_wide<-agedata %>% group_by(year,age) %>% summarize(count=length(rep(age,Count))) %>% pivot_wider(names_from=age,values_from=count) %>% data.frame
age_count_YA_wide[is.na(age_count_YA_wide)]<-0
colnames(age_count_YA_wide)<-gsub("X","",colnames(age_count_YA_wide))
age_count_YA_wide<-as.matrix(age_count_YA_wide)
age_prop_YA_wide<-prop.table(age_count_YA_wide[,-1],margin=1)
age_prop_YA_wide<-age_prop_YA_wide[,order(match(as.numeric(colnames(age_prop_YA_wide)),plot_ages))] ## order columns by age group
age_prop_YA_BB<-age_prop_YA_wide
##-----------------------------------------------------------## each river
age_prop_YA_stocks<-list()
for(s in 1:nS){
stock<-rivers[s]
agedata_river<-agedata[agedata$stock==stock,]
age_count_YA_wide<-agedata_river %>% group_by(year,age) %>% summarize(count=length(rep(age,Count))) %>% pivot_wider(names_from=age,values_from=count) %>% data.frame
age_count_YA_wide[is.na(age_count_YA_wide)]<-0
colnames(age_count_YA_wide)<-gsub("X","",colnames(age_count_YA_wide))
age_count_YA_wide<-as.matrix(age_count_YA_wide)
age_prop_YA_wide<-prop.table(age_count_YA_wide[,-1],margin=1)
age_prop_YA_wide<-age_prop_YA_wide[,order(match(as.numeric(colnames(age_prop_YA_wide)),plot_ages))] 
age_prop_YA_stocks[[s]]<-age_prop_YA_wide
}

##=====================================================================##
##========================## new version with age proportions as polygons
##=====================================================================##
baywide<-T ## plot bay-wide data and trends?
alpha.ci<-0.05 ## 1-alpha is the confidence interval
gamma<-1.15 ## smoothing parameter in GAMs (default=1)
if(baywide) { pdf("Size-and-age-trends-by-river-v2.pdf",width=8.5,height=3) } else { pdf("Size-and-age-trends-by-river-v2.pdf",width=7.5,height=3) }
if(baywide) { layout(matrix(seq((nS+1)*3),ncol=(nS+1),nrow=3,byrow=T)) } else { layout(matrix(seq(nS*3),ncol=nS,nrow=3,byrow=T)) }
par(mar=c(0.3,0.3,0,0),oma=c(3.5,4,2,0.5),mgp=c(1.5,0.5,0),tcl=-0.3,lwd=0.5)
if(year.type=="brood"){ xlab<-"Brood year" } else { xlab<-"Run year" }
x<-years;nY<-length(years);xlim<-c(min(x),max(x))
lws.l<-0.5;cols.l<-cols ## raw data lines
lwd.g<-1.5;cols.gl<-alpha(cols,0.8);cols.gp<-alpha(cols,0.2);edge<-"white"
cols.bwl<-alpha(1,0.8);cols.bwp<-alpha(1,0.2)
cex.a<-0.8;cex.l<-0.7;cex.t<-0.75 ## axis; label; title
##===========================================================## mean size
ylim<-c(485,600)
##------------------------------------------------------------## bay-wide
if(baywide){
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
  lines(x,y,lty=1,lwd=lws.l,col=1)
  axis(2,at=seq(500,600,20),label=T,cex.axis=cex.a,las=2,lwd=0.5)
  mtext("Mean size\n(mm)",side=2,line=2,cex=cex.l)
  mtext("bay-wide ",side=3,line=0.5,cex=cex.t,font=2,col=1)
  box()
}
##--------------------------------------------------------------## rivers
for(s in 1:nS){
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
  lines(x,y,lty=1,lwd=lws.l,col=cols.l[s])
  mtext(paste0(rivers[s]," "),side=3,line=0.5,cex=cex.t,font=2,col=cols[s])
  box()
}
##=========================================## average size-at-age anomaly
ylim<-c(-26,30)
##------------------------------------------------------------## bay-wide
if(baywide){
  y<-SaA_anomaly_Y[,2]
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
  lines(x,y,lty=1,lwd=lws.l,col=1)
  axis(2,at=seq(-20,30,10),label=T,cex.axis=cex.a,las=2,lwd=0.5)
  mtext("Mean size-at-age\nanomaly (mm)",side=2,line=2,cex=cex.l)
  box()
}
##--------------------------------------------------------------## rivers
for(s in 1:nS){
  y<-SaA_anomaly_YS[,s+1]
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
  lines(x,y,lty=1,lwd=lws.l,col=cols.l[s])
  box()
}
##======================================================## age proportions
agecols<-alpha(c("chocolate2","darkgoldenrod1","skyblue","dodgerblue3"),.9)
##------------------------------------------------------------## bay-wide
if(baywide){
  plot.prop<-round(age_prop_YA_BB,5) 
  plot(NA,NA,xlim=c(min(years),max(years)),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n")
  poly.years<-as.numeric(unique(age_count_YA_wide[,1]))
  n.yrs<-length(poly.years)
  poly.x<-c(poly.years,rev(poly.years))
  for(a in 1:length(plot_ages)){ ## loop through age groups
    if(a==1) { prop_added<-plot.prop[,a];poly.y<-c(prop_added,rep(0,n.yrs)) }
    if(a!=1) { prop_prev<-prop_added;prop_added<-prop_prev+plot.prop[,a];poly.y<-c(prop_added,rev(prop_prev)) }
    polygon(poly.x,poly.y,lwd=0.5,col=agecols[a],border=agecols[a])
  } ## end age loop
  axis(2,at=seq(0,1,0.2),label=T,cex.axis=cex.a,las=2,lwd=0.5)
  mtext("Age-class\nproportions",side=2,line=2,cex=cex.l)
  axis(1,at=seq(1960,2020,10),label=T,cex.axis=cex.a,las=2,lwd=0.5)
  mtext("Brood year",side=1,line=2.5,cex=0.75)
  legend("bottomleft",rev(plot_ages),pch=15,cex=0.8,xpd=NA,horiz=F,bty="n",col=rev(agecols),inset=c(-0.6,-0.7))
  #text(1962,0.04,"1.2",cex=0.5,font=2)
  #text(1962,0.4,"2.2",cex=0.5,font=2)
  #text(1962,0.8,"1.3",cex=0.5,font=2)
  #text(1962,0.96,"2.3",cex=0.5,font=2)
}
##--------------------------------------------------------------## rivers
for(s in 1:nS){
  stock<-rivers[s]
  plot.prop<-round(age_prop_YA_stocks[[s]],5) 
  plot(NA,NA,xlim=c(min(years),max(years)),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n")
  poly.years<-as.numeric(unique(age_count_YA_wide[,1]))
  n.yrs<-length(poly.years)
  poly.x<-c(poly.years,rev(poly.years))
  for(a in 1:length(plot_ages)){ ## loop through age groups
    if(a==1) { prop_added<-plot.prop[,a];poly.y<-c(prop_added,rep(0,n.yrs)) }
    if(a!=1) { prop_prev<-prop_added;prop_added<-prop_prev+plot.prop[,a];poly.y<-c(prop_added,rev(prop_prev)) }
    polygon(poly.x,poly.y,lwd=0.5,col=agecols[a],border=agecols[a])
    } ## end age loop
  axis(1,at=seq(1960,2020,10),label=T,cex.axis=cex.a,las=2,lwd=0.5)
  mtext("Brood year",side=1,line=2.5,cex=0.75)
} ## end loop over stocks/rivers
##-----------------------------------------------------------## save plot
dev.off()

##=====================================================================##
##========================================## only age proportions figures
##=====================================================================##

##============================================## bay-wide age-proportions
pdf("Age-class-proportions.pdf",width=5,height=4)
par(mar=c(3.5,3.5,2,1),mgp=c(2,0.5,0),tcl=-0.3,yaxs="i",xaxs="i")
plot(NA,NA,xlim=c(min(years),max(years)),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n")
plot.prop<-round(age_prop_YA_BB,5) 
poly.years<-as.numeric(unique(age_count_YA_wide[,1]))
n.yrs<-length(poly.years)
poly.x<-c(poly.years,rev(poly.years))
for(a in 1:length(plot_ages)){ ## loop through age groups
  if(a==1) { prop_added<-plot.prop[,a];poly.y<-c(prop_added,rep(0,n.yrs)) }
  if(a!=1) { prop_prev<-prop_added;prop_added<-prop_prev+plot.prop[,a];poly.y<-c(prop_added,rev(prop_prev)) }
  polygon(poly.x,poly.y,lwd=0.5,col=agecols[a],border=agecols[a])
  if(a==3) lines(poly.years,prop_prev,lwd=0.5,col="white")
} ## end age loop
axis(2,at=seq(0,1,0.2),label=T,cex.axis=1,lwd=0.5)
mtext("Proportion",side=2,line=2,cex=1.2)
axis(1,at=seq(1960,2020,10),label=T,cex.axis=1,lwd=0.5)
mtext("Brood year",side=1,line=2,cex=1.2)
legend("top",plot_ages,pch=15,cex=1,xpd=T,horiz=T,bty="n",col=agecols, inset=c(0,-0.14))
dev.off()

##=========================## bay-wide and stock-sepcific age-proportions
pdf("Age-class-proportions-all.pdf",width=8,height=6)
layout(matrix(c(1,1,1,2,1,1,1,3,1,1,1,4,5,6,7,8),ncol=4,nrow=4,byrow=T))
par(mar=c(2,2,0,0),oma=c(2,2,1,1),mgp=c(2,0.5,0),tcl=-0.3,yaxs="i",xaxs="i")
##-----------------------------------------------------------## bay-wide
plot(NA,NA,xlim=c(min(years),max(years)),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n")
poly.years<-as.numeric(unique(age_count_YA_wide[,1]))
plot.prop<-round(age_prop_YA_BB,5) 
n.yrs<-length(poly.years)
poly.x<-c(poly.years,rev(poly.years))
for(a in 1:length(plot_ages)){ ## loop through age groups
  if(a==1) { prop_added<-plot.prop[,a];poly.y<-c(prop_added,rep(0,n.yrs)) }
  if(a!=1) { prop_prev<-prop_added;prop_added<-prop_prev+plot.prop[,a];poly.y<-c(prop_added,rev(prop_prev)) }
  polygon(poly.x,poly.y,lwd=0.5,col=agecols[a],border=agecols[a])
  if(a==3) lines(poly.years,prop_prev,lwd=0.5,col="white")
} ## end age loop
axis(2,at=seq(0,1,0.2),label=T,cex.axis=1,lwd=0.5)
#mtext("Proportion",side=2,line=2,cex=1.2,outer=T)
axis(1,at=seq(1960,2020,10),label=T,cex.axis=1,lwd=0.5)
#mtext("Brood year",side=1,line=2,cex=1.2)
legend("top",plot_ages,pch=15,cex=1,xpd=T,horiz=T,bty="n",col=agecols, inset=c(0,-0.14))
mtext("bay-wide ",side=3,line=-1.2,cex=0.8,font=2,adj=1,col="white")
text(1961,.04,"1.2",cex=1.1,font=2,col="white")
#text(1961,0.04,"1.2",font=2)
text(1961,.4,"2.2",font=2,cex=1.1,col="white")
#text(1961,0.4,"2.2",font=2)
text(1961,.75,"1.3",font=2,cex=1.1,col="white")
#text(1961,0.75,"1.3",font=2)
text(1961,.96,"2.3",font=2,cex=1.1,col="white")
#text(1961,0.96,"2.3",font=2)
##-----------------------------------------------------------## each river
for(s in 1:nS){
  stock<-rivers[s]
  plot.prop<-round(age_prop_YA_stocks[[s]],5) 
  plot(NA,NA,xlim=c(min(years),max(years)),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n")
  poly.years<-as.numeric(unique(age_count_YA_wide[,1]))
  n.yrs<-length(poly.years)
  poly.x<-c(poly.years,rev(poly.years))
  for(a in 1:length(plot_ages)){ ## loop through age groups
    if(a==1) { prop_added<-plot.prop[,a];poly.y<-c(prop_added,rep(0,n.yrs)) }
    if(a!=1) { prop_prev<-prop_added;prop_added<-prop_prev+plot.prop[,a];poly.y<-c(prop_added,rev(prop_prev)) }
    polygon(poly.x,poly.y,lwd=0.5,col=agecols[a],border=agecols[a])
    if(a==3) lines(poly.years,prop_prev,lwd=0.5,col="white")
  } ## end age loop
  axis(1,at=seq(1960,2020,10),label=T,cex.axis=0.8,lwd=0.5)
  if(s==4) axis(2,at=seq(0,1,0.2),label=T,cex.axis=0.8,lwd=0.5)
  #mtext("Brood year",side=1,line=2.5,cex=0.75)
  mtext(paste0(stock," "),side=3,line=-1,cex=0.6,font=2,adj=1,col="white")
} ## end loop over stocks/rivers
##-----------------------------------------------------------## save plot
mtext("Proportion",side=2,line=2,cex=1.2,outer=T)
mtext("Brood year",side=1,line=0.5,cex=1.2,outer=T)
dev.off()

##=====================================================================##
##=====================================================================##
##=====================================================================##