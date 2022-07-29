##=====================================================================##
##                                                                     ##
## Changes in mean size, age composition and size-at-age of BB sockeye ##
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
nS<-length(rivers)
##===========================================================## directory
subDir<-paste(year.type,"year",catch.type,"data")
dir.create(file.path(paste0(homeDir,"/observed/",subDir)),showWarnings=F)
setwd(file.path(paste0(homeDir,"/observed/",subDir)))
##==================================================## age-length dataset
data<-read.csv("data_used.csv")[,-1]
# stocks<-unique(data$stock);nS<-length(stocks)
ages<-unique(data$age);nA<-length(ages)
years<-unique(data$year);nY<-length(years)
##==================================================## mean size each year
meansize_Y<-read.csv("Mean_size_BB_wide.csv")[,-1]
meansize_YS<-read.csv("Mean_size_by_system.csv")[,-1] %>% select(year,all_of(rivers)) %>% data.frame
##===================================## mean size-at-age anomaly each year
SaA_anomaly_Y<-read.csv("SaA_anomaly_BB_wide.csv")[,-1]
SaA_anomaly_YS<-read.csv("SaA_anomaly_by_system.csv")[,-1] %>% select(year,all_of(rivers)) %>% data.frame

##=====================================================================##
##===================================================## summarize BB-wide
##=====================================================================##
## aggregate individual observations for all of BB
##===============================================## overall size variance
sdsize_Y<-data %>% group_by(year) %>% summarize(sd_size=sd(rep(length,Count))) %>% data.frame
write.csv(sdsize_Y,file="sd_size_by_year_BB_wide.csv")
##====================================================## mean size-at-age
meanSaA_YA<-data %>% group_by(age,year) %>% summarize(meansize=mean(rep(length,Count),na.rm=T)) %>% data.frame
write.csv(meanSaA_YA,file="mean_SaA_by_year_BB_wide.csv")
##======================================================## mean total age
meanage_Y<-data %>% group_by(year) %>% summarize(meanage=mean(rep(as.numeric(Total.Age),Count),na.rm=T)) %>% data.frame
write.csv(meanage_Y,file="Mean_total_age_BB_wide.csv")
##==================================================## mean saltwater age
meanSWage_Y<-data %>% group_by(year) %>% summarize(meanage=mean(rep(as.numeric(Salt.Water.Age),Count),na.rm=T)) %>% data.frame
write.csv(meanSWage_Y,file="Mean_ocean_age_BB_wide.csv")
##=================================================## mean freshwater age
meanFWage_Y<-data %>% group_by(year) %>% summarize(meanage=mean(rep(as.numeric(Fresh.Water.Age),Count),na.rm=T)) %>% data.frame
write.csv(meanFWage_Y,file="Mean_freshwater_age_BB_wide.csv")
##==============================================## number of abservations
##------------------------------------------------## observations by year
nobs_Y<-data %>% group_by(year) %>% summarize(nobs=length(rep(length,Count))) %>% data.frame
write.csv(nobs_Y,"nobs_by_year.csv")
##----------------------------------------## observations by year and age
nobs_AY<-data %>% group_by(age,year) %>% summarize(nobs=length(rep(length,Count))) %>% data.frame
nobs_AY_wide<-nobs_AY %>% pivot_wider(names_from=age,values_from=nobs) %>% data.frame
names(nobs_AY_wide)<-gsub("X","",names(nobs_AY_wide))
write.csv(nobs_AY,"nobs_by_year_and_age.csv")
##-----------------------------------------------------## age proportions
nobs_A<-data %>% group_by(age) %>% summarize(nobs=length(rep(length,Count))) %>% data.frame
nobs_A$percent<-round(nobs_A$nobs/sum(nobs_A$nobs)*100,2)
##---------------------------------------------## age proportions by year
nobs_AY_wide<-as.matrix(nobs_AY_wide)
props_AY<-round(prop.table(nobs_AY_wide[,-1],margin=1),4)
age_props_YA_wide<-prop.table(nobs_AY_wide[,-1],margin=1)
age_props_YA_wide<-age_props_YA_wide[,order(as.numeric(colnames(age_props_YA_wide)))] ## order columns by age group
age_props_YA<-round(age_props_YA_wide,3)

##=====================================================================##
##=================================================## summarize by system
##=====================================================================##
## aggregate by year and stock/system
##======================================## observations by year and stock
nobs_YS<-data %>% group_by(year,stock) %>% summarize(nobs=length(rep(length,Count))) %>% data.frame
nobs_YS_wide<-nobs_YS %>% pivot_wider(names_from=stock,values_from=nobs) %>% select(year,all_of(rivers))  %>% data.frame
write.csv(nobs_YS,"nobs_by_year_system.csv")
##===============================================## overall size variance 
sdsize_YS_wide<-data %>% group_by(stock,year) %>% summarize(meansize=sd(rep(length,Count))) %>% pivot_wider(names_from=stock,values_from=meansize) %>% select(year,all_of(rivers)) %>% data.frame
sdsize_YS_wide<-sdsize_YS_wide[order(sdsize_YS_wide$year),]
sdsize_YS_wide[sdsize_YS_wide==0]<-NA
##====================================================## mean size-at-age
meanSaA_SYA<-data %>% group_by(stock,year,age) %>% summarize(meansize=mean(rep(length,Count),na.rm=T)) %>% data.frame
write.csv(meanSaA_SYA,file="mean_SaA_by_stock_and_year.csv")
meanSaA_SA<-data %>% group_by(stock,age) %>% summarize(meansize=mean(rep(length,Count),na.rm=T)) %>% data.frame
##=====================================================## mean ocean age
dat<-dplyr::select(data,year,Salt.Water.Age,Count,length,stock)
meansaltage_YS_wide<-dat %>% group_by(stock,year) %>% summarize(meanage=mean(rep(Salt.Water.Age,Count))) %>% pivot_wider(names_from=stock,values_from=meanage) %>% select(year,all_of(rivers)) %>% data.frame
meansaltage_YS_wide<-meansaltage_YS_wide[order(meansaltage_YS_wide$year),]
write.csv(meansaltage_YS_wide,file="Mean_ocean_age_by_system.csv")
##=====================================================## mean total age
dat<-dplyr::select(data,year,Total.Age,Count,length,stock)
meanage_YS_wide<-dat %>% group_by(stock,year) %>% summarize(meanage=mean(rep(Total.Age,Count))) %>% pivot_wider(names_from=stock,values_from=meanage) %>% select(year,all_of(rivers)) %>% data.frame
meanage_YS_wide<-meanage_YS_wide[order(meanage_YS_wide$year),]
write.csv(meanage_YS_wide,file="Mean_total_age_by_system.csv")
##=================================================## mean freshwater age
dat<-dplyr::select(data,year,Fresh.Water.Age,Count,length,stock)
meanfreshage_YS_wide<-dat %>% group_by(stock,year) %>% summarize(meanage=mean(rep(Fresh.Water.Age,Count))) %>% pivot_wider(names_from=stock,values_from=meanage) %>% select(year,all_of(rivers)) %>% data.frame
meanfreshage_YS_wide<-meanfreshage_YS_wide[order(meanfreshage_YS_wide$year),]
write.csv(meanfreshage_YS_wide,file="Mean_freshwater_age_by_system.csv")

##=====================================================================##
##===============================================================## plots
##=====================================================================##
# pal<-colorRampPalette(brewer.pal(8,"Set1"));cols<-pal(nS) ## stock colors
# cols<-c("goldenrod1","darkblue","chocolate2","darkorchid3", "forestgreen","firebrick2","dodgerblue2","darkgray") # midnightblue/navyblue/darkblue

##---------------------## colors based on 'Sockeye2' photo by Jason Ching
cols<-c("#C37B7A","#855A27","#C7AC80","#8D9121","#629D9B","#178BC9", "#536373","#AE2633") 
if(dropAlagnak) cols<-c("#855A27","#C7AC80","#8D9121","#629D9B","#178BC9", "#536373","#AE2633") 
rivercols<-cols
##-------------------------------## function to match rivers to districts
if(dropAlagnak) { districts<-c(325,325,325,324,324,322,321) } else { c(325,325,325,324,324,324,322,321) } 
rtd<-data.frame(stock=rivers,district=districts) 
ind<-which(names(data)=="stock")
data$district<-apply(data,1,function(x) rtd$district[rtd$stock==x[ind]])

##====================================================## mean size-at-age
plot_ages<-c("1.2","1.3","2.2","2.3")
meanSaA_SYA$district<-apply(meanSaA_SYA,1,function(x) rtd$district[rtd$stock==x[1]])
meanSaA_SYA_plot<-dplyr::filter(meanSaA_SYA,age %in% plot_ages)
##----------------------------------------------------## by age and river
meanSaA_SYA_plot %>% 
  ggplot(aes(x=year,y=meansize,color=stock)) +
  geom_point(position=position_dodge(width=1)) +
  geom_line(lwd=0.5) +
  facet_wrap(vars(age),nrow=1,ncol=4,scales='free_y') +
  scale_color_manual(values=cols) +
  theme_classic() +
  theme(strip.background=element_blank())
ggsave("Mean-size-at-age-by-river.pdf",width=30,height=10,units="cm")

##===================================## mean size-at-age anomaly bay-wide 
plot_ages<-c("1.2","2.2","1.3","2.3")
meanSaA_YA_select<-dplyr::filter(meanSaA_YA,age %in% plot_ages)
meanSaA_YA_wide<- meanSaA_YA_select %>% pivot_wider(names_from=age, values_from=meansize) %>% data.frame
yrs<-meanSaA_YA_wide[,1]
meanSaA_YA_std<-meanSaA_YA_wide[,-1] %>% scale(center=T,scale=F) %>% data.frame
names(meanSaA_YA_std)<-plot_ages

##=================================## size-at-age anomaly trend by system
pdf("Size-at-age-trends-compared-bay-wide.pdf",width=5,height=4)
par(mar=c(3.5,3.5,1,1),mgp=c(1.75,0.5,0),tcl=-0.3,cex.lab=1,cex.axis=0.8)
xlim<-c(min(yrs),max(yrs))
if(year.type=="brood"){ xlab<-"Brood year" } else { xlab<-"Run year" }
yqs<-quantile(meanSaA_YA_std,probs=c(0,1),na.rm=T)
ylim<-c(yqs[1],yqs[2])
agecols<-c("goldenrod1","chocolate2","forestgreen","darkorchid3")
agecols<-c("chocolate2","goldenrod1","dodgerblue2","midnightblue")
plot(NA,NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="Size-at-age anomaly (mm)")
matlines(yrs,meanSaA_YA_std,lty=1,lwd=1.5,col=agecols)
# matpoints(yrs,meanSaA_YA_std,pch=16,cex=0.8,col=agecols) 
legend("topright",plot_ages,pch=16,lwd=1,cex=0.6,bty="n",col=agecols)
dev.off()

test<-apply(meanSaA_YA_std,2,function(x) as.numeric(as.character(x)))
res<-Hmisc::rcorr(test)
out<-res$r ## all pairwise correlations
diag(out)<-NA
mean(out,na.rm=T)

##=====================================================## age proportions
# plot_ages<-c("1.2","2.2","1.3","2.3")
agedata<-data[data$age %in% plot_ages,] ## common ages
age_count_YA_wide<-agedata %>% group_by(year,age) %>% summarize(count=length(rep(age,Count))) %>% pivot_wider(names_from=age,values_from=count) %>% data.frame
age_count_YA_wide[is.na(age_count_YA_wide)]<-0
colnames(age_count_YA_wide)<-gsub("X","",colnames(age_count_YA_wide))
age_count_YA_wide<-as.matrix(age_count_YA_wide)
age_prop_YA_wide<-prop.table(age_count_YA_wide[,-1],margin=1)
age_prop_YA_wide<-age_prop_YA_wide[,order(match(as.numeric(colnames(age_prop_YA_wide)),plot_ages))] ## order columns by age group
age_prop_YA_plot<-round(age_prop_YA_wide,5) 

##-------------------------------------------------## plot age proportions
pdf("Age-proportions.pdf",width=5,height=4)
par(mar=c(3,3,2,1),mgp=c(1.8,.5,0),tcl=-0.3,cex.lab=1.2,yaxs="i",xaxs="i")
plot.prop<-age_prop_YA_plot
plot_ages<-colnames(age_prop_YA_plot)
agecols<-alpha(c("chocolate2","goldenrod1","dodgerblue2","midnightblue"),0.9) ##firebrick2
if(year.type=="brood") { xlab<-"Brood year" } else { xlab<-"Year" }
plot(NA,NA,xlim=c(min(years),max(years)),ylim=c(0,1),xlab=xlab,ylab="Proportion")
poly.years<-as.numeric(unique(age_count_YA_wide[,1]))
n.yrs<-length(poly.years)
poly.x<-c(poly.years,rev(poly.years))
for(a in 1:length(plot_ages)){ ## loop through age groups
if(a==1) { prop_added<-plot.prop[,a];poly.y<-c(prop_added,rep(0,n.yrs)) }
if(a!=1) { prop_prev<-prop_added;prop_added<-prop_prev+plot.prop[,a];poly.y<-c(prop_added,rev(prop_prev)) }
polygon(poly.x,poly.y,lwd=0.5,col=agecols[a],border=agecols[a])
} ## end age loop
legend("top",plot_ages,pch=15,cex=1,xpd=T,horiz=T,bty="n",col=agecols, inset=c(0,-0.14))
##------------------------------## add mean age trend and axis with label
# par(new=T)
# plot(NA,NA,xlim=c(min(years),max(years)),ylim=c(3.5,5.5),axes=F,yaxt="n",xaxt="n",xlab="",ylab="")
# axis(side=4,at=seq(1,6,0.5),labels=T)
# mtext("Mean age",side=4,line=1.8,cex=1.2)
# lines(meanage_Y[,1],meanage_Y[,2],lwd=3)
box()
dev.off()

##-------------------------------------------------## plot age proportions
pdf("Age-proportions-scatter.pdf",width=5,height=4)
par(mar=c(3,3,2,1),mgp=c(1.8,.5,0),tcl=-0.3,cex.lab=1.2)
plot.prop<-age_prop_YA_plot[,-1]
plot_ages<-colnames(age_prop_YA_plot[,-1])
agecols<-alpha(c("chocolate2","goldenrod1","dodgerblue2","midnightblue"),0.9) ##firebrick2
if(year.type=="brood") { xlab<-"Brood year" } else { xlab<-"Year" }
plot(NA,NA,xlim=c(min(years),max(years)),ylim=c(0,0.9),xlab=xlab,ylab="Proportion")
# matlines(years,plot.prop,lwd=1,lty=1,col=agecols)
matpoints(years,plot.prop,pch=21,lwd=0.2,cex=0.8,col="black",bg=agecols)
for(a in 1:4) {
lines(loess.smooth(years,plot.prop[,a],span=span),lwd=3,col=agecols[a])
}
legend("topright",plot_ages,lwd=2,col=agecols,cex=0.8,seg.len=1.5,bty="n")
dev.off()


##================================## mean size-at-age dominant age groups
meanSaA_YA_wide<-meanSaA_YA %>% pivot_wider(names_from=age,values_from=meansize) %>% data.frame
meanSaA_YA_plot<-meanSaA_YA_wide %>% select(year,X1.2,X1.3,X2.2,X2.3) 
names(meanSaA_YA_plot)<-gsub("X","",names(meanSaA_YA_plot))
meanSaA_YA_plot<-meanSaA_YA_plot[order(meanSaA_YA_plot$year),]
##--------------------------------## same data frame with age proportions
age_prop_YA_plot<-data.frame(cbind(year=age_count_YA_wide[,1],age_prop_YA_plot)) 
names(age_prop_YA_plot)<-gsub("X","",names(age_prop_YA_plot))

##---------------------------------------------------------## plot bay-wide
pdf("Mean-size-at-age-bubble-plot.pdf",width=4,height=6.5)
cols<-c("skyblue","goldenrod1","dodgerblue3","chocolate2")
lnms<-names(meanSaA_YA_plot)[-1]
par(mar=c(3,3,1,1),mgp=c(2,0.5,0),tcl=-0.3,cex.lab=1.2)
if(year.type=="brood"){ xlab<-"Brood year" } else { xlab<-"Run year" }
yrs<-meanSaA_YA_plot[,1];xlim<-c(min(yrs),max(yrs))
ylim<-c(min(meanSaA_YA_plot[,-1],na.rm=T),max(meanSaA_YA_plot[,-1],na.rm=T))
cex_fac<-5
plot(NA,NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="Mean size (mm)")
for(a in 1:4) {
lines(yrs,meanSaA_YA_plot[,a+1],lwd=0.5,col=cols[a]) 
points(yrs,meanSaA_YA_plot[,a+1],cex=age_prop_YA_plot[,a+1]*cex_fac,lwd=0.1, pch=16,col=cols[a]) 
}
dev.off()

##---------------------------------------------------------## plot bay-wide
pdf("Mean-size-at-age-plot.pdf",width=4,height=5.5)
agecols<-c("chocolate2","skyblue","goldenrod1","dodgerblue3")
lnms<-names(meanSaA_YA_plot)[-1]
par(mar=c(3.5,3.5,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.3,cex.lab=1.1)
if(year.type=="brood"){ xlab<-"Brood year" } else { xlab<-"Run year" }
yrs<-meanSaA_YA_plot[,1];xlim<-c(min(yrs),max(yrs))
ylim<-c(min(meanSaA_YA_plot[,-1],na.rm=T),max(meanSaA_YA_plot[,-1],na.rm=T))
plot(NA,NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="Mean size (mm)")
for(a in 1:4) {
  lines(yrs,meanSaA_YA_plot[,a+1],lwd=0.5,col=agecols[a]) 
  points(yrs,meanSaA_YA_plot[,a+1],cex=0.8,pch=16,col=agecols[a]) 
}
legend("topright",lnms[c(4,2,3,1)],pch=16,lwd=1,col=agecols[c(4,2,3,1)],cex=0.8,seg.len=1.5,bty="n")
dev.off()


##=================================## size-at-age anomaly trend by system
pdf("Size-at-age-and-anomaly-compared-bay-wide.pdf",width=4,height=6.5)
par(mar=c(2,3,0,0),oma=c(0.5,0.5,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.3,cex.axis=0.8)
layout(matrix(c(1:2),ncol=1,byrow=T),heights=c(0.58,0.42))
xlim<-c(min(yrs),max(yrs))
if(year.type=="brood"){ xlab<-"Brood year" } else { xlab<-"Run year" }
agecols<-c("skyblue","goldenrod1","dodgerblue3","chocolate2")
##-----------------------------------------------------## mean size-at-age
lnms<-names(meanSaA_YA_plot)[-1]
yrs<-meanSaA_YA_plot[,1];xlim<-c(min(yrs),max(yrs))
ylim<-c(min(meanSaA_YA_plot[,-1],na.rm=T),max(meanSaA_YA_plot[,-1],na.rm=T))
plot(NA,NA,xlim=xlim,ylim=ylim,xlab="",ylab="Mean size-at-age (mm)")
for(a in 1:4) {
  lines(yrs,meanSaA_YA_plot[,a+1],lwd=0.5,col=agecols[a]) 
  points(yrs,meanSaA_YA_plot[,a+1],cex=0.8,lwd=0.1,pch=16,col=agecols[a]) 
}
legend("topright",lnms[c(4,2,3,1)],lwd=1,pch=16,col=agecols[c(4,2,3,1)],cex=0.6,seg.len=1.5,bty="n")
##--------------------------------------------------## size-at-age anomaly
par(mar=c(3,3,0,0))
yqs<-quantile(meanSaA_YA_std,probs=c(0,1),na.rm=T)
ylim<-c(yqs[1],yqs[2])
plot(NA,NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="Size-at-age anomaly (mm)")
matlines(yrs,meanSaA_YA_std,lty=1,lwd=1,col=agecols)
# matpoints(yrs,meanSaA_YA_std,pch=16,cex=0.8,col=agecols) 
legend("topright",lnms[c(4,2,3,1)],lwd=1,cex=0.6,bty="n",col=agecols[c(4,2,3,1)])
dev.off()


##=============================## mean freshwater and ocean age by system
pdf("Mean-age-trends-by-system.pdf",width=8,height=4)
layout(matrix(c(1:8),ncol=4,byrow=T))
par(mar=c(3,3,0.5,0.5),oma=c(0.5,0.5,0.5,0.5),mgp=c(1.75,0.5,0),tcl=-0.3, cex.lab=1.2,cex.axis=0.8)
if(year.type=="brood") { xlab="Brood year" } else {  xlab="Run year" }
span<-0.5
cols<-rivercols
yrs<-meansaltage_YS_wide$year
xlim<-c(min(yrs),max(yrs))
for(s in 1:nS){
stock<-rivers[s]
plot(NA,NA,xlim=xlim,ylim=c(0.8,3.2),xlab=xlab,ylab="Mean age")
mtext(stock,side=3,line=-1.2,cex=0.7,font=2)
##--------------------------------------------------------## ocean age
y<-meansaltage_YS_wide[,names(meansaltage_YS_wide)==stock]
lines(yrs,y,lty=1,lwd=0.5,col=alpha(cols[s],0.5))
points(yrs,y,pch=16,cex=0.7,col=cols[s])
lines(loess.smooth(yrs,y,span=span),lwd=2,col=cols[s])
##---------------------------------------------------## freshwater age
y<-meanfreshage_YS_wide[,names(meanfreshage_YS_wide)==stock]
lines(yrs,y,lty=1,lwd=0.5,col=alpha(cols[s],0.5))
points(yrs,y,pch=15,cex=0.6,col=cols[s])
lines(loess.smooth(yrs,y,span=span),lwd=2,col=cols[s])
}
dev.off()

##=============================================## compare FW vs SW growth
pdf("Growth-ocean-vs-freshwater-increments.pdf",width=5,height=4)
par(mar=c(3,3,1,1),mgp=c(1.75,0.5,0),tcl=-0.3,cex.lab=1.1,cex.axis=0.9)
plot(NA,NA,xlim=c(min(years),max(years)),ylim=c(470,590),xlab=xlab,ylab="Mean size (mm)")
colors<-c("firebrick","goldenrod","chocolate")
plotages<-c("1.3","2.2","1.2")
legend("topright",plotages,lwd=1,pch=16,bty="n",cex=0.8,col=colors)
data13s<-meanSaA_YA[meanSaA_YA$age==plotages[1],-1]
matlines(data13s[,1],data13s[,2],lty=1,lwd=1,col=colors[1])
matpoints(data13s[,1],data13s[,2],pch=16,col=colors[1]) 
abline(h=mean(data13s[,2]),lty=2,col=colors[1])
data22s<-meanSaA_YA[meanSaA_YA$age==plotages[2],-1]
matlines(data22s[,1],data22s[,2],lty=1,lwd=1,col=colors[2])
matpoints(data22s[,1],data22s[,2],pch=16,col=colors[2]) 
abline(h=mean(data22s[,2]),lty=2,col=colors[2])
data12s<-meanSaA_YA[meanSaA_YA$age==plotages[3],-1]
matlines(data12s[,1],data12s[,2],lty=1,lwd=1,col=colors[3])
matpoints(data12s[,1],data12s[,2],pch=16,col=colors[3]) 
abline(h=mean(data12s[,2]),lty=2,col=colors[3])
dev.off()

##=============================================## compare FW vs SW growth
use_ages<-c("1.2","1.3","2.2")
meanSaA_YA_wide<-meanSaA_YA %>% pivot_wider(names_from=age,values_from=meansize) %>% data.frame
names(meanSaA_YA_wide)[-1]<-gsub("X","",names(meanSaA_YA_wide)[-1])
meanSaA_YA_wide<-meanSaA_YA_wide[order(meanSaA_YA_wide$year),]
years<-meanSaA_YA_wide$year
gdata<-meanSaA_YA_wide[,names(meanSaA_YA_wide) %in% use_ages]
names(gdata)<-c("age12","age13","age22")
##----------------------------------## added growth in FW vs SW by cohort
gdata$add1FW<-gdata$age22-gdata$age12
gdata$add1SW<-gdata$age13-gdata$age12
##-----------------------------------------------------------------## plot
pdf("Growth-increment-extra-year-in-SW-vs-FW.pdf",width=5,height=4)
par(mar=c(3,3,1,1),mgp=c(1.75,0.5,0),tcl=-0.3,cex.lab=1.1,cex.axis=0.9)
span<-0.45
colors<-c("forestgreen","dodgerblue")
if(year.type=="brood") { xlab<-"Brood year" } else { xlab<-"Run year" }
x<-years
plot(NA,NA,xlab=xlab,ylab="Mean size increment (mm)",xlim=c(min(x),max(x)), ylim=c(min(gdata$add1FW),max(gdata$add1SW)))
##------------------------------------## 1 year of SW growth for FW-1 fish
y<-gdata$add1SW
matlines(x,y,lty=1,lwd=1,col=colors[1])
matpoints(x,y,pch=16,col=colors[1]) 
lines(loess.smooth(x,y,span=span),lwd=2,col=colors[1])
##--------------------------------------------------## 1 year of FW growth
y<-gdata$add1FW
matlines(x,y,lty=1,lwd=1,col=colors[2])
matpoints(x,y,pch=16,col=colors[2]) 
lines(loess.smooth(x,y,span=span),lwd=2,col=colors[2])
##--------------------------------------------------## legend
#legend("right",c("additional year in the ocean (1.2 > 1.3)", "additional year in freshwater (1.2 > 2.2)"),pch=16,lwd=1,cex=0.8,col=colors,bty="n")
##-----------------------------------------------------------------## save
dev.off()

##-------------------------------------## average growth benefit FW vs SW
av_growth_added_FW<-mean(gdata$add1FW)
av_growth_added_SW_FW1<-mean(gdata$add1SW)
av_growth_added_SW_FW1/av_growth_added_FW 
## assuming no size-selective mortality prior to terminal fisheries 
## trade-off between 2nd year in FW vs 3rd in SW: 1 extra year in SW means ~4.7 times as much growth as 1 extra year in FW (across rivers and years)
## this is  likely an underestimate of the ocean growth benefit, because fish that mature as sw-2s are on average larger than those that do not mature and stray to return as sw-3s, and fish that migrate out as fw-1s are on average larger than those that stay to migrate out as fw-2s (the latter bias is smaller though, such that )

##=======================================================## size variance
pdf("Size-variance-trends.pdf",width=5,height=4)
par(mar=c(3,3,1,1),mgp=c(1.75,0.5,0),tcl=-0.3,cex.lab=1.1,cex.axis=0.9)
layout(matrix(c(1:2),ncol=2,byrow=T),widths=c(0.8,0.2))
span<-0.45
yrs<-sdsize_YS_wide[,1];xlim<-c(min(yrs),max(yrs))
if(year.type=="brood"){xlab<-"Brood year"}else{xlab<-"Run year"}
yqs<-quantile(sdsize_YS_wide[,-1],probs=c(0,1),na.rm=T)
ylim<-c(yqs[1],yqs[2])
plot(NA,NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="Standard deviation in size (mm)")
matlines(yrs,sdsize_YS_wide[,-1],lty=1,lwd=1,col=alpha(cols,0.5))
matpoints(yrs,sdsize_YS_wide[,-1],pch=16,cex=0.8,col=cols) ## each stock
##-------------------------------------------## smoother on overall mean
lines(yrs,sdsize_Y[,-1],type="o",pch=16,lwd=1,cex=1,col=1)
lines(loess.smooth(yrs,sdsize_Y[,-1],degree=1,span=span),lwd=2,col=1)
##--------------------------------------------------------------## legend
par(mar=c(0,0,0,0));plot(NA,NA,axes=F,xaxt="n",yaxt="n",xlab="",ylab="",xlim=c(0,1),ylim=c(0,1));legend("left",c("Bristol Bay",rivers),pch=16,lwd=1,cex=0.7,bty="n",col=c("black",cols))
dev.off()

##=================================## size-at-age anomaly trend by system
pdf("Size-at-age-trend-by-system.pdf",width=5,height=4)
par(mar=c(3.5,3.5,1,1),mgp=c(1.75,0.5,0),tcl=-0.3,cex.lab=1,cex.axis=0.8)
yrs<-meansize_YS[,1];xlim<-c(min(yrs),max(yrs))
if(year.type=="brood"){xlab<-"Brood year"}else{xlab<-"Run year"}
yqs<-quantile(SaA_anomaly_YS[,-1],probs=c(0,1),na.rm=T)
ylim<-c(yqs[1],yqs[2])
yrs<-SaA_anomaly_YS$year
plot(NA,NA,xlim=xlim,ylim=ylim,xlab="",ylab="Mean size-at-age anomaly (mm)")
matlines(yrs,SaA_anomaly_YS[,-1],lty=1,lwd=0.5,col=alpha(cols,0.5))
matpoints(yrs,SaA_anomaly_YS[,-1],pch=16,cex=0.8,col=cols) 
for(s in 1:nS) lines(loess.smooth(yrs,SaA_anomaly_YS[,s+1],span=0.35),lwd=2,col=cols[s])
lnames<-names(SaA_anomaly_YS)[-1]
legend("topright",lnames,pch=16,lwd=1,cex=0.6,bty="n",col=cols)
mtext(xlab,side=1,line=2,cex=1)
dev.off()

##===========================================## mean size trend by system
center<-F
pdf("Size-trend-by-system.pdf",width=5,height=4)
par(mar=c(3.5,3.5,1,1),mgp=c(1.75,0.5,0),tcl=-0.3,cex.lab=1,cex.axis=0.8)
yrs<-meansize_YS[,1];xlim<-c(min(yrs),max(yrs))
if(year.type=="brood"){xlab<-"Brood year"}else{xlab<-"Run year"}
meansize_YS_std<-apply(meansize_YS[,-1],2,function(x) scale(x,center=center,scale=F))
yqs<-quantile(meansize_YS_std,probs=c(0,1),na.rm=T);ylim<-c(yqs[1],yqs[2])
plot(NA,NA,xlim=xlim,ylim=ylim,xlab="",ylab="Mean size (mm)")
matlines(yrs,meansize_YS_std,lty=1,lwd=0.5,col=alpha(cols,0.5))
matpoints(yrs,meansize_YS_std,pch=16,cex=0.8,col=cols)
for(s in 1:nS) lines(loess.smooth(yrs,meansize_YS_std[,s],span=0.45),lwd=2, col=cols[s])
legend("topright",lnames,pch=16,lwd=1,cex=0.6,bty="n",col=cols)
mtext(xlab,side=1,line=2,cex=1)
dev.off()

##===========================================## mean age trend by system
# center<-T
# pdf("Ocean-age-trend-by-system.pdf",width=5,height=4)
# par(mar=c(3.5,3.5,1,1),mgp=c(1.75,0.5,0),tcl=-0.3,cex.lab=1,cex.axis=0.8)
# yrs<-meansaltage_YS_wide[,1];xlim<-c(min(yrs),max(yrs))
# if(year.type=="brood"){xlab<-"Brood year"}else{xlab<-"Run year"}
# meanage_YS_std<-apply(meansaltage_YS_wide[,-1],2,function(x) scale(x,center=center,scale=F))
# yqs<-quantile(meanage_YS_std,probs=c(0,1),na.rm=T);ylim<-c(yqs[1],yqs[2])
# plot(NA,NA,xlim=xlim,ylim=ylim,xlab="",ylab="Mean ocean age")
# matlines(yrs,meanage_YS_std,lty=1,lwd=0.5,col=alpha(cols,0.5))
# matpoints(yrs,meanage_YS_std,pch=16,cex=0.8,col=cols)
# for(s in 1:nS) lines(loess.smooth(yrs,meanage_YS_std[,s],span=0.55),lwd=2, col=cols[s])
# legend("bottomright",lnames,pch=16,lwd=1,cex=0.6,bty="n",col=cols)
# mtext(xlab,side=1,line=2,cex=1)
# dev.off()

##===========================================## mean age trend by system
# center<-F
# pdf("Ocean-age-trend-by-system-tests.pdf",width=5,height=4)
# par(mar=c(3.5,3.5,1,1),mgp=c(1.75,0.5,0),tcl=-0.3,cex.lab=1,cex.axis=0.8)
# if(year.type=="brood"){xlab<-"Brood year"}else{xlab<-"Run year"}
# meansaltage_YS_wide<-meansaltage_YS_wide[meansaltage_YS_wide$year>2000,]
# yrs<-meansaltage_YS_wide[,1];xlim<-c(min(yrs),max(yrs))
# meanage_YS_std<-apply(meansaltage_YS_wide[,-1],2,function(x) scale(x,center=center,scale=F))
# yqs<-quantile(meanage_YS_std,probs=c(0,1),na.rm=T)
# ylim<-c(yqs[1],yqs[2])#;ylim<-c(-0.4,0.2)
# plot(NA,NA,xlim=xlim,ylim=ylim,xlab="",ylab="Mean ocean age")
# lwds<-c(1,6,2,4,3,5,8,7)
# #matlines(yrs,meanage_YS_std,lty=1,lwd=lwds,col=alpha(cols,1))
# #matpoints(yrs,meanage_YS_std,pch=16,cex=0.8,col=cols)
# for(s in 1:nS) lines(loess.smooth(yrs,meanage_YS_std[,s],span=0.5),lwd=lwds[s], col=cols[s])
# legend("topright",lnames,pch=16,lwd=1,cex=0.6,bty="n",col=cols)
# mtext(xlab,side=1,line=2,cex=1)
# dev.off()


##=====================================================================##
##==========================================================## main plots
##=====================================================================##
center<-F ## center time series (TRUE) or not (FALSE)?
pdf("Size-and-age-trends-main.pdf",width=6,height=5)
layout(matrix(c(1:6),ncol=3,byrow=T),widths=c(0.42,0.42,0.16))
par(mar=c(3,3,1,1),mgp=c(1.75,0.5,0),tcl=-0.3,cex.lab=1.1,cex.axis=0.9)
span<-0.45
ylimQs<-c(0,1)
yrs<-meansize_YS[,1];xlim<-c(min(yrs),max(yrs))
if(year.type=="brood"){xlab<-"Brood year"}else{xlab<-"Run year"}
##==========================================================## mean size
meansize_YS_std<-apply(meansize_YS[,-1],2,function(x) scale(x,center=center,scale=F))
yqs<-quantile(meansize_YS_std,probs=ylimQs,na.rm=T)
ylim<-c(yqs[1],yqs[2])
plot(NA,NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="Overall mean size (mm)")
matlines(yrs,meansize_YS_std,lty=1,lwd=0.5,col=alpha(cols,0.5))
matpoints(yrs,meansize_YS_std,pch=16,cex=0.8,col=cols) ## each stock
##-------------------------------------------## smoother on overall mean
meansize_Y_std<-scale(meansize_Y$meansize,center=center,scale=F)
lines(yrs,meansize_Y_std,type="o",pch=16,lwd=0.5,cex=1,col=1)
lines(loess.smooth(yrs,meansize_Y_std,degree=1,span=span),lwd=2,col=1)
##=========================================## sum of size-at-age anomaly
yqs<-quantile(SaA_anomaly_YS[,-1],probs=ylimQs,na.rm=T)
ylim<-c(yqs[1],yqs[2])
yrs<-SaA_anomaly_YS$year
plot(NA,NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="Size-at-age anomaly")
matlines(yrs,SaA_anomaly_YS[,-1],lty=1,lwd=0.5,col=alpha(cols,0.5))
matpoints(yrs,SaA_anomaly_YS[,-1],pch=16,cex=0.8,col=cols) 
##-------------------------------------------## smoother on overall mean
SaA_anomaly_Y_std<-scale(SaA_anomaly_Y$SaA_anomaly,center=center,scale=F)
lines(yrs,SaA_anomaly_Y_std,type="o",lwd=0.5,pch=16,cex=1,col=1)
lines(loess.smooth(yrs,SaA_anomaly_Y_std,degree=1,span=span),lwd=2,col=1)
##--------------------------------------------------------------## legend
par(mar=c(0,0,0,0));plot(NA,NA,axes=F,xaxt="n",yaxt="n",xlab="",ylab="",xlim=c(0,1),ylim=c(0,1))
lnames<-c("Bristol Bay",names(SaA_anomaly_YS)[-1])
legend("left",lnames,pch=16,lwd=1,cex=1,bty="n",col=c("black",cols))
par(mar=c(3,3,1,1))
##=================================================## mean freshwater age
meanfreshage_YS_std<-apply(meanfreshage_YS_wide[,-1],2,function(x) scale(x,center=center,scale=F))
yqs<-quantile(meanfreshage_YS_std,probs=ylimQs,na.rm=T)
ylim<-c(yqs[1],yqs[2])
plot(NA,NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="Mean freshwater age")
matlines(yrs,meanfreshage_YS_std,lty=1,lwd=0.5,col=alpha(cols,0.5))
matpoints(yrs,meanfreshage_YS_std,pch=16,cex=0.8,col=cols) ## each stock
##-------------------------------------------## smoother on overall mean
meanfreshage_Y_std<-scale(meanFWage_Y$meanage,center=center,scale=F)
lines(yrs,meanfreshage_Y_std,type="o",lwd=0.5,pch=16,cex=1,col=1)
lines(loess.smooth(yrs,meanfreshage_Y_std,degree=1,span=span),lwd=2,col=1)
##======================================================## mean ocean age
meansaltage_YS_std<-apply(meansaltage_YS_wide[,-1],2,function(x) scale(x,center=center,scale=F))
yqs<-quantile(meansaltage_YS_std,probs=ylimQs,na.rm=T)
ylim<-c(yqs[1],yqs[2])
plot(NA,NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="Mean ocean age")
matlines(yrs,meansaltage_YS_std,lty=1,lwd=0.5,col=alpha(cols,0.5))
matpoints(yrs,meansaltage_YS_std,pch=16,cex=0.8,col=cols) ## each stock
##-------------------------------------------## smoother on overall mean
meansaltage_Y_std<-scale(meanSWage_Y$meanage,center=center,scale=F)
lines(yrs,meansaltage_Y_std,type="o",lwd=0.5,pch=16,cex=1,col=1)
lines(loess.smooth(yrs,meansaltage_Y_std,degree=1,span=span),lwd=2,col=1)
##---------------------------------------------------------------## empty
plot(NA,NA,axes=F,xaxt="n",yaxt="n",xlab="",ylab="",xlim=c(0,1),ylim=c(0,1))
dev.off()

##=====================================================================##
##==========================================================## manuscript
##=====================================================================##

##=================================================================## v.1
pdf("Size-and-age-trends-manuscript-v1.pdf",width=3,height=6)
layout(matrix(c(1:3),nrow=3,byrow=T))
par(mar=c(1,3.5,1,1),oma=c(2.5,0,0,0),mgp=c(1.75,0.5,0),tcl=-0.3,cex.lab=1.1,cex.axis=0.9)
alphaval<-0.8;colb<-alpha(1,alphaval);pcols<-alpha(cols,alphaval)
lwd.b<-1;lws.s<-0.5
if(year.type=="brood"){ xlab<-"Brood year" } else { xlab<-"Run year" }
yrs<-meansize_YS[,1];xlim<-c(min(yrs),max(yrs))
##==========================================================## mean size
ylim<-c(min(meansize_YS[,-1],na.rm=T),max(meansize_YS[,-1],na.rm=T))
plot(NA,NA,xlim=xlim,ylim=ylim,xlab="",ylab="Mean size (mm)")
matlines(yrs,meansize_YS[,-1],lty=1,lwd=lws.s,col=pcols)
matpoints(yrs,meansize_YS[,-1],pch=16,cex=0.8,col=pcols) ## each stock
lines(yrs,meansize_Y[,2],lwd=lwd.b,col=colb)
points(yrs,meansize_Y[,2],pch=16,cex=1,col=colb)
##=========================================## average size-at-age anomaly
ylim<-c(min(SaA_anomaly_YS[,-1],na.rm=T),max(SaA_anomaly_YS[,-1],na.rm=T))
plot(NA,NA,xlim=xlim,ylim=ylim,xlab="",ylab="Mean size-at-age anomaly (mm)")
matlines(yrs,SaA_anomaly_YS[,-1],lty=1,lwd=lws.s,col=pcols)
matpoints(yrs,SaA_anomaly_YS[,-1],pch=16,cex=0.8,col=pcols) 
lines(yrs,SaA_anomaly_Y[,2],lwd=lwd.b,col=colb)
points(yrs,SaA_anomaly_Y[,2],pch=16,cex=1,col=colb)
##--------------------------------------------------------------## legend
legend("topright",c("bay-wide",rivers),pt.cex=c(1,rep(0.8,nS)),pch=16,lwd=c(lwd.b,rep(lws.s,nS)),cex=0.5,bty="n",col=c(colb,pcols))
##======================================================## mean ocean age
meansaltage_YS<-apply(meansaltage_YS_wide,2,function(x) scale(x,scale=F,center=T))
ylim<-c(min(meansaltage_YS[,-1],na.rm=T),max(meansaltage_YS[,-1],na.rm=T))
plot(NA,NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="Mean ocean age anomaly")
meansaltage_Y<-meanSWage_Y
meansaltage_Y<-scale(meanSWage_Y,scale=F,center=T)
matlines(yrs,meansaltage_YS[,-1],lty=1,lwd=1,col=pcols)
matpoints(yrs,meansaltage_YS[,-1],pch=16,cex=0.8,col=pcols)
lines(yrs,meansaltage_Y[,2],lwd=lwd.b,col=colb)
points(yrs,meansaltage_Y[,2],pch=16,cex=1,col=colb)
##--------------------------------------------------------## x-axis label
mtext(xlab,side=1,line=2,cex=0.8)
dev.off()

##=================================================================## NEW
pdf("Size-and-age-trends-manuscript.pdf",width=3,height=6)
layout(matrix(c(1:3),nrow=3,byrow=T))
par(mar=c(0.5,3.5,1.5,1),oma=c(3,0,0.5,0),mgp=c(1.75,0.5,0),tcl=-0.3,cex.lab=1.1,cex.axis=0.9)
colb<-alpha(1,1);pcols<-alpha(cols,0.5);pcols<-cols
lwd.b<-2;lws.s<-1.5
if(year.type=="brood"){ xlab<-"Brood year" } else { xlab<-"Run year" }
yrs<-meansize_YS[,1];xlim<-c(min(yrs),max(yrs))
##==========================================================## mean size
ylim<-c(min(meansize_YS[,-1],na.rm=T),max(meansize_YS[,-1],na.rm=T))
plot(NA,NA,xlim=xlim,ylim=ylim,xlab="",ylab="Mean size (mm)")
matlines(yrs,meansize_YS[,-1],lty=1,lwd=lws.s,col=pcols)
lines(yrs,meansize_Y[,2],lwd=lwd.b,col=colb)
##=========================================## average size-at-age anomaly
ylim<-c(min(SaA_anomaly_YS[,-1],na.rm=T),max(SaA_anomaly_YS[,-1],na.rm=T))
plot(NA,NA,xlim=xlim,ylim=ylim,xlab="",ylab="Mean size-at-age anomaly (mm)")
matlines(yrs,SaA_anomaly_YS[,-1],lty=1,lwd=lws.s,col=pcols)
lines(yrs,SaA_anomaly_Y[,2],lwd=lwd.b,col=colb)
##--------------------------------------------------------------## legend
legend("topright",c("bay-wide",rivers),lwd=c(lwd.b,rep(lws.s,nS)),cex=0.5,bty="n",col=c(colb,pcols))
##======================================================## mean ocean age
#meansaltage_YS<-meansaltage_YS_wide
meansaltage_YS<-apply(meansaltage_YS_wide,2,function(x) scale(x,scale=F,center=T))
ylim<-c(min(meansaltage_YS[,-1],na.rm=T),max(meansaltage_YS[,-1],na.rm=T))
plot(NA,NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="Mean ocean age anomaly")
matlines(yrs,meansaltage_YS[,-1],lty=1,lwd=lws.s,col=pcols)
#meansaltage_Y<-meanSWage_Y
meansaltage_Y<-scale(meanSWage_Y,scale=F,center=T)
lines(yrs,meansaltage_Y[,2],lwd=lwd.b,col=colb)
##--------------------------------------------------------## x-axis label
mtext(xlab,side=1,line=2,cex=0.8)
dev.off()

##=================================================================## NEW
smooth<-F
baywide<-T
pdf("Size-and-age-trends-manuscript-new.pdf",width=4,height=6.5)
layout(matrix(c(2,1,3,0,4,0),ncol=2,byrow=T),widths=c(0.8,0.2))
par(oma=c(3,0,0,0),mgp=c(1.75,0.5,0),tcl=-0.3,cex.lab=1.1,cex.axis=0.9)
lwd.b<-1.5;lws.s<-1;lwd.l<-5;cl="gray"
if(year.type=="brood"){ xlab<-"Brood year" } else { xlab<-"Run year" }
yrs<-meansize_YS[,1];xlim<-c(min(yrs),max(yrs))
if(smooth) span<-0.35
##--------------------------------------------------------------## legend
par(mar=c(0,0,0,0))
if(baywide) { lnames<-c("bay-wide",rivers) } else { lnames<-rivers }
plot(NA,NA,axes=F,xaxt="n",yaxt="n",xlab="",ylab="",xlim=c(0,1),ylim=c(0,1))
legend("topleft",lnames,lwd=2,bty="n",cex=0.9,col=c(colb,pcols),seg.len=1.5,inset=c(0,0.05)) ## lwd=c(lwd.b,rep(lws.s,nS))
##==========================================================## mean size
par(mar=c(0.5,3.5,1.5,0))
ylim<-c(min(meansize_YS[,-1],na.rm=T),max(meansize_YS[,-1],na.rm=T))
plot(NA,NA,xlim=xlim,ylim=ylim,xlab="",ylab="Mean size (mm)")
if(smooth) lines(loess.smooth(x,meansize_Y[,2],span=span),lwd=lwd.l,col=cl)
matlines(yrs,meansize_YS[,-1],lty=1,lwd=lws.s,col=cols)
if(baywide) lines(yrs,meansize_Y[,2],lwd=lwd.b,col=colb)
##=========================================## average size-at-age anomaly
par(mar=c(0.5,3.5,1.5,0))
ylim<-c(min(SaA_anomaly_YS[,-1],na.rm=T),max(SaA_anomaly_YS[,-1],na.rm=T))
plot(NA,NA,xlim=xlim,ylim=ylim,xlab="",ylab="Mean size-at-age anomaly (mm)")
if(smooth) lines(loess.smooth(x,SaA_anomaly_YS[,2],span=span),lwd=lwd.l,col=cl)
matlines(yrs,SaA_anomaly_YS[,-1],lty=1,lwd=lws.s,col=cols)
if(baywide) lines(yrs,SaA_anomaly_Y[,2],lwd=lwd.b,col=colb)
##======================================================## mean ocean age
par(mar=c(0.5,3.5,1.5,0))
center<-T
ylab<-"Mean ocean age (years)";if(center) ylab<-"Mean ocean age anomaly"
meansaltage_YS<-apply(meansaltage_YS_wide,2,function(x) scale(x,scale=F,center=center))
ylim<-c(min(meansaltage_YS[,-1],na.rm=T),max(meansaltage_YS[,-1],na.rm=T))
plot(NA,NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab)
meansaltage_Y<-scale(meanSWage_Y,scale=F,center=center)
if(smooth) lines(loess.smooth(x,meansaltage_Y[,2],span=span),lwd=lwd.l,col=cl)
matlines(yrs,meansaltage_YS[,-1],lty=1,lwd=lws.s,col=cols)
if(baywide) lines(yrs,meansaltage_Y[,2],lwd=lwd.b,col=colb)
##--------------------------------------------------------## x-axis label
mtext(xlab,side=1,line=2,cex=0.75)
dev.off()

##=================================================================## NEW
pdf("Size-and-age-trends-bay-wide-compared.pdf",width=7.5,height=2.5)
layout(matrix(c(1,2,3),nrow=1,byrow=T))
par(mar=c(3.5,3.5,0,0),oma=c(0,0,1,1),mgp=c(1.8,0.5,0),tcl=-0.3,cex.lab=1.2,cex.axis=0.9)
if(year.type=="brood"){ xlab<-"Brood year" } else { xlab<-"Run year" }
x<-meansize_Y[,1];xlim<-c(min(x),max(x))
colors<-c("#C52C2E","#60ADB6","#85501A")
span<-0.45;lwd1<-0.5;lwd2<-2
nYref<-5 ## reference period at beginning of time series
scale<-F;center<-F
# ylim<-c(-2.75,2.75)
##==========================================================## mean size
y<-scale(meansize_Y[,2],scale=scale,center=center)
y<-y-mean(y[1:nYref]) ## relative to mean of first five years
ylim<-c(1.1*min(y),1.1*max(y))
plot(NA,NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="Change in mean size")
lines(x,y,lwd=lwd1,col=colors[1])
lines(loess.smooth(x,y,span=span),lwd=lwd2,col=colors[1])
##=========================================## average size-at-age anomaly
y<-scale(SaA_anomaly_Y[,2],scale=scale,center=center) 
y<-y-mean(y[1:nYref]) ## relative to mean of first five years
ylim<-c(1.1*min(y),1.1*max(y))
plot(NA,NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="Change in mean size-at-age")
lines(x,y,lwd=lwd1,col=colors[2])
lines(loess.smooth(x,y,span=span),lwd=lwd2,col=colors[2])
##======================================================## mean ocean age
y<-scale(meanSWage_Y[,2],scale=scale,center=center) 
y<-y-mean(y[1:nYref]) ## relative to mean of first five years
ylim<-c(3.1*min(y),1.1*max(y))
plot(NA,NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="Change in mean ocean age")
lines(x,y,lwd=lwd1,col=colors[3])
lines(loess.smooth(x,y,span=span),lwd=lwd2,col=colors[3])
##==================================================## mean freshwater age
y<-scale(meanFWage_Y[,2],scale=scale,center=center) 
y<-y-mean(y[1:nYref]) ## relative to mean of first five years
#ylim<-c(1.1*min(y),1.1*max(y))
#plot(NA,NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab="Change in mean ocean age")
lines(x,y,lwd=lwd1,col=colors[3])
lines(loess.smooth(x,y,span=span),lwd=lwd2,col=colors[3])
##-----------------------------------------------------------## save plot
dev.off() 
## change relative to the first five years on a standardized scale

##===========================================## mean size trends for map
pdf("Mean-size-trend-by-system-for-map.pdf",width=7.3,height=6.5)
layout(matrix(c(1,2,3,4,0,0,0,5,0,0,0,6,0,0,0,7),ncol=4,nrow=4,byrow=T))
par(mar=c(2,2,0,0),oma=c(2,2,1,1),mgp=c(2,0.5,0),tcl=-0.3)
x<-meansize_YS[,1];xlim<-c(min(x),max(x))
ylim<-c(480,600)
for(s in 1:nS){
y<-meansize_YS[,1+s]
#ylim<-c(min(y),max(y))
plot(NA,NA,xaxt="n",yaxt="n",xlim=xlim,ylim=ylim,xlab="",ylab="")
if(s %in% c(1,5,6,7)) mtext("Mean size (mm)",side=2,line=2,cex=0.8)
if(s %in% c(1,2,3)) mtext("Brood year",side=1,line=2,cex=0.8)
axis(1,at=seq(1960,2020,10),label=T,cex.axis=0.8)
axis(2,at=seq(480,600,20),label=T,cex.axis=0.8)
lines(x,y,lwd=1.5,col=cols[s])
#lines(loess.smooth(x,y,span=0.45),lwd=2,col=cols[s])
mtext(paste0(rivers[s]," "),side=3,line=-1,cex=0.6,font=2,adj=1,col=cols[s])
}
mtext("Brood year",side=1,line=2,cex=0.8)
dev.off() 

##=====================================================================##
##==============## supplementary figure with all trends by rivers as rows
##=====================================================================##
smooth<-T
pdf("Size-and-age-trends-by-system-supplement.pdf",width=6.5,height=6.5)
layout(matrix(seq(nS*4),ncol=4,nrow=nS,byrow=F))
par(mar=c(0.5,3.5,0,0),oma=c(3,0,3,3),mgp=c(1.5,0.5,0),tcl=-0.3,cex.lab=1.1,cex.axis=0.9)
if(year.type=="brood"){ xlab<-"Brood year" } else { xlab<-"Run year" }
x<-years;xlim<-c(min(x),max(x))
if(smooth) span<-0.35
lws.s<-0.5;lwd.l<-2
##==========================================================## mean size
for(s in 1:nS){
  y<-meansize_YS[,s+1]
  ylim<-c(min(y,na.rm=T),max(y,na.rm=T))
  main=""#;if(s==1) main<-"Mean size"
  plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",xlab="",ylab="",main=main)
  if(smooth) lines(loess.smooth(x,y,span=span),lwd=lwd.l,col=cols[s])
  lines(yrs,y,lty=1,lwd=lws.s,col=cols[s])
  if(s==1) mtext("Mean size",side=3,line=0.8,font=2,cex=0.8)
  if(s==4) mtext("Mean size (mm)",side=2,line=1.8,cex=0.8)
  if(s==nS) axis(1,at=seq(1960,2020,10),label=T,cex.axis=0.8)
  if(s==nS) mtext("Brood year",side=1,line=2,cex=0.8)
}
##=========================================## average size-at-age anomaly
for(s in 1:nS){
  y<-SaA_anomaly_YS[,s+1]
  ylim<-c(min(y,na.rm=T),max(y,na.rm=T))
  main=""#;if(s==1) main<-"Size-at-age"
  plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",xlab="",ylab="",main=main)
  if(smooth) lines(loess.smooth(x,y,span=span),lwd=lwd.l,col=cols[s])
  lines(yrs,y,lty=1,lwd=lws.s,col=cols[s])
  if(s==1) mtext("Size-at-age",side=3,line=0.8,font=2,cex=0.8)
  if(s==4) mtext("Mean size-at-age anomaly (mm)",side=2,line=1.8,cex=0.8)
  if(s==nS) axis(1,at=seq(1960,2020,10),label=T,cex.axis=0.8)
  if(s==nS) mtext("Brood year",side=1,line=2,cex=0.8)
}
##======================================================## mean ocean age
for(s in 1:nS){
  y<-meansaltage_YS_wide[,s+1]
  ylim<-c(min(y,na.rm=T),max(y,na.rm=T))
  main=""#;if(s==1) main<-"Ocean age"
  plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",xlab="",ylab="",main=main)
  if(smooth) lines(loess.smooth(x,y,span=span),lwd=lwd.l,col=cols[s])
  lines(yrs,y,lty=1,lwd=lws.s,col=cols[s])
  if(s==1) mtext("Ocean age",side=3,line=0.8,font=2,cex=0.8)
  if(s==4) mtext("Mean ocean age (years)",side=2,line=1.8,cex=0.8)
  if(s==nS) axis(1,at=seq(1960,2020,10),label=T,cex.axis=0.8)
  if(s==nS) mtext("Brood year",side=1,line=2,cex=0.8)
}
##=================================================## mean freshwater age
for(s in 1:nS){
  y<-meanfreshage_YS_wide[,s+1]
  ylim<-c(min(y,na.rm=T),max(y,na.rm=T))
  main=""#;if(s==1) main<-"Freshwater age"
  plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",xlab="",ylab="",main=main)
  if(smooth) lines(loess.smooth(x,y,span=span),lwd=lwd.l,col=cols[s])
  lines(yrs,y,lty=1,lwd=lws.s,col=cols[s])
  if(s==1) mtext("Freshwater age",side=3,line=0.8,font=2,cex=0.8)
  if(s==4) mtext("Mean freshwater age (years)",side=2,line=1.8,cex=0.8)
  if(s==nS) axis(1,at=seq(1960,2020,10),label=T,cex.axis=0.8)
  if(s==nS) mtext("Brood year",side=1,line=2,cex=0.8)
  mtext(paste0(rivers[s]," "),side=4,line=1,cex=0.8,font=2,las=0,col=cols[s])
}
dev.off()


##=====================================================================##
##====================## main figure with all trends by rivers as columns
##=====================================================================##
baywide<-T
if(baywide) { pdf("Size-and-age-trends-by-river.pdf",width=14,height=5.5) } else { pdf("Size-and-age-trends-by-river.pdf",width=12,height=5) }
if(baywide) { layout(matrix(seq((nS+1)*4),ncol=(nS+1),nrow=4,byrow=T)) } else { layout(matrix(seq(nS*4),ncol=nS,nrow=4,byrow=T)) }
par(mar=c(0.5,0.5,0,0),oma=c(3,4,2,1),mgp=c(1.5,0.5,0),tcl=-0.3)
if(year.type=="brood"){ xlab<-"Brood year" } else { xlab<-"Run year" }
x<-years;nY<-length(years);xlim<-c(min(x),max(x))
lws.l<-1;cols.l<-cols ## raw data lines
lwd.g<-2;cols.gl<-alpha(cols,0.5);cols.gp<-alpha(cols,0.1);edge<-"white"
cols.bwl<-alpha(1,0.5);cols.bwp<-alpha(1,0.1)
##===========================================================## mean size
ylim<-c(475,605)
##--------------------------------------------------------------## rivers
for(s in 1:nS){
y<-meansize_YS[,s+1]
plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
gam_y<-gam(y~s(x),kmethod="REML")
gam_pred<-visreg(gam_y,plot=F)
low<-gam_pred$fit$visregLwr
upp<-gam_pred$fit$visregUpr
fit<-gam_pred$fit$visregFit
yrs<-gam_pred$fit$x
poly.x<-c(yrs,rev(yrs))
poly.y<-c(low,rev(upp))
polygon(poly.x,poly.y,lwd=0.1,col=cols.gp[s],border=edge)
lines(yrs,fit,lwd=lwd.g,col=cols.gl[s])
lines(x,y,lty=1,lwd=lws.l,col=cols.l[s])
if(s==1) axis(2,at=seq(480,600,20),label=T,cex.axis=0.8)
if(s==1) mtext("Mean size\n(mm)",side=2,line=1.8,cex=0.8)
mtext(paste0(rivers[s]," "),side=3,line=0.5,cex=1,font=2,col=cols[s])
}
##------------------------------------------------------------## bay-wide
if(baywide){
y<-meansize_Y[,2]
plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
gam_y<-gam(y~s(x),kmethod="REML")
gam_pred<-visreg(gam_y,plot=F)
low<-gam_pred$fit$visregLwr
upp<-gam_pred$fit$visregUpr
fit<-gam_pred$fit$visregFit
yrs<-gam_pred$fit$x
poly.x<-c(yrs,rev(yrs))
poly.y<-c(low,rev(upp))
polygon(poly.x,poly.y,lwd=0.1,col=cols.bwp,border=edge)
lines(yrs,fit,lwd=lwd.g,col=cols.bwl)
lines(x,y,lty=1,lwd=lws.l,col=1)
mtext("bay-wide ",side=3,line=0.5,cex=1,font=2,col=1)
}
##=========================================## average size-at-age anomaly
ylim<-c(-25,35)
##--------------------------------------------------------------## rivers
for(s in 1:nS){
y<-SaA_anomaly_YS[,s+1]
plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
gam_y<-gam(y~s(x),kmethod="REML")
gam_pred<-visreg(gam_y,plot=F)
low<-gam_pred$fit$visregLwr
upp<-gam_pred$fit$visregUpr
fit<-gam_pred$fit$visregFit
yrs<-gam_pred$fit$x
poly.x<-c(yrs,rev(yrs))
poly.y<-c(low,rev(upp))
polygon(poly.x,poly.y,lwd=0.1,col=cols.gp[s],border=edge)
lines(yrs,fit,lwd=lwd.g,col=cols.gl[s])
lines(x,y,lty=1,lwd=lws.l,col=cols.l[s])
if(s==1) axis(2,at=seq(-20,30,10),label=T,cex.axis=0.8)
if(s==1) mtext("Mean size-at-age\nanomaly (mm)",side=2,line=1.8,cex=0.8)
}
##------------------------------------------------------------## bay-wide
if(baywide){
y<-SaA_anomaly_Y[,2]
plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
gam_y<-gam(y~s(x),kmethod="REML")
gam_pred<-visreg(gam_y,plot=F)
low<-gam_pred$fit$visregLwr
upp<-gam_pred$fit$visregUpr
fit<-gam_pred$fit$visregFit
yrs<-gam_pred$fit$x
poly.x<-c(yrs,rev(yrs))
poly.y<-c(low,rev(upp))
polygon(poly.x,poly.y,lwd=0.1,col=cols.bwp,border=edge)
lines(yrs,fit,lwd=lwd.g,col=cols.bwl)
lines(x,y,lty=1,lwd=lws.l,col=1)
}
##======================================================## mean ocean age
ylim<-c(1.9,3.1)
##--------------------------------------------------------------## rivers
for(s in 1:nS){
y<-meansaltage_YS_wide[,s+1]
plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
gam_y<-gam(y~s(x),kmethod="REML")
gam_pred<-visreg(gam_y,plot=F)
low<-gam_pred$fit$visregLwr
upp<-gam_pred$fit$visregUpr
fit<-gam_pred$fit$visregFit
yrs<-gam_pred$fit$x
poly.x<-c(yrs,rev(yrs))
poly.y<-c(low,rev(upp))
polygon(poly.x,poly.y,lwd=0.1,col=cols.gp[s],border=edge)
lines(yrs,fit,lwd=lwd.g,col=cols.gl[s])
lines(x,y,lty=1,lwd=lws.l,col=cols.l[s])
if(s==1) axis(2,at=seq(2,3,0.2),label=T,cex.axis=0.8)
if(s==1) mtext("Mean ocean\nage (years)",side=2,line=1.8,cex=0.8)
}
##------------------------------------------------------------## bay-wide
if(baywide){
y<-meanSWage_Y[,2]
plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
gam_y<-gam(y~s(x),kmethod="REML")
gam_pred<-visreg(gam_y,plot=F)
low<-gam_pred$fit$visregLwr
upp<-gam_pred$fit$visregUpr
fit<-gam_pred$fit$visregFit
yrs<-gam_pred$fit$x
poly.x<-c(yrs,rev(yrs))
poly.y<-c(low,rev(upp))
polygon(poly.x,poly.y,lwd=0.1,col=cols.bwp,border=edge)
lines(yrs,fit,lwd=lwd.g,col=cols.bwl)
lines(x,y,lty=1,lwd=lws.l,col=1)  
}
##=================================================## mean freshwater age
ylim<-c(0.4,2.1)
##--------------------------------------------------------------## rivers
for(s in 1:nS){
y<-meanfreshage_YS_wide[,s+1]
plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
gam_y<-gam(y~s(x),kmethod="REML")
gam_pred<-visreg(gam_y,plot=F)
low<-gam_pred$fit$visregLwr
upp<-gam_pred$fit$visregUpr
fit<-gam_pred$fit$visregFit
yrs<-gam_pred$fit$x
poly.x<-c(yrs,rev(yrs))
poly.y<-c(low,rev(upp))
polygon(poly.x,poly.y,lwd=0.1,col=cols.gp[s],border=edge)
lines(yrs,fit,lwd=lwd.g,col=cols.gl[s])
lines(x,y,lty=1,lwd=lws.l,col=cols.l[s])
if(s==1) axis(2,at=seq(0.5,2,0.5),label=T,cex.axis=0.8)
if(s==1) mtext("Mean freshwater\nage (years)",side=2,line=1.8,cex=0.8)
axis(1,at=seq(1960,2020,10),label=T,cex.axis=0.8)
mtext("Brood year",side=1,line=2,cex=0.8)
}
##------------------------------------------------------------## bay-wide
if(baywide){
y<-meanFWage_Y[,2]
plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
gam_y<-gam(y~s(x),kmethod="REML")
gam_pred<-visreg(gam_y,plot=F)
low<-gam_pred$fit$visregLwr
upp<-gam_pred$fit$visregUpr
fit<-gam_pred$fit$visregFit
yrs<-gam_pred$fit$x
poly.x<-c(yrs,rev(yrs))
poly.y<-c(low,rev(upp))
polygon(poly.x,poly.y,lwd=0.1,col=cols.bwp,border=edge)
lines(yrs,fit,lwd=lwd.g,col=cols.bwl)
lines(x,y,lty=1,lwd=lws.l,col=1)
axis(1,at=seq(1960,2020,10),label=T,cex.axis=0.8)
mtext("Brood year",side=1,line=2,cex=0.8)
}
##-----------------------------------------------------------## save plot
dev.off()



##=====================================================================##
##=========================## same as above but colors by size/age metric
##=====================================================================##
baywide<-T
if(baywide) { pdf("Size-and-age-trends-by-river2.pdf",width=14,height=5.5) } else { pdf("Size-and-age-trends-by-river2.pdf",width=12,height=5) }
if(baywide) { layout(matrix(seq((nS+1)*4),ncol=(nS+1),nrow=4,byrow=T)) } else { layout(matrix(seq(nS*4),ncol=nS,nrow=4,byrow=T)) }
par(mar=c(0.5,0.5,0,0),oma=c(3,4,2,1),mgp=c(1.5,0.5,0),tcl=-0.3)
if(year.type=="brood"){ xlab<-"Brood year" } else { xlab<-"Run year" }
x<-years;nY<-length(years);xlim<-c(min(x),max(x))
lws.l<-1;lwd.g<-2;edge<-"white"
cols_new<-c("#C52C2E","#60ADB6","darkgray","darkgray")
##===========================================================## mean size
ylim<-c(475,605)
##--------------------------------------------------------------## rivers
for(s in 1:nS){
  y<-meansize_YS[,s+1]
  plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
  gam_y<-gam(y~s(x),kmethod="REML")
  gam_pred<-visreg(gam_y,plot=F)
  low<-gam_pred$fit$visregLwr
  upp<-gam_pred$fit$visregUpr
  fit<-gam_pred$fit$visregFit
  yrs<-gam_pred$fit$x
  poly.x<-c(yrs,rev(yrs))
  poly.y<-c(low,rev(upp))
  polygon(poly.x,poly.y,lwd=0.1,col=alpha(cols_new[1],0.1),border=edge)
  lines(yrs,fit,lwd=lwd.g,col=alpha(cols_new[1],0.5))
  lines(x,y,lty=1,lwd=lws.l,col=cols_new[1])
  if(s==1) axis(2,at=seq(480,600,20),label=T,cex.axis=0.8)
  if(s==1) mtext("Mean size\n(mm)",side=2,line=1.8,cex=0.8)
  mtext(paste0(rivers[s]," "),side=3,line=0.5,cex=1,font=2,col=1)
}
##------------------------------------------------------------## bay-wide
if(baywide){
  y<-meansize_Y[,2]
  plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
  gam_y<-gam(y~s(x),kmethod="REML")
  gam_pred<-visreg(gam_y,plot=F)
  low<-gam_pred$fit$visregLwr
  upp<-gam_pred$fit$visregUpr
  fit<-gam_pred$fit$visregFit
  yrs<-gam_pred$fit$x
  poly.x<-c(yrs,rev(yrs))
  poly.y<-c(low,rev(upp))
  polygon(poly.x,poly.y,lwd=0.1,col=alpha(cols_new[1],0.1),border=edge)
  lines(yrs,fit,lwd=lwd.g,col=alpha(cols_new[1],0.5))
  lines(x,y,lty=1,lwd=lws.l,col=cols_new[1])
  mtext("bay-wide ",side=3,line=0.5,cex=1,font=2,col=1)
}
##=========================================## average size-at-age anomaly
ylim<-c(-25,35)
##--------------------------------------------------------------## rivers
for(s in 1:nS){
  y<-SaA_anomaly_YS[,s+1]
  plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
  gam_y<-gam(y~s(x),kmethod="REML")
  gam_pred<-visreg(gam_y,plot=F)
  low<-gam_pred$fit$visregLwr
  upp<-gam_pred$fit$visregUpr
  fit<-gam_pred$fit$visregFit
  yrs<-gam_pred$fit$x
  poly.x<-c(yrs,rev(yrs))
  poly.y<-c(low,rev(upp))
  polygon(poly.x,poly.y,lwd=0.1,col=alpha(cols_new[2],0.1),border=edge)
  lines(yrs,fit,lwd=lwd.g,col=alpha(cols_new[2],0.5))
  lines(x,y,lty=1,lwd=lws.l,col=cols_new[2])
  if(s==1) axis(2,at=seq(-20,30,10),label=T,cex.axis=0.8)
  if(s==1) mtext("Mean size-at-age\nanomaly (mm)",side=2,line=1.8,cex=0.8)
}
##------------------------------------------------------------## bay-wide
if(baywide){
  y<-SaA_anomaly_Y[,2]
  plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
  gam_y<-gam(y~s(x),kmethod="REML")
  gam_pred<-visreg(gam_y,plot=F)
  low<-gam_pred$fit$visregLwr
  upp<-gam_pred$fit$visregUpr
  fit<-gam_pred$fit$visregFit
  yrs<-gam_pred$fit$x
  poly.x<-c(yrs,rev(yrs))
  poly.y<-c(low,rev(upp))
  polygon(poly.x,poly.y,lwd=0.1,col=alpha(cols_new[2],0.1),border=edge)
  lines(yrs,fit,lwd=lwd.g,col=alpha(cols_new[2],0.5))
  lines(x,y,lty=1,lwd=lws.l,col=cols_new[2])
}
##======================================================## mean ocean age
ylim<-c(1.9,3.1)
##--------------------------------------------------------------## rivers
for(s in 1:nS){
  y<-meansaltage_YS_wide[,s+1]
  plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
  gam_y<-gam(y~s(x),kmethod="REML")
  gam_pred<-visreg(gam_y,plot=F)
  low<-gam_pred$fit$visregLwr
  upp<-gam_pred$fit$visregUpr
  fit<-gam_pred$fit$visregFit
  yrs<-gam_pred$fit$x
  poly.x<-c(yrs,rev(yrs))
  poly.y<-c(low,rev(upp))
  polygon(poly.x,poly.y,lwd=0.1,col=alpha(cols_new[3],0.1),border=edge)
  lines(yrs,fit,lwd=lwd.g,col=alpha(cols_new[3],0.5))
  lines(x,y,lty=1,lwd=lws.l,col=cols_new[3])
  if(s==1) axis(2,at=seq(2,3,0.2),label=T,cex.axis=0.8)
  if(s==1) mtext("Mean ocean\nage (years)",side=2,line=1.8,cex=0.8)
}
##------------------------------------------------------------## bay-wide
if(baywide){
  y<-meanSWage_Y[,2]
  plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
  gam_y<-gam(y~s(x),kmethod="REML")
  gam_pred<-visreg(gam_y,plot=F)
  low<-gam_pred$fit$visregLwr
  upp<-gam_pred$fit$visregUpr
  fit<-gam_pred$fit$visregFit
  yrs<-gam_pred$fit$x
  poly.x<-c(yrs,rev(yrs))
  poly.y<-c(low,rev(upp))
  polygon(poly.x,poly.y,lwd=0.1,col=alpha(cols_new[3],0.1),border=edge)
  lines(yrs,fit,lwd=lwd.g,col=alpha(cols_new[3],0.5))
  lines(x,y,lty=1,lwd=lws.l,col=cols_new[3])
}
##=================================================## mean freshwater age
ylim<-c(0.4,2.1)
##--------------------------------------------------------------## rivers
for(s in 1:nS){
  y<-meanfreshage_YS_wide[,s+1]
  plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
  gam_y<-gam(y~s(x),kmethod="REML")
  gam_pred<-visreg(gam_y,plot=F)
  low<-gam_pred$fit$visregLwr
  upp<-gam_pred$fit$visregUpr
  fit<-gam_pred$fit$visregFit
  yrs<-gam_pred$fit$x
  poly.x<-c(yrs,rev(yrs))
  poly.y<-c(low,rev(upp))
  polygon(poly.x,poly.y,lwd=0.1,col=alpha(cols_new[4],0.1),border=edge)
  lines(yrs,fit,lwd=lwd.g,col=alpha(cols_new[4],0.5))
  lines(x,y,lty=1,lwd=lws.l,col=cols_new[4])
  if(s==1) axis(2,at=seq(0.5,2,0.5),label=T,cex.axis=0.8)
  if(s==1) mtext("Mean freshwater\nage (years)",side=2,line=1.8,cex=0.8)
  axis(1,at=seq(1960,2020,10),label=T,cex.axis=0.8)
  mtext("Brood year",side=1,line=2,cex=0.8)
}
##------------------------------------------------------------## bay-wide
if(baywide){
  y<-meanFWage_Y[,2]
  plot(NA,NA,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
  gam_y<-gam(y~s(x),kmethod="REML")
  gam_pred<-visreg(gam_y,plot=F)
  low<-gam_pred$fit$visregLwr
  upp<-gam_pred$fit$visregUpr
  fit<-gam_pred$fit$visregFit
  yrs<-gam_pred$fit$x
  poly.x<-c(yrs,rev(yrs))
  poly.y<-c(low,rev(upp))
  polygon(poly.x,poly.y,lwd=0.1,col=alpha(cols_new[4],0.1),border=edge)
  lines(yrs,fit,lwd=lwd.g,col=alpha(cols_new[4],0.5))
  lines(x,y,lty=1,lwd=lws.l,col=cols_new[4])
  axis(1,at=seq(1960,2020,10),label=T,cex.axis=0.8)
  mtext("Brood year",side=1,line=2,cex=0.8)
}
##-----------------------------------------------------------## save plot
dev.off()

##=====================================================================##
##=====================================================================##
##=====================================================================##