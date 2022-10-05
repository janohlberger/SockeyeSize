##======================================================================##
##                                                                      ##
## Assessment of harvest rate, selectivity, and selection differentials ##
##                                                                      ##
##======================================================================##
pkgs<-c("tidyverse","dplyr","readxl","ggplot2","ggridges","ggsidekick")
if(length(setdiff(pkgs,rownames(installed.packages())))>0) { install.packages(setdiff(pkgs,rownames(installed.packages())),dependencies=T) }
invisible(lapply(pkgs,library,character.only=T))
homeDir<-here::here()
setwd(homeDir)

##======================================================================##
##============================================================## load data
##======================================================================##
setwd(paste0(homeDir,"/output/"))
data<-read.csv("data_used_by_run_year_all.csv")[,-1] 
stocks<-c("Igushik","Wood","Nushagak","Kvichak","Naknek","Egegik","Ugashik")
nS<-length(stocks)

##===========================================================## escapement
meansize_Y_esc<-read.csv("Mean_size_BB_wide_by_run_year_esc.csv")[,-1] 
meansize_YS_esc<-read.csv("Mean_size_by_system_by_run_year_esc.csv")[,-1] 
SaA_anomaly_Y_esc<-read.csv("SaA_anomaly_BB_wide_by_run_year_esc.csv")[,-1]
SaA_anomaly_YS_esc<-read.csv("SaA_anomaly_by_system_by_run_year_esc.csv")[,-1]

##================================================================## catch
meansize_Y_harv<-read.csv("Mean_size_BB_wide_by_run_year_harv.csv")[,-1]
meansize_YS_harv<-read.csv("Mean_size_by_system_by_run_year_harv.csv")[,-1]
SaA_anomaly_Y_harv<-read.csv("SaA_anomaly_BB_wide_by_run_year_harv.csv")[,-1]
SaA_anomaly_YS_harv<-read.csv("SaA_anomaly_by_system_by_run_year_harv.csv")[,-1]

##=================================================================## run
meansize_Y_all<-read.csv("Mean_size_BB_wide_by_run_year_all.csv")[,-1]
meansize_YS_all<-read.csv("Mean_size_by_system_by_run_year_all.csv")[,-1]
SaA_anomaly_Y_all<-read.csv("SaA_anomaly_BB_wide_by_run_year_all.csv")[,-1]
SaA_anomaly_YS_all<-read.csv("SaA_anomaly_by_system_by_run_year_all.csv")[,-1]
sdsize_Y_all<-read.csv("SD_size_BB_wide_by_run_year_all.csv")[,-1]
sdsize_YS_all<-read.csv("SD_size_by_system_by_run_year_all.csv")[,-1]

##===================================================## total size changes
size_decline_S<-read.csv("SaA_contribution_by_system_by_run_year_all.csv")[,-1] 
size_decline_S<-size_decline_S[order(match(size_decline_S$stock,stocks)),]

##==============================================## selection differentials
## selected mean trait minus population mean trait 
##-----------------------------------------------------------## each river
sel_diffs<-meansize_YS_esc[,-1]-meansize_YS_all[,-1]
sel_diffs[is.na(sel_diffs)]<-0
sel_diffs$year<-meansize_YS_esc[,1]
write.csv(sel_diffs,"Selection_differentials_YS.csv")
ind<-which(names(sel_diffs)=="year")
##-------------------------------## standardized
sel_diffs_std<-(meansize_YS_esc[,-1]-meansize_YS_all[,-1])/sdsize_YS_all[,-1]
sel_diffs_std[is.na(sel_diffs_std)]<-0
sel_diffs_std$year<-meansize_YS_esc[,1]
##-------------------------------------------------------------## bay-wide
sel_diff_BB<-data.frame(sel_diff=meansize_Y_esc[,2]-meansize_Y_all[,2])
sel_diff_BB$year<-meansize_Y_esc[,1]
write.csv(sel_diff_BB,"Selection_differentials_Y.csv")
##-------------------------------## standardized
sel_diff_BB_std<-data.frame(sel_diff=(meansize_Y_esc[,2]-meansize_Y_all[,2])/sdsize_Y_all[,2])
sel_diff_BB_std$year<-meansize_Y_esc[,1]
write.csv(sel_diff_BB_std,"Selection_differentials_Y_std.csv")
##-------------------------------------------------------------## averages
av_sel_diffs<-data.frame(sel_diff=apply(sel_diffs[,-ind],2,mean))
av_sel_diffs$stock<-rownames(av_sel_diffs)
##------------------------------------------------------------## variances
sd_sel_diffs<-data.frame(sd_sel_diff=apply(sel_diffs[,-ind],2,sd))
sd_sel_diffs$stock<-rownames(av_sel_diffs)
##---------------------------------------------## new data frame for plots
df<-merge(av_sel_diffs,sd_sel_diffs,by="stock")
df<-merge(df,size_decline_S,by="stock")

##======================================================================##
##================================================================## plots
##======================================================================##
setwd(paste0(homeDir,"/plots/"))
colors<-c("dodgerblue4","goldenrod1")
cols<-c("#855A27","#C7AC80","#8D9121","#629D9B","#178BC9","#536373","#AE2633") 

##==========================================================## selectivity
## deviation mean size harvested relative to mean size of run
pdf("Selectivity-over-time-by-system.pdf", width=4.5,height=3.5)
par(mar=c(3,3,1,1),mgp=c(1.75,0.5,0),tcl=-0.3,cex.lab=1,cex.axis=0.9)
xx<-meansize_Y_harv[,1] ## bay-wide
yy<-meansize_Y_harv[,2]-meansize_Y_all[,2] ## bay-wide
xlim<-c(min(xx)+1,max(xx)-1)
plot(NA,NA,xlim=xlim,ylim=c(-20,32),xlab="Year",ylab="Selectivity (mm)")
abline(h=0,lty=3,lwd=0.5)
pal<-colorRampPalette(brewer.pal(8,"Set2"))
for(s in 1:nS) { ## each system
x<-meansize_YS_harv[,1]
y<-meansize_YS_harv[,s+1]-meansize_YS_all[,s+1]
lines(x,y,lwd=1,col=cols[s]) 
}
lines(xx,yy,lwd=2) ## bay-wide line
legend("bottomleft",c("bay-wide",paste0(stocks)),pch=c(NA,rep(NA,nS)),lwd=c(2,rep(1,nS)),lty=1,col=c(1,cols),cex=0.5,bty="n",seg.len=1.5)
dev.off()

##====================================## selection differentials over time
## deviation mean size escapement relative to mean size of run
pdf("Selection-differentials-by-system.pdf",width=4.5,height=3.5)
par(mar=c(3,3,1,1),mgp=c(1.75,0.5,0),tcl=-0.3,cex.lab=1,cex.axis=0.9)
ylim<-c(-40,15) ## actual
plot(NA,NA,xlim=xlim,ylim=ylim,xlab="Year",ylab="Selection differential (mm)")
abline(h=0,lty=3,lwd=0.5)
labs<-c("bay-wide",paste0(stocks))
##------------------------------------------------## each system 
for(s in 1:nS) { ## each system 
x<-sel_diffs$year
y<-sel_diffs[,s]  
lines(x,y,lwd=1,col=cols[s]) ## without extreme values?
}
##---------------------------------------------------## bay-wide 
x<-sel_diff_BB$year
y<-sel_diff_BB$sel_diff
lines(x,y,lwd=2) ## bay-wide
legend("bottomleft",labs,pch=c(NA,rep(NA,nS)),lwd=c(2,rep(1,nS)),lty=1,col=c(1,cols),seg.len=1.5,cex=0.5,bty="n")
dev.off()

##====================## mean size catch and escapement relative to return
pdf("Size-catch-vs-esc-rel-to-return-by-system.pdf",width=9,height=6.5)
layout(matrix(c(1:9),ncol=3,nrow=3,byrow=T))
par(mar=c(3,4,1,1),mgp=c(1.75,0.5,0),tcl=-0.3,cex.lab=1.1,cex.axis=0.9)
thi<-0.1 ## thickness of vertical lines 
yrs<-meansize_YS_all[,1]
ylim<-c(-40,30)
ylab<-"Deviation in mean size\n relative to return (mm)"
xlim<-c(min(meansize_YS_all[,1]),max(meansize_YS_all[,1]))
xlab<-"Year"
##------------------------------------------------------------## bay-wide
plot(NA,NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab)
x<-meansize_Y_esc[,1]
y1<-meansize_Y_esc[,2]-meansize_Y_all[,2]
rect(x-thi,0,x+thi,y1,col=colors[1],border=colors[1])
x<-meansize_Y_harv[,1]
y2<-meansize_Y_harv[,2]-meansize_Y_all[,2]
rect(x-thi,0,x+thi,y2,col=colors[2],border=colors[2])
abline(h=0,lwd=1)
legend("topright",c("escapement","catch"),lwd=2,col=colors,cex=0.6,bty="n")
##---------------------------------------------------------## each system
for(s in 1:nS) {
  plot(NA,NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab)
  x<-meansize_YS_esc[,1]
  y1<-meansize_YS_esc[,s+1]-meansize_YS_all[,s+1]
  rect(x-thi,0,x+thi,y1,col=colors[1],border=colors[1])
  x<-meansize_YS_harv[,1]
  y2<-meansize_YS_harv[,s+1]-meansize_YS_all[,s+1]
  rect(x-thi,0,x+thi,y2,col=colors[2],border=colors[2])
  abline(h=0,lwd=1)
  mtext(paste0(stocks[s]),side=3,line=-1.2,cex=0.8)
}
dev.off()

##======================================================================##
##==============================================## plot size distributions
##======================================================================##
# require(splitstackshape) ## to expand to full data
# data_expanded<-data.frame(uncount(data,weights=Count))
# data_expanded<-expandRows(data,"Count",count.is.col=T,drop=T)
##-----------------------------------------------## sample based on counts
nsample<-2e7 ## 20 million or ~1% of data (more results in plotting error)
dat<-slice_sample(data,n=nsample,weight_by=Count,replace=T)
dat<-dplyr::select(dat,year,length,age,stock,type)

##==========================================================## make plots
p<-dat %>%
  # left_join(data.frame(stock=stocks),dat,by="stock") %>%
  # filter(age %in% c("1.2","2.2")) %>%
  # filter(age %in% c("1.3","2.3")) %>%
  ggplot(.,aes(x=length,y=as.factor(year),fill=factor(type),color=factor(type))) +
  geom_density_ridges(alpha=0.5,size=0.25,bandwidth=10) +
  labs(x="Length (mm)",y="Year",title="") +
  facet_grid(~factor(stock,levels=stocks),scales="free_y") + 
  scale_x_continuous(limits=c(390,660),breaks=seq(400,650,50)) +
  coord_cartesian(clip="off") +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  theme_sleek() +
  theme(legend.title=element_blank(),
        legend.position="right",
        panel.border=element_blank(), 
        panel.spacing.x=unit(0,"line"),
        panel.spacing.y=unit(0,"line"),
        axis.text.x=element_text(angle=90),
        axis.text=element_text(size=8),
        axis.title=element_text(size=10)
  ) +
  NULL
ggsave("size-distributions-by-river.pdf",p,"pdf",width=6.8,height=6.8)

p<-dat %>%
  #filter(age %in% c("1.2","2.2")) %>%
  filter(age %in% c("1.3","2.3")) %>%
  ggplot(.,aes(x=length,y=as.factor(year),fill=factor(type),color=factor(type))) +
  geom_density_ridges(alpha=0.5,size=0.25,bandwidth=10) +
  labs(x="Length (mm)",y="Year",title="") +
  scale_x_continuous(limits=c(380,680),breaks=seq(400,650,50)) +
  coord_cartesian(clip="off") +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  theme_sleek() +
  theme(legend.title=element_blank(),
        legend.position="top",
        panel.border=element_blank(), 
        panel.spacing.x=unit(0,"line"),
        panel.spacing.y=unit(0,"line"),
        axis.text.x=element_text(angle=0),
        axis.text=element_text(size=8),
        axis.title=element_text(size=12)
  ) +
  NULL
ggsave("size-distributions-bay-wide-ocean3s.pdf",p,"pdf",width=2.2,height=6.8)

##======================================================================##
##======================================================================##
##======================================================================##