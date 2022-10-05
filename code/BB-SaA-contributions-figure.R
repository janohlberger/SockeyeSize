##=====================================================================##
##                                                                     ##
## Contribution of changes in size-at-age to changes in mean body size ##
##                                                                     ##
##=====================================================================##
pkgs<-c("here","tidyr","dplyr")
if(length(setdiff(pkgs,rownames(installed.packages())))>0) { install.packages(setdiff(pkgs,rownames(installed.packages())),dependencies=T) }
invisible(lapply(pkgs,library,character.only=T))
homeDir<-here::here()
setwd(paste0(homeDir,"/output/"))

##====================================================## load size trends
sizetrend<-read.csv(file="SaA_contribution_BB_wide_by_brood_year_all.csv")[,-1]
sizetrends<-read.csv(file="SaA_contribution_by_system_by_brood_year_all.csv")[,-1]

##==============================================================## rivers
rivers<-c("Igushik","Wood","Nushagak","Kvichak","Naknek","Egegik","Ugashik")
sizetrends<-sizetrends[order(match(sizetrends$stock,rivers)),]

##==================================================## plot contributions
pdf("plots/Size-change-contributions.pdf",width=5,height=4)
layout(matrix(c(1:2),ncol=2,byrow=T),widths=c(0.25,0.75))
par(mgp=c(2.2,0.5,0),cex.lab=0.9,cex.axis=0.8,tcl=-0.3,las=1)
cols<-c("#C52C2E","#60ADB6","gray")
lnms<-c("mean size","size-at-age","age structure")
ylim<-c(-38.5,14)
##------------------------------------## size-at-age-contribution BB-wide
par(mar=c(4.5,3.5,0.5,0))
plotdata<-dplyr::select(sizetrend,change_size,change_SaA,age_change)
barplot(as.matrix(t(plotdata)),beside=T,xlab="",ylab="Size change (mm)",main="",axisnames=F,col=cols,names.arg=paste0("bay-wide"),las=2,border=NA,ylim=ylim,xlim=c(0.5,4.5))
mtext("bay-wide",side=1,cex=0.85,line=0.5,las=2)
abline(h=0,lty=1,lwd=0.5,col="darkgray");box()
##---------------------------------## size-at-age-contribution by system
par(mar=c(4.5,0,0.5,0.5))
stock_names<-sizetrends$stock
plotdata<-dplyr::select(sizetrends,size_change,SaA_change,age_change) 
barplot(as.matrix(t(plotdata)),beside=T,xlab="",ylab="",main="",axisnames=T,col=cols,names.arg=stock_names,las=2,border=NA,ylim=ylim,yaxt="n")
legend("topleft",lnms,pch=15,col=cols,cex=0.8,pt.cex=1,bty="n")
abline(h=0,lty=1,lwd=0.5,col="darkgray");box()
dev.off()
