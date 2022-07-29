##======================================================================##
##                                                                      ##
##  Run size of Bristol Bay sockeye  salmon in each major river system  ##
##                                                                      ##
##======================================================================##
# rm(list=ls(all=T)) 
pkgs<-c("tidyverse","dplyr","readxl")
if(length(setdiff(pkgs,rownames(installed.packages())))>0) { install.packages(setdiff(pkgs,rownames(installed.packages())),dependencies=T) }
invisible(lapply(pkgs,library,character.only=T))
if(exists("homeDir")) { } else { homeDir<-setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) }
setwd(file.path(paste0(homeDir)))

##================================================================## data
returns<-read_excel("/Users/janohlberger/Documents/Work/UW/SalmonSizeChange/DataBB/Brood-Return-Tables/2020_BBay_Return_Table_Summary.xlsx")
returns<-data.frame(round(returns)) ## rounded to individuals
returns[,-1]<-returns[,-1]*1e-6 ## converted to millions
BBreturns<-dplyr::select(returns,year,total_return=Total)
BBreturns$total_return<-BBreturns$total_return
returns<-dplyr::select(returns,-Total)

##=================================================================## plot
add_prop<-TRUE
if(add_prop) { 
pdf("observed/BristolBay-RunSize-BySystem.pdf",width=5,height=6.5)
layout(matrix(c(1:2),nrow=2,byrow=F)) 
} else {
pdf("observed/BristolBay-RunSize-BySystem.pdf",width=5,height=3.5)
}
par(mar=c(3,3,0.5,5),mgp=c(1.5,0.25,0),tcl=-0.3,cex.lab=1,cex.axis=0.8,xaxs="i",yaxs="i")
rivers<-c("Igushik","Wood","Nushagak","Kvichak","Alagnak","Naknek","Egegik","Ugashik") 
river_cols<-c("gray50","#855A27","#C7AC80","#8D9121","#629D9B","gray85","#178BC9","#536373","#AE2633")
allrivers<-c("Togiak","Igushik","Wood","Nushagak","Kvichak","Alagnak","Naknek","Egegik","Ugashik")
return<-dplyr::select(returns,year,all_of(rivers),Alagnak,Togiak)
years<-return[,1];nY<-length(years)
allsystems<-names(return[,-1]);nS<-length(allsystems)
##-----------------------------------------------------------## abundances
plot(NA,NA,xlim=c(min(years),max(years)),ylim=c(0,72),xlab="Year",ylab="Abundance (millions)")
return<-dplyr::select(returns,year,all_of(allrivers))
poly.x<-c(years,rev(years))
for(s in 1:nS){ ## loop stocks
if(s==1) {
run_added<-return[,s+1]
poly.y<-c(run_added,rep(0,nY))
}
if(s!=1) {
run_prev<-run_added
run_added<-run_prev+return[,s+1]
poly.y<-c(run_added,rev(run_prev))
}
polygon(poly.x,poly.y,lwd=0.5,col=river_cols[s],border=river_cols[s])
}
box()
legend("topright",rev(names(return)[-1]),pch=15,pt.cex=1,cex=0.7,adj=0,bty="n",col=rev(river_cols),xpd=T,inset=c(-0.25,-0.01),y.intersp=1.1)
##----------------------------------------------------------## proportions
if(add_prop) { 
prop_return<-prop.table(as.matrix(return[,-1]),1)
plot(NA,NA,xlim=c(min(years),max(years)),ylim=c(0,1),xlab="Year",ylab="Proportion of run")
for(s in 1:nS){ ## loop stocks
if(s==1) {
prop_added<-prop_return[,s]
poly.y<-c(prop_added,rep(0,nY))
}
if(s!=1) {
run_prev<-prop_added
prop_added<-run_prev+prop_return[,s]
poly.y<-c(prop_added,rev(run_prev))
}
polygon(poly.x,poly.y,lwd=0.5,col=river_cols[s],border=river_cols[s])
}
box()
# legend("bottomright",rev(names(return)[-1]),pch=15,pt.cex=1,cex=0.7,adj=0,bty="n",col=rev(river_cols),xpd=T,inset=c(-0.25,-0.05),y.intersp=2.1)
} 
##-----------------------------------------------------------## save plot
dev.off()

##======================================================================##
##======================================================================##
##======================================================================##