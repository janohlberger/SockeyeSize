##=====================================================================##
##                                                                     ##
##  Contribution of size-at-age changes to mean size changes over time ##
##                                                                     ##
##=====================================================================##
pkgs<-c("here","dplyr","tidyr")
if(length(setdiff(pkgs,rownames(installed.packages())))>0) { install.packages(setdiff(pkgs,rownames(installed.packages())),dependencies=T) }
invisible(lapply(pkgs,library,character.only=T))
homeDir<-here::here()
setwd(homeDir)

##============================================================## settings
nYref<-5 ## number of years to compare in early and late periods
highres<-TRUE ## if TRUE use high temporal resolution

##==================================================## age-length dataset
data<-read.csv("output/data_used.csv")[,-1]
selected_data<-data
stocks<-unique(data$stock);nS<-length(stocks)
ages<-unique(data$age);nA<-length(ages)
years<-unique(data$year);nY<-length(years)

##=====================================================================##
##============================================================## analyses
##=====================================================================##
out_by_system_list<-out_BB_wide_list<-list()
spatial_extents<-c("BB_wide","by_system")
nse<-length(spatial_extents)
if(highres) { final_years<-seq(min(years)+nYref-1,max(years),1) } else { final_years<-round(seq(min(years)+nYref-1,max(years),length=5)) }
nfy<-length(final_years)

##=========================================## final year of analysis loop
for(k in 1:nfy){
final_year_of_analysis<-final_years[k]
print(paste0("final year = ",final_year_of_analysis))
data<-dplyr::filter(selected_data,year<=final_year_of_analysis)

##=====================================================================##
##===================================================## summarize BB-wide
##=====================================================================##
## averages across stocks/systems by year
##---------------------------------------------------## overall mean size
meansize_Y<-data %>% group_by(year) %>% summarize(meansize=mean(rep(length,Count))) 
meansize_Y<-meansize_Y[order(meansize_Y$year),]
##---------------------------------------## mean size at age across years
mean_SaA<-data %>% group_by(age) %>% summarize(meansize=mean(rep(length,Count),na.rm=T)) %>% data.frame()
##-----------------------------------------------------## anomaly function
SaA_anomaly_BB<-function(length,age) { as.numeric(length)- mean_SaA$meansize[mean_SaA$age==age] }
##---------------------------## size-at-age anomaries for all individuals
data$SaAanomalyBB<-apply(data,1,function(x) SaA_anomaly_BB(x[names(x)=="length"],x[names(x)=="age"]))
##----------------------------------## average annual size-at-age anomaly
SaA_anomaly_Y_all<-data %>% group_by(year) %>% summarize(SaA_anomaly=mean(rep(SaAanomalyBB,Count))) 
SaA_anomaly_Y_all<-SaA_anomaly_Y_all[order(SaA_anomaly_Y_all$year),]

##======================## size change (mm) due to changes in size-at-age
change_SaA<-NA
SaA_anomaly_Y<-SaA_anomaly_Y_all[order(SaA_anomaly_Y_all$year),]
nYall<-dim(SaA_anomaly_Y)[1]
##-----------------------------------## change early versus late period
early<-mean(SaA_anomaly_Y$SaA_anomaly[1:nYref])
late<-mean(SaA_anomaly_Y$SaA_anomaly[(nYall-nYref+1):nYall])
change_SaA<-round(late-early,2)

##============================================## overall size change (mm)
change_size_BB<-NA
nYall<-dim(meansize_Y)[1]
##-----------------------------------## change early versus late period
early<-mean(meansize_Y$meansize[1:nYref])
late<-mean(meansize_Y$meansize[(nYall-nYref+1):nYall])
change_size<-round(late-early,2)

##==========================================================## sizetrends
sizetrend<-data.frame(cbind(change_size,change_SaA))
sizetrend$age_change<-sizetrend$change_size-sizetrend$change_SaA
out_BB_wide_list[[k]]<-sizetrend
if(k==nfy) out_BB_wide<-sizetrend 

##=====================================================================##
##=================================================## summarize by system
##=====================================================================##

##===================================================## overall mean size
meansize_YS<-data %>% group_by(stock,year) %>% summarize(meansize=mean(rep(length,Count),na.rm=T)) 
meansize_YS_wide<-meansize_YS %>% pivot_wider(names_from=stock,values_from=meansize) 
meansize_YS_wide<-meansize_YS_wide[order(meansize_YS_wide$year),]
meansize_YS_wide[meansize_YS_wide==0]<-NA

##====================================================## mean size-at-age
meanSaA_SYA<-data %>% group_by(stock,year,age) %>% summarize(meansize=mean(rep(length,Count),na.rm=T))
meanSaA_SA<-data %>% group_by(stock,age) %>% summarize(meansize=mean(rep(length,Count),na.rm=T))

##==================================================## size-at-age anomaly
##-----------------------------------## mean size at age by stock and age
mean_SaA_SA<-data %>% group_by(stock,age) %>% summarize(meansize=mean(rep(length,Count),na.rm=T)) # %>% data.frame
##------------------------------------------------------------## function
SaA_anomaly<-function(length,age,stock) { as.numeric(length)-mean_SaA_SA$meansize[mean_SaA_SA$age==age & mean_SaA_SA$stock==as.character(stock)] }
##---------------------------## size-at-age anomalies for all individuals
data$SaAanomaly<-apply(data,1,function(x) SaA_anomaly(x[names(x)=="length"],x[names(x)=="age"],x[names(x)=="stock"]))

##======================## average annual size-at-age anomaly across ages
SaA_anomaly_YS<-data %>% group_by(stock,year) %>% summarize(SaA_anomaly=mean(rep(SaAanomaly,Count)))
SaA_anomaly_YS_wide<-SaA_anomaly_YS %>% pivot_wider(names_from=stock,values_from=SaA_anomaly)
SaA_anomaly_YS_wide<-SaA_anomaly_YS_wide[order(SaA_anomaly_YS_wide$year),]
stocks<-names(SaA_anomaly_YS_wide)[-1]

##======================## size change (mm) due to changes in size-at-age
change_SaA<-data.frame(stock=stocks,SaA_change=NA)
for(s in 1:length(stocks)){
SaA_anomaly_Y<-SaA_anomaly_YS[SaA_anomaly_YS$stock==stocks[s],] 
SaA_anomaly_Y<-SaA_anomaly_Y[order(SaA_anomaly_Y$year),]
nYstock<-dim(SaA_anomaly_Y)[1]
##-----------------------------------## change early versus late period
early<-mean(SaA_anomaly_Y$SaA_anomaly[1:nYref])
late<-mean(SaA_anomaly_Y$SaA_anomaly[(nYstock-nYref+1):nYstock])
change_SaA[s,2]<-round(late-early,2)
} ## end loop over stocks

##============================================## overall size change (mm)
change_size<-data.frame(stock=stocks,size_change=NA)
for(s in 1:length(stocks)){
meansize_Y_stock<-meansize_YS[meansize_YS$stock==stocks[s],] 
meansize_Y_stock<-meansize_Y_stock[order(meansize_Y_stock$year),]
nYstock<-dim(meansize_Y_stock)[1]
##-----------------------------------## change early versus late period
early<-mean(meansize_Y_stock$meansize[1:nYref])
late<-mean(meansize_Y_stock$meansize[(nYstock-nYref+1):nYstock])
change_size[s,2]<-round(late-early,2)
} ## end loop over stocks

##==========================================================## sizetrends
sizetrends<-merge(change_size,change_SaA,by="stock")
sizetrends$age_change<-sizetrends$size_change-sizetrends$SaA_change
out_by_system_list[[k]]<-sizetrends
if(k==nfy) out_by_system<-sizetrends 

##=========================## end k-loop through final years of analysis
}

##================================## save list with different final years
save(out_by_system_list,file="output/SaA_contribution_BB_wide_list.RData")
save(out_BB_wide_list,file="output/SaA_contribution_by_system_list.RData")

##===================================## function to convert list to array
list_to_array<-function(list_in){
dims12<-dim(list_in[[1]])
dim3<-length(list_in)
array_out<-array(unlist(list_in),dim=c(dims12,dim3))
colnames(array_out)<-colnames(list_in[[1]])
rownames(array_out)<-rownames(list_in[[1]])
return(array_out) ## output as a 3-D array
}

##===============================## save array with different final years
out_by_system_over_time<-list_to_array(out_by_system_list)
out_BB_wide_over_time<-list_to_array(out_BB_wide_list)

##=====================================================================##
##===========================## plot size-at-age contributions over time
##=====================================================================##

##===================================## recruitment (return by brood year)
recruits<-read_excel("data/2020_BBay_Brood_Tables_Recruitment.xlsx")
recruits<-data.frame(round(recruits)) ## round to number of individuals
BBrecruits<-dplyr::select(recruits,year=Brood.Year)
BBrecruits$total_recruits<-rowSums(recruits[,-1])*1e-6 ## in millions

##===============================================================## plot
pdf("output/Size-at-age-contribution-over-time.pdf",width=8,height=6)
layout(matrix(c(2,1,1,1,3,1,1,1,4,1,1,1,5,6,7,8),ncol=4,nrow=4,byrow=T))
par(mar=c(2,2,0,0),oma=c(2,2,1,1),mgp=c(2,0.5,0),tcl=-0.3)
lnms<-c("mean size","size-at-age")#,"age structure")
rec.lwd<-1;rec.lty<-2
cols<-c("#C52C2E","#60ADB6")
##-------------------------------------------------------------## bay-wide
par(mar=c(2,2,0,3))
BB_wide<-data.frame(out_BB_wide_over_time[1,,])
names(BB_wide)<-final_years
size_changes<-as.numeric(BB_wide[rownames(BB_wide)=="change_size",])
SaA_changes<-as.numeric(BB_wide[rownames(BB_wide)=="change_SaA",])
xlim<-c(min(final_years),max(final_years))
ylim<-c(min(c(size_changes,SaA_changes)),max(c(size_changes,SaA_changes)))
# ylim<-c(-20,20)
xlab<-"Last brood year of analysis"
ylab<-"Size change (mm)"
plot(NA,NA,xaxt="n",yaxt="n",xlim=xlim,ylim=ylim,xlab="",ylab="")
abline(h=0,lty=1,lwd=0.25)
axis(1,at=seq(1960,2020,10),label=T,cex.axis=1.2)
axis(2,at=seq(-20,20,5),label=T,cex.axis=1.2)
poly.x<-c(final_years,rev(final_years))
poly.y<-c(size_changes,rev(SaA_changes))
polygon(poly.x,poly.y,lwd=0.5,col="gray90",border="white")
lines(final_years,size_changes,lwd=3,col=cols[1])
lines(final_years,SaA_changes,lwd=3,col=cols[2])
mtext("bay-wide ",side=3,line=-1.2,cex=0.8,font=2,adj=1)
##--------------------------------------------------------## add legends
legend("topleft",lnms,lwd=2.5,col=cols,cex=1,bty="n",seg.len=1.5)
##----------------------------------------------------## add recruitment
lname<-"Brood year return (5-year moving average)"
legend("bottomleft",lname,lwd=rec.lwd,lty=rec.lty,cex=1,bty="n")
par(new=TRUE)
x<-BBrecruits$year
plot(NA,NA,xlim=c(min(x),max(x)),ylim=c(0,70),type="l",axes=F,xaxt="n",yaxt="n",xlab="",ylab="")
axis(4,at=seq(0,70,10),labels=TRUE)
mname<-"Abundance (millions)"
mtext(mname,side=4,line=2.2,cex=1,outer=F)
y<-BBrecruits$total_recruits
lines(x[-c(1:4)],movavg(y,5,"s")[-c(1:4)],lwd=rec.lwd,lty=rec.lty)
##--------------------------------------------------------## each system
par(mar=c(2,2,0,0))
rivers<-c("Igushik","Wood","Nushagak","Kvichak","Naknek","Egegik","Ugashik")
stock_names<-data.frame(out_by_system_over_time[,,1])$stock
nstocks<-length(stock_names)
for(s in 1:nstocks){
  river<-rivers[s]
  index<-which(stock_names==river)
  size_changes<-as.numeric(out_by_system_over_time[index,2,])
  SaA_changes<-as.numeric(out_by_system_over_time[index,3,])
  ylim<-c(min(c(size_changes,SaA_changes)),max(c(size_changes,SaA_changes)))
  plot(NA,NA,xaxt="n",yaxt="n",xlim=xlim,ylim=ylim,xlab="",ylab="")
  abline(h=0,lty=1,lwd=0.25)
  axis(1,at=seq(1960,2020,10),label=T,cex.axis=0.8)
  axis(2,at=seq(-20,20,10),label=T,cex.axis=0.8)
  poly.x<-c(final_years,rev(final_years))
  poly.y<-c(size_changes,rev(SaA_changes))
  polygon(poly.x,poly.y,lwd=0.5,col="gray90",border="white")
  lines(final_years,size_changes,lwd=2,col=cols[1])
  lines(final_years,SaA_changes,lwd=2,col=cols[2])
  mtext(paste0(river," "),side=3,line=-1,cex=0.6,font=2,adj=1)
  ##---------------------------------------------## end loop over systmes
} 
##-----------------------------------------------------------## save plot
mtext(xlab,side=1,line=0.5,cex=1,outer=T)
mtext(ylab,side=2,line=0.5,cex=1,outer=T)
dev.off()

##=====================================================================##
##=====================================================================##
##=====================================================================##