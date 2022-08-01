##=====================================================================##
##                                                                     ##
## Contribution of changes in size-at-age to changes in mean body size ##
##                                                                     ##
##=====================================================================##
pkgs<-c("here","tidyr","dplyr")
if(length(setdiff(pkgs,rownames(installed.packages())))>0) { install.packages(setdiff(pkgs,rownames(installed.packages())),dependencies=T) }
invisible(lapply(pkgs,library,character.only=T))
homeDir<-here::here()
setwd(homeDir)

##============================================================## settings
year.type<-"brood" ## "brood" (contributions) or "run" (covariate model)
nYref<-5 ## number of years to compare in early and late periods
min.per.age<-0.1 ## minimum percent of samples per age and system 

##=====================================================================##
##===========================================================## load data
##=====================================================================##
esc.data<-read.csv("data/ResampledEscapement_fromBroodTable.csv")
harv.data<-read.csv("data/ResampledCatch_fromBroodTable.csv") 
all.data<-data.frame(rbind(esc.data,harv.data))

##===============================================## select data for model
data<-all.data
data<-dplyr::filter(data,!is.na(Length),Length!=0)
data$length<-as.numeric(data$Length)
data$age<-as.character(data$Age)
##============================## drop very rare age groups in each system
ages<-as.character(unique(data$age))
data$AgeSystem<-as.factor(paste(data$age,data$System,sep="_"))
nobs_AS<-data %>% group_by(AgeSystem,age,System) %>% summarize(nobs=length(rep(length,Count))) %>% data.frame
nobs_S<-data %>% group_by(System) %>% summarize(total=length(rep(length,Count))) %>% data.frame
nobs_AS<-merge(nobs_AS,nobs_S,by="System",all=T)
nobs_AS$percent<-round(nobs_AS$nobs*100/nobs_AS$total,4)
keep_AS_cat<-nobs_AS[nobs_AS$percent>=min.per.age,]
data<-data[data$AgeSystem %in% keep_AS_cat$AgeSystem,]
data$age<-as.character(data$age)
ages<-as.character(unique(data$age))
##============================## select brood or run years across systems
data$year<-data$ReturnYear
years<-sort(unique(data$year))
data$Total.Age<-data$Salt.Water.Age+data$Fresh.Water.Age+1 
if(year.type=="brood") {
  data$year<-data$BroodYear
  first_brood_year<-min(years)-min(data$Total.Age)
  last_brood_year<-max(years)-max(data$Total.Age)
  years<-seq(first_brood_year,last_brood_year,1)
  data<-dplyr::filter(data,year %in% years)
}
data<-data[data$System!= "Alagnak",] ## too few escapement ASL samples

##=======================================================## selected data
data$stock<-data$System
write.csv(data,"output/data_used.csv")
data<-dplyr::select(data,year,age,length,Count,stock)
data<-data[order(data$stock,data$year,data$age,data$length),]
nobs<-data %>% summarize(nobs=length(rep(length,Count))) ## observations

##=====================================================================##
##===================================================## summarize BB-wide
##=====================================================================##

##---------------------------------------## mean size at age across years
mean_SaA<-data %>% group_by(age) %>% summarize(meansize=mean(rep(length,Count),na.rm=T)) %>% data.frame()
##---------------------------------------------------## overall mean size
meansize_Y<-data %>% group_by(year) %>% summarize(meansize=mean(rep(length,Count))) 
meansize_Y<-meansize_Y[order(meansize_Y$year),]
write.csv(meansize_Y,file="output/Mean_size_BB_wide.csv")
##--------------------------------------------## mean size-at-age by year
meanSaA_YA<-data %>% group_by(age,year) %>% summarize(meansize=mean(rep(length,Count),na.rm=T)) 
# write.csv(round(meanSaA_YA,2),file="output/mean_SaA_by_year_BB_wide.csv")

##===============================================## size-at-age anomalies
## size-at-age anomalies from age groups mean size across years
##-----------------------------------------------------## anomaly function
SaA_anomaly_BB<-function(length,age) { as.numeric(length)- mean_SaA$meansize[mean_SaA$age==age] }
##-----------------------------## size-at-age anomalies of all individuals
data$SaAanomalyBB<-apply(data,1,function(x) SaA_anomaly_BB(x[names(x)=="length"],x[names(x)=="age"]))
##----------------------------------## average annual size-at-age anomaly
SaA_anomaly_Y_all<-data %>% group_by(year) %>% summarize(SaA_anomaly=mean(rep(SaAanomalyBB,Count))) 
SaA_anomaly_Y_all<-SaA_anomaly_Y_all[order(SaA_anomaly_Y_all$year),]
SaA_anomaly_Y<-SaA_anomaly_Y_all[order(SaA_anomaly_Y_all$year),]
write.csv(SaA_anomaly_Y,file="output/SaA_anomaly_BB_wide.csv")

##======================## size change (mm) due to changes in size-at-age
change_SaA<-NA
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
write.csv(sizetrend,file="output/SaA_contribution_BB_wide.csv")

##=====================================================================##
##=================================================## summarize by system
##=====================================================================##

##===============================================## size-at-age anomalies
## size-at-age anomalies from age groups mean size across years by stock
##-----------------------------------## mean size at age by stock and age
mean_SaA_SA<-data %>% group_by(stock,age) %>% summarize(meansize=mean(rep(length,Count),na.rm=T)) %>% data.frame
##----------------------------------------------------## anomaly function
SaA_anomaly<-function(length,age,stock) { as.numeric(length)-mean_SaA_SA$meansize[mean_SaA_SA$age==age & mean_SaA_SA$stock==as.character(stock)] }
##---------------------------## size-at-age anomalies for all individuals
data$SaAanomaly<-apply(data,1,function(x) SaA_anomaly(x[names(x)=="length"],x[names(x)=="age"],x[names(x)=="stock"]))

##===================================================## overall mean size
meansize_YS<-data %>% group_by(stock,year) %>% summarize(meansize=mean(rep(length,Count),na.rm=T)) 
meansize_YS_wide<-meansize_YS %>% pivot_wider(names_from=stock,values_from=meansize) 
meansize_YS_wide<-meansize_YS_wide[order(meansize_YS_wide$year),]
meansize_YS_wide[meansize_YS_wide==0]<-NA
write.csv(meansize_YS_wide,file="output/Mean_size_by_system.csv")

##====================================================## mean size-at-age
meanSaA_SYA<-data %>% group_by(stock,year,age) %>% summarize(meansize=mean(rep(length,Count),na.rm=T))
meanSaA_SA<-data %>% group_by(stock,age) %>% summarize(meansize=mean(rep(length,Count),na.rm=T))

##======================## average annual size-at-age anomaly across ages
SaA_anomaly_YS<-data %>% group_by(stock,year) %>% summarize(SaA_anomaly=mean(rep(SaAanomaly,Count)))
SaA_anomaly_YS_wide<-SaA_anomaly_YS %>% pivot_wider(names_from=stock,values_from=SaA_anomaly)
SaA_anomaly_YS_wide<-SaA_anomaly_YS_wide[order(SaA_anomaly_YS_wide$year),]
write.csv(SaA_anomaly_YS_wide,file="output/SaA_anomaly_by_system.csv")
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
write.csv(sizetrends,file="output/SaA_contribution_by_system.csv")

##=====================================================================##
##=====================================================================##
##=====================================================================##
##=========================================================## size trends
rivers<-c("Igushik","Wood","Nushagak","Kvichak","Naknek","Egegik","Ugashik")
sizetrends<-sizetrends[order(match(sizetrends$stock,rivers)),]

##==================================================## plot contributions
pdf("output/Size-change-contributions.pdf",width=5,height=4)
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

##=====================================================================##
##=====================================================================##
##=====================================================================##