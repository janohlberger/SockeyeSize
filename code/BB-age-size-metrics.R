##=====================================================================##
##                                                                     ##
## Time series of size and age metrics for Bristol Bay sockeye salmon  ##
##                                                                     ##
##=====================================================================##
pkgs<-c("here","tidyr","dplyr")
if(length(setdiff(pkgs,rownames(installed.packages())))>0) { install.packages(setdiff(pkgs,rownames(installed.packages())),dependencies=T) }
invisible(lapply(pkgs,library,character.only=T))
homeDir<-here::here()
setwd(homeDir)

##============================================================## settings
nYref<-5 ## number of years to compare in early and late periods
min.per.age<-0.1 ## minimum percent of samples per age and system 

##===========================================================## load data
esc.data<-read.csv("data/ResampledEscapement_fromBroodTable.csv")
esc.data$type<-"escapement"
harv.data<-read.csv("data/ResampledCatch_fromBroodTable.csv") 
harv.data$type<-"catch"
all.data<-data.frame(rbind(esc.data,harv.data))
data.list<-list(esc.data,harv.data,all.data)
ds<-c("esc","harv","all")

##==================================================## brood and run year
ts<-c("brood","run") ## 'brood' (contributions) | 'run' (covariate model)
for(i in 1:2) {
year.type<-ts[i]

##==================================## escapement, harvest, and entire run
if(year.type=="run") {
for(j in 1:3) {
data<-data.list[[j]]
  
##===============================================## select data for model
data<-dplyr::filter(data,!is.na(Length),Length!=0)
data$length<-as.numeric(data$Length)
data$age<-as.character(data$Age)
##----------------------------## drop very rare age groups in each system
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
##----------------------------## select brood or run years across systems
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
data<-data[data$System!="Alagnak",] ## too few samples
##-------------------------------------------------------## selected data
data$stock<-data$System
data<-dplyr::select(data,year,age,length,Count,stock,type)
data<-data[order(data$stock,data$year,data$age,data$length),]
write.csv(data,paste0("output/data_used_by_",ts[i],"_year_",ds[j],".csv"))

##=====================================================================##
##===================================================## summarize BB-wide
##=====================================================================##

##---------------------------------------## mean size at age across years
mean_SaA<-data %>% 
  group_by(age) %>% 
  summarize(meansize=mean(rep(length,Count),na.rm=T)) %>% 
  data.frame()
##---------------------------------------------------## overall mean size
meansize_Y<-data %>% 
  group_by(year) %>% 
  summarize(meansize=mean(rep(length,Count))) %>%
  arrange(year)
filepath<-paste0("Mean_size_BB_wide_by_",ts[i],"_year_",ds[j],".csv")
write.csv(meansize_Y,file=paste0("output/",filepath))
##--------------------------------------------------## overall sd in size
sdsize_Y<-data %>% 
  group_by(year) %>% 
  summarize(sddize=sd(rep(length,Count))) %>%
  arrange(year)
filepath<-paste0("SD_size_BB_wide_by_",ts[i],"_year_",ds[j],".csv")
write.csv(sdsize_Y,file=paste0("output/",filepath))
##--------------------------------------------------## overall sd in size
sdsize_YA<-data %>% 
  group_by(year,age) %>% 
  summarize(sdsize=sd(rep(length,Count))) %>%
  arrange(year,age)
filepath<-paste0("SD_size_BB_wide_each_age_by_",ts[i],"_year_",ds[j],".csv")
write.csv(sdsize_YA,file=paste0("output/",filepath))
##--------------------------------------------## mean size-at-age by year
meanSaA_YA<-data %>% 
  group_by(age,year) %>% 
  summarize(meansize=mean(rep(length,Count),na.rm=T)) 
filepath<-paste0("mean_SaA_BB_wide_by_",ts[i],"_year_",ds[j],".csv")
write.csv(meanSaA_YA,file=paste0("output/",filepath))

##===============================================## size-at-age anomalies
## size-at-age anomalies from age groups mean size across years
##-----------------------------------------------------## anomaly function
SaA_anomaly_BB<-function(length,age) { as.numeric(length)- mean_SaA$meansize[mean_SaA$age==age] }
##-----------------------------## size-at-age anomalies of all individuals
data$SaAanomalyBB<-apply(data,1,function(x) SaA_anomaly_BB(x[names(x)=="length"],x[names(x)=="age"]))
##----------------------------------## average annual size-at-age anomaly
SaA_anomaly_Y<-data %>% 
  group_by(year) %>% 
  summarize(SaA_anomaly=mean(rep(SaAanomalyBB,Count))) %>%
  arrange(year)
filepath<-paste0("SaA_anomaly_BB_wide_by_",ts[i],"_year_",ds[j],".csv")
write.csv(SaA_anomaly_Y,file=paste0("output/",filepath))

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

##=========================================================## size trends
sizetrend<-data.frame(cbind(change_size,change_SaA))
sizetrend$age_change<-sizetrend$change_size-sizetrend$change_SaA
filepath<-paste0("SaA_contribution_BB_wide_by_",ts[i],"_year_",ds[j],".csv")
write.csv(sizetrend,file=paste0("output/",filepath))

##=====================================================================##
##=================================================## summarize by system
##=====================================================================##

##===============================================## size-at-age anomalies
## size-at-age anomalies from age groups mean size across years by stock
##-----------------------------------## mean size at age by stock and age
mean_SaA_SA<-data %>% 
  group_by(stock,age) %>% 
  summarize(meansize=mean(rep(length,Count),na.rm=T)) %>% 
  data.frame()
##----------------------------------------------------## anomaly function
SaA_anomaly<-function(length,age,stock) { as.numeric(length)-mean_SaA_SA$meansize[mean_SaA_SA$age==age & mean_SaA_SA$stock==as.character(stock)] }
##---------------------------## size-at-age anomalies for all individuals
data$SaAanomaly<-apply(data,1,function(x) SaA_anomaly(x[names(x)=="length"],x[names(x)=="age"],x[names(x)=="stock"]))

##===================================================## overall mean size
meansize_YS<-data %>% 
  group_by(stock,year) %>% 
  summarize(meansize=mean(rep(length,Count),na.rm=T)) 
meansize_YS_wide<-meansize_YS %>% 
  pivot_wider(names_from=stock,values_from=meansize) %>%
  arrange(year)
meansize_YS_wide[meansize_YS_wide==0]<-NA
filepath<-paste0("Mean_size_by_system_by_",ts[i],"_year_",ds[j],".csv")
write.csv(meansize_YS_wide,file=paste0("output/",filepath))

##=============================================================## sd size
sdsize_YS<-data %>% 
  group_by(stock,year) %>% 
  summarize(sdsize=sd(rep(length,Count),na.rm=T))  %>% 
  pivot_wider(names_from=stock,values_from=sdsize) %>%
  arrange(year)
sdsize_YS[sdsize_YS==0]<-NA
filepath<-paste0("SD_size_by_system_by_",ts[i],"_year_",ds[j],".csv")
write.csv(sdsize_YS,file=paste0("output/",filepath))

##====================================================## mean size-at-age
meanSaA_SYA<-data %>% 
  group_by(stock,year,age) %>% 
  summarize(meansize=mean(rep(length,Count),na.rm=T))
meanSaA_SA<-data %>% 
  group_by(stock,age) %>% 
  summarize(meansize=mean(rep(length,Count),na.rm=T))

##======================## average annual size-at-age anomaly across ages
SaA_anomaly_YS<-data %>% 
  group_by(stock,year) %>% 
  summarize(SaA_anomaly=mean(rep(SaAanomaly,Count)))
SaA_anomaly_YS_wide<-SaA_anomaly_YS %>% 
  pivot_wider(names_from=stock,values_from=SaA_anomaly) %>%
  arrange(year)
filepath<-paste0("SaA_anomaly_by_system_by_",ts[i],"_year_",ds[j],".csv")
write.csv(SaA_anomaly_YS_wide,file=paste0("output/",filepath))
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

##=========================================================## size trends
sizetrends<-merge(change_size,change_SaA,by="stock")
sizetrends$age_change<-sizetrends$size_change-sizetrends$SaA_change
filepath<-paste0("SaA_contribution_by_system_by_",ts[i],"_year_",ds[j],".csv")
write.csv(sizetrends,file=paste0("output/",filepath))

} ## end j-loop
} ## end if statement
} ## end i-loop
