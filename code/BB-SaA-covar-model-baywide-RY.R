##======================================================================##
##                                                                      ##
##  Model of changes in size-at-age of Bristol Bay sockeye by run year  ##
##                                                                      ##
##======================================================================##
pkgs<-c("here","tidyr","dplyr","readxl","pracma","MuMIn","Hmisc","stringr", "relaimpo","visreg","RColorBrewer","ggplot2","viridis","nlme")
if(length(setdiff(pkgs,rownames(installed.packages())))>0) { install.packages(setdiff(pkgs,rownames(installed.packages())),dependencies=T) }
invisible(lapply(pkgs,library,character.only=T))
homeDir<-here::here()
setwd(homeDir)

##======================================================================##
##======================================================================##
##============================================================## load data
##======================================================================##
##======================================================================##

##======================================================================##
##===================================## average annual size-at-age anomaly
##======================================================================##
SaA_anomaly_Y<-read.csv("output/SaA_anomaly_BB_wide.csv")[,-1]

##======================================================================##
##===================================================## BB sockeye returns
##======================================================================##
returns<-read_excel("data/2020_BBay_Return_Table_Summary.xlsx")
returns<-data.frame(round(returns)) ## round to number of individuals
BBreturns<-dplyr::select(returns,year,total_return=Total)
BBreturns$total_return<-BBreturns$total_return*1e-6 ## millions
returns<-dplyr::select(returns,-Total)

##======================================================================##
##=============================## North Pacific pink/chum salmon abundance
##======================================================================##
## Ruggerone, Irvine, Connors (2021, NPAFC) for 2016-2020 (millions)
## Ruggerone and Irvine (2018) for 1952-2015 (millions)
##==========================================================## chum salmon
chum_salmon<-data.frame(read_excel("data/Chum-Salmon-Total-Abundance.xlsx"))
chum_salmon<-dplyr::select(chum_salmon,year=Year,chum_tot_num=Total)
##-----------------------------------------------------## add recent years
add<-data.frame(year=seq(2016,2020,1),chum_tot_num=c(150,123,140,111,92))
chum_salmon<-data.frame(rbind(chum_salmon,add))
##==========================================================## pink salmon
pink_salmon<-data.frame(read_excel("data/Pink-Salmon-Total-Abundance.xlsx"))
pink_salmon<-dplyr::select(pink_salmon,year=Year,pink_tot_num=Total)
##-----------------------------------------------------## add recent years
add<-data.frame(year=seq(2016,2020,1),pink_tot_num=c(432,513,701,639,315))
pink_salmon<-data.frame(rbind(pink_salmon,add))

##======================================================================##
##==================================================================## SST
##======================================================================##
## Temperature data from the NOAA Earth System Research Laboratory at
## https://www.esrl.noaa.gov/psd/cgi-bin/data/timeseries/timeseries1.pl
##==============================================================## seasons
winter_months<-c("Jan","Feb","Mar")
summer_months<-c("Jul","Aug","Sep")
##==========================================================## Bristol Bay
SST_BB<-data.frame(read_excel("data/SST-BristolBay.xlsx"))
SST_BB$BBsumT<-rowMeans(SST_BB[,names(SST_BB) %in% summer_months])
SST_BB<-dplyr::select(SST_BB,year=Year,BBsumT) 
##===========================================================## Bering Sea
SST_BS<-data.frame(read_excel("data/SST-BeringSea.xlsx"))
SST_BS$BSsumT<-rowMeans(SST_BS[,names(SST_BS) %in% summer_months])
SST_BS<-dplyr::select(SST_BS,year=Year,BSsumT)
##==============================================## around Aleutian Islands
SST_AI<-data.frame(read_excel("data/SST-AleutianIslands.xlsx"))
SST_AI$AIwinT<-rowMeans(SST_AI[,names(SST_AI) %in% winter_months])
SST_AI$AIsumT<-rowMeans(SST_AI[,names(SST_AI) %in% summer_months])
SST_AI<-dplyr::select(SST_AI,year=Year,AIwinT,AIsumT)
##========================================## north-western Gulf of Alaska
SST_GoA<-data.frame(read_excel("data/SST-GulfOfAlaska.xlsx"))
SST_GoA$GoAwinT<-rowMeans(SST_GoA[,names(SST_GoA) %in% winter_months])
SST_GoA<-dplyr::select(SST_GoA,year=Year,GoAwinT) 

##======================================================================##
##====================================================## lake temperatures
##======================================================================##
## average summer temperatures in Lake Aleknagik of the Wood river system
## average across all stations from 0-20m depth from June 15th - Sept 5th
LakeTemp<-data.frame(read_excel("data/Aleknagik-Lake-Temp-1963-2020.xlsx"))
LakeTemp<-dplyr::select(LakeTemp,year=Year,LakeTemp=MeanLakeTemp) 

##======================================================================##
##==============================================## save raw covariate data
##======================================================================##
cov_list<-list(BBreturns,SST_BB,SST_GoA,SST_AI,SST_BS,LakeTemp,pink_salmon,chum_salmon)
covariates<-Reduce(function(...) merge(...,by="year",all=T),cov_list)
covariates<-covariates[covariates$year>=1960,]
write.csv(round(covariates,4),file="data/covariate_data.csv")

##======================================================================##
##=================================================## model covariate data
##======================================================================##
## 'lag' are relative to run year, e.g. -1 means year prior to return
## 'nw' are backward window lengths for moving averages
##==================================================================## SST
SST_list<-list(SST_BB,SST_GoA,SST_AI,SST_BS)
SSTdata<-Reduce(function(...) merge(...,by="year",all=T),SST_list)
nr<-dim(SSTdata)[1];nc<-dim(SSTdata)[2]
##----------------------------------------------------------------## lag-1
SSTdata_lag1<-data.frame(rbind(rep(NA,nc-1),SSTdata[-nr,-1]))
names(SSTdata_lag1)<-paste0(names(SSTdata)[-1],"_lag1")
SST_data<-data.frame(cbind(SSTdata,SSTdata_lag1))
##----------------------------------------------------------------## lag-2
SSTdata_lag2<-data.frame(rbind(rep(NA,nc-1),rep(NA,nc-1),SSTdata[-c(nr-1,nr),-1]))
names(SSTdata_lag2)<-paste0(names(SSTdata)[-1],"_lag2")
SST_data<-data.frame(cbind(SST_data,SSTdata_lag2))
##----------------------------------------------------------------## lag-3
SSTdata_lag3<-data.frame(rbind(rep(NA,nc-1),rep(NA,nc-1),rep(NA,nc-1), SSTdata[-c(nr-2,nr-1,nr),-1]))
names(SSTdata_lag3)<-paste0(names(SSTdata)[-1],"_lag3")
SST_data<-data.frame(cbind(SST_data,SSTdata_lag3))
##-----------------------------------------------## 3-year moving averages
## averages including previous two and return year winter
SST_data$AIwinT_movav3<-movavg(SST_data$AIwinT,n=3,type="s")
SST_data$GoAwinT_movav3<-movavg(SST_data$GoAwinT,n=3,type="s")
##-----------------------------------------------## 2-year moving averages
## averages including previous and return year winter
SST_data$AIwinT_movav2<-movavg(SST_data$AIwinT,n=2,type="s")
SST_data$GoAwinT_movav2<-movavg(SST_data$GoAwinT,n=2,type="s")
##-----------------------------------------## lag-1 2-year moving averages
## average of previous two summers that most fish spent at sea
SST_data$AIsumT_lag1_movav2<-movavg(SST_data$AIsumT_lag1,n=2,type="s")
SST_data$BSsumT_lag1_movav2<-movavg(SST_data$BSsumT_lag1,n=2,type="s")
##-----------------------------------------## lag-1 2-year moving averages
## average of previous three summers including both 1st summers
SST_data$AIsumT_lag1_movav3<-movavg(SST_data$AIsumT_lag1,n=3,type="s")
SST_data$BSsumT_lag1_movav3<-movavg(SST_data$BSsumT_lag1,n=3,type="s")
##-----------------------------------------## lag-2 2-year moving averages
## average of 1st summer experienced by ocean 2s and 3s
SST_data$AIsumT_lag2_movav2<-movavg(SST_data$AIsumT_lag2,n=2,type="s")
SST_data$BBsumT_lag2_movav2<-movavg(SST_data$BBsumT_lag2,n=2,type="s")
SST_data$BSsumT_lag2_movav2<-movavg(SST_data$BSsumT_lag2,n=2,type="s")

##============================================================## lake temp
nr<-dim(LakeTemp)[1];nc<-dim(LakeTemp)[2]
##----------------------------------------------------------------## lag-3
LakeTemp$LakeTemp_lag3<-c(rep(NA,3),LakeTemp$LakeTemp[-c(nr-2,nr-1,nr)])
##-----------------------------------------## 2-year moving average on lag
LakeTemp$LakeTemp_lag3_movav<-movavg(LakeTemp$LakeTemp_lag3,n=2,type="s")
## mean summer lake temps when most fish reared in lakes

##=====================================================## salmon abundance
SalmonData<-merge(pink_salmon,chum_salmon,by="year",all=T)
SalmonData$comp_tot_num<-SalmonData$pink_tot_num+SalmonData$chum_tot_num
nr<-dim(SalmonData)[1];nc<-dim(SalmonData)[2]
##----------------------------------------------------------------## lag-1
Salmon_data_lag1<-data.frame(rbind(rep(NA,nc-1),SalmonData[-nr,-1]))
names(Salmon_data_lag1)<-paste0(names(SalmonData)[-1],"_lag1")
Salmon_data<-data.frame(cbind(SalmonData,Salmon_data_lag1))
##----------------------------------------------------------------## lag-2
Salmon_data_lag2<-data.frame(rbind(rep(NA,nc-1),rep(NA,nc-1),SalmonData[-c(nr-1,nr),-1]))
names(Salmon_data_lag2)<-paste0(names(SalmonData)[-1],"_lag2")
Salmon_data<-data.frame(cbind(Salmon_data,Salmon_data_lag2))
##------------------------------------------------## 2-year moving average
SalmonData_movav<-data.frame(apply(SalmonData[,-1],2,function(x) movavg(x,n=2,type="s")))
names(SalmonData_movav)<-paste0(names(SalmonData)[-1],"_movav2")
Salmon_data<-data.frame(cbind(Salmon_data,SalmonData_movav))
##--------------------------------------## 2-year moving average on lag-1
SalmonData_lag1_movav<-data.frame(apply(Salmon_data_lag1,2,function(x) movavg(x,n=2,type="s")))
names(SalmonData_lag1_movav)<-paste0(names(SalmonData)[-1],"_lag1_movav2")
Salmon_data<-data.frame(cbind(Salmon_data,SalmonData_lag1_movav))

##======================================================================##
##======================================================================##
##================================## bay-wide model of size-at-age anomaly
##======================================================================##
##======================================================================##
setwd(file.path(paste0(homeDir,"/output")))

##===========================================================## model data
datalist<-list(SaA_anomaly_Y,BBreturns,SST_data,Salmon_data) #,LakeTemp)
alldata<-data<-Reduce(function(...) merge(...,by="year"),datalist)
## lake temperature data start with 1967 return year due to 3-4 year lag
## run model with lake temp and re-run using all years if not included

##================================================## pairwise correlations 
##-------------------------## correlations between covariates and response
test<-apply(data,2,function(x) as.numeric(as.character(x)))
res<-Hmisc::rcorr(test,type="pearson")
out<-res$r ## all pairwise correlations
diag(out)<-NA
cor_test_res<-data.frame(SaA_anomaly=out[,colnames(out)=="SaA_anomaly"])

##----------------------------## correlations among competitor time series
comp_data<-dplyr::select(data,pink_tot_num,chum_tot_num,comp_tot_num) 
test<-apply(comp_data,2,function(x) as.numeric(as.character(x)))
test<-test[ ]
res<-Hmisc::rcorr(test)
out<-res$r ## all pairwise correlations
diag(out)<-NA

##-------------------------## correlations among model-selected time series
selected_ts<-dplyr::select(data,total_return,pink_tot_num_lag1,BSsumT_lag1,AIwinT) 
test<-apply(selected_ts,2,function(x) as.numeric(as.character(x)))
test<-test[ ]
res<-Hmisc::rcorr(test)
out<-res$r ## all pairwise correlations
diag(out)<-NA
round(out,3)

##==============================================## select final covariates
data<-dplyr::select(data,year,SaA_anomaly,total_return,AIwinT,BSsumT_lag1, pink_tot_num_lag1) 

##======================================================## scaling of data
scaleD<-T ## center and scale data to mean=0 and sd=1
data_other<-dplyr::select(data,year,SaA_anomaly) 
data_unscaled<-dplyr::select(data,-year,-SaA_anomaly)
data_means<-sapply(data_unscaled,mean)
data_sds<-sapply(data_unscaled,sd)
if(scaleD==T){ data<-data.frame(cbind(data_other,scale(data_unscaled))) }
data<-data[complete.cases(data),] ## drop years with NAs
nY<-dim(data)[1]

##======================================================================##
##=====================================## size-at-age anomaly linear model
##======================================================================##

##=========================================================## main effects
mod_form<-formula(SaA_anomaly~total_return+I(total_return^2)+AIwinT+I(AIwinT^2)+BSsumT_lag1+I(BSsumT_lag1^2)+pink_tot_num_lag1+I(pink_tot_num_lag1^2))

##=========================================================## interactions
##-------------------------------------------## North Pacific regime shift
data$regime[data$year<=1988]<-"upto1988"
data$regime[data$year>=1989]<-"since1989"
##-----------------------------------------------------------## full model
mod_form<-formula(SaA_anomaly~total_return+I(total_return^2)+AIwinT+I(AIwinT^2)+AIwinT:regime+BSsumT_lag1+I(BSsumT_lag1^2)+BSsumT_lag1:regime+pink_tot_num_lag1+I(pink_tot_num_lag1^2))

##======================================================## model selection
options(na.action="na.fail") 
mod_lm_full<-lm(mod_form,data=data) 
mod_select<-dredge(mod_lm_full,trace=F,rank="AIC")
mod_sel<-get.models(mod_select,subset=1)[[1]]
summary(mod_sel)

##======================================================## quick AIC table
n_top_mods<-17
aic_table<-data.frame(mod_select)
aic_table[1:n_top_mods,]

##============================================================## AIC table
#models_subset<-get.models(mod_select,subset=1:n_top_mods)
models_subset<-get.models(mod_select,subset=delta<2) 
aic_table<-data.frame(mod_select)
aic_table<-aic_table[1:length(models_subset),]
aic_table$cum.weights<-cumsum(aic_table$weight)
num_of_mods<-length(models_subset)
mod_forms<-NA
for(i in 1:num_of_mods) {
  use_mod<-models_subset[[i]]
  form<-as.character(use_mod$call)[2]
  mod_forms[i]<-form
}
mod_forms<-unlist(mod_forms)
mod_forms<-gsub("SaA_anomaly","",mod_forms)
mod_forms<-gsub("total_return","R",mod_forms)
mod_forms<-gsub("pink_tot_num_lag1","P",mod_forms)
mod_forms<-gsub("AIwinT:regime","W:r",mod_forms)
mod_forms<-gsub("BSsumT_lag1:regime","S:r",mod_forms)
mod_forms<-gsub("AIwinT","W",mod_forms)
mod_forms<-gsub("BSsumT_lag1","S",mod_forms)
mod_forms<-gsub("1","",mod_forms)
mod_forms<-gsub(" ","",mod_forms)
mod_forms<-lapply(mod_forms,function(x) substr(x,1,nchar(x)-1))
mod_forms<-unlist(mod_forms)
aic_table$mod_forms<-mod_forms ## add to table
aic_table$rank<-seq(dim(aic_table)[1])
aic_table$AIC<-round(aic_table$AIC,2)
aic_table$delta<-round(aic_table$delta,2)
aic_table_to_save<-dplyr::select(aic_table,rank,mod_forms,AIC,delta)
## exclude models that include quadratic without linear effect of predictor
aic_table_to_save<-aic_table_to_save[-c(8,9,15),] 
aic_table_to_save$rank<-seq(1:dim(aic_table_to_save)[1])
# write.csv(aic_table_to_save,"table_AICs.csv")


##======================================================================##
##=======================================================## selected model
##======================================================================##
mod<-mod_sel
##--------------------------------------------------------## model results
residuals<-residuals(mod,type="response")
fitted<-fitted(mod)
out_mod<-summary(mod) 
##------------------------------------------------------## autocorrelation
as.numeric(pacf(residuals(mod),lag=9,plot=F)$acf) 
##---------------------------------------------------------## VIF analysis
car::vif(mod)
##-------------------------## pairwise correlations of selected covariates
mod_terms<-as.character(names(mod[[1]])[-1])
mod_terms<-mod_terms[!str_detect(mod_terms,"2")]
mod_terms<-unique(mod_terms)
test_data<-data_unscaled[,colnames(data_unscaled) %in% mod_terms]
test<-apply(test_data,2,function(x) as.numeric(as.character(x)))
out<-round(rcorr(test,type="pearson")$r,3)
max(out[out!=1],na.rm=T) 

##===================================================## variable importance
relimp<-calc.relimp(mod,type="lmg") ## % of response
print(relimp)
##------------------------------------------------------## make data frame
relimp_lmg<-relimp$lmg
pinks<-as.numeric(sum(relimp_lmg[str_detect(names(relimp_lmg),"pink")]))
return<-as.numeric(sum(relimp_lmg[str_detect(names(relimp_lmg),"return")]))
winT<-as.numeric(sum(relimp_lmg[str_detect(names(relimp_lmg),"AIwinT")]))
sumT<-as.numeric(sum(relimp_lmg[str_detect(names(relimp_lmg),"BSsumT_lag1")]))
relimp_df<-data.frame(pinks=pinks,winT=winT,return=return,sumT=sumT)
relimp_df<-round(relimp_df*100,1)
relimp_df ## sum of absolute var expl equals r2:
round(sum(relimp_df),2)
round(summary(mod)$r.squared*100,2)

##======================================================================##
##=================================================## partial effects plot
##======================================================================##
add_eff_anom<-FALSE ## add effect anomaly over time?
add_time_series<-TRUE ## add covariate time series plots?
covariates<-covariates[covariates$year>=1962,]
##----------------------------------------------------------## model terms
mod_terms_ordered<-mod_terms[order(mod_terms,decreasing=T)]
##---------------------------------------------------------------## colors
cols<-c("#855A27","#C7AC80","#8D9121","#629D9B","#178BC9", "#536373","#AE2633") 
##-----------------------------------------------------------## start plot
if(add_time_series){ 
pdf("Model-size-at-age-partial-effects.pdf",width=12,height=6)
layout(matrix(c(1:8),nrow=2,byrow=F)) 
} else {
pdf("Model-size-at-age-partial-effects.pdf",width=12,height=3)
layout(matrix(c(1:4),nrow=1,byrow=T)) 
}
if(add_eff_anom){
pdf("Model-size-at-age-partial-effects.pdf",width=12,height=9)
layout(matrix(c(1:12),nrow=3,byrow=F))
}
par(mar=c(4,4,.5,.5),oma=c(0,0,1,.5),mgp=c(2.2,.5,0),tcl=-0.3,cex.lab=1.2)
##--------------------------------------------## for partial effects plots
ylab<-"Partial effect on size-at-age anomaly"
ll<-c(-0.06,-0.02);cexll<-1.2 ## inset and size of letters 
add.line<-TRUE;lwd.ab<-0.25;cexpt<-1.2
yr<-covariates$year
yr<-yr-min(yr)+1
yr<-round(100*yr/max(yr))/100
if(add_eff_anom){ bg.pt<-alpha(1,0.25) } else { bg.pt<-alpha("black",yr) }
# bg.pt<-"darkslategray" ## same color all points
ylim<-c(-23,23)
##---------------------------------------------## for effect anomaly plots
# eff_cols<-c("chocolate3","forestgreen")
eff_cols<-c("snow3","snow4")
eff_cols<-c("slategray","darkslategray")
years<-data$year
no_yrs<-length(years)
lwd.h<-3
ylim_anom<-c(-23,23)
##------------------------------------------------## for time series plots
xx<-covariates$year
xlim<-c(min(xx),max(xx))
colors<-c("dodgerblue3","firebrick") ## SST time series
##=========================================================## total return
covar<-mod_terms_ordered[1];covar<-"total_return"
xtrans<-function(x) { (x*data_sds[names(data_sds)==covar]+data_means[names(data_means)==covar]) }
##------------------------------------------------------## partial effects
newlim<-c(2,69)
p1<-visreg(mod,xvar=covar,xlab="Total return (millions)",partial=T, ylab=ylab,scale="response",points.par=list(cex=cexpt,pch=21,lwd=0.1,bg=bg.pt), fill.par=list(col=alpha(1,0.1)),line.par=list(lwd=2,col=1),xtrans=xtrans, ylim=ylim)#,xlim=newlim)
if(add.line) abline(h=0,lwd=lwd.ab,lty=3,col="darkgray")
legend("topleft","a",text.font=2,cex=cexll,bty="n",inset=ll,xpd=NA)
##---------------------------------------------------## add effect anomaly
if(add_eff_anom){
eff_anom<-p1$res$visregRes-mean(p1$res$visregRes) ## anomaly?
eff_col<-as.character(factor(eff_anom>=0,labels=eff_cols))  
plot(years,eff_anom,type="h",lwd=lwd.h,col=eff_col,lend=1,xlim=xlim,ylim=ylim_anom, xlab="Year",ylab="Anomaly of partial effect") 
abline(h=0,lty=1,lwd=0.5)
}
##------------------------------------------------------## add time series
if(add_time_series){ 
yy<-covariates$total_return ## convert to in millions
plot(xx,yy,type="h",lwd=1.5,col="saddlebrown",xlab="Year",ylab="Total run size (millions)",ylim=newlim,xlim=xlim) ## or use: darkorchid4
legend("topleft","e",text.font=2,cex=cexll,bty="n",inset=ll,xpd=NA)
}
##===========================================## pinks salmon previous year
covar<-mod_terms_ordered[2];covar<-"pink_tot_num_lag1"
xtrans<-function(x) { x*data_sds[names(data_sds)==covar]+data_means[names(data_means)==covar] }
##------------------------------------------------------## partial effects
newlim<-c(110,710)
p2<-visreg(mod,xvar=covar,xlab="Pink salmon abundance (millions)", partial=T, ylab=ylab,scale="response",points.par=list(cex=cexpt,pch=21,lwd=0.1,bg=bg.pt), fill.par=list(col=alpha(1,0.1)),line.par=list(lwd=2,col=1),xtrans=xtrans, ylim=ylim)#,xlim=newlim) 
if(add.line) abline(h=0,lwd=lwd.ab,lty=3,col="darkgray")
legend("topleft","b",text.font=2,cex=cexll,bty="n",inset=ll,xpd=NA)
##---------------------------------------------------## add effect anomaly
if(add_eff_anom){
eff_anom<-p2$res$visregRes-mean(p1$res$visregRes) ## anomaly?
eff_col<-as.character(factor(eff_anom>=0,labels=eff_cols))  
plot(years,eff_anom,type="h",lwd=lwd.h,col=eff_col,lend=1,xlim=xlim,ylim=ylim_anom, xlab="Year",ylab="Anomaly of partial effect") 
abline(h=0,lty=1,lwd=0.5)
}
##------------------------------------------------------## add time series
if(add_time_series){ 
yy<-covariates$pink_tot_num ## convert tons to 1000s of tons
plot(xx,yy,type="l",lwd=1.5,col="saddlebrown",xlab="Year",ylab="Pink salmon abundance (millions)",ylim=newlim,xlim=xlim) ## or use: chocolate2
legend("topleft","f",text.font=2,cex=cexll,bty="n",inset=ll,xpd=NA)
}
##====================================## ocean temperature previous summer
covar<-mod_terms_ordered[3];covar<-"BSsumT_lag1"
xtrans<-function(x) { (x*data_sds[names(data_sds)==covar]+data_means[names(data_means)==covar]) }
##------------------------------------------------------## partial effects
newlim<-c(7,11.1)
p3<-visreg(mod,xvar=covar,xlab="Summer SST (°C)",partial=T, ylab=ylab,scale="response",points.par=list(cex=cexpt,pch=21,lwd=0.1,bg=bg.pt), fill.par=list(col=alpha(1,0.1)),line.par=list(lwd=2,col=1),xtrans=xtrans, ylim=ylim)#,xlim=newlim)
if(add.line) abline(h=0,lwd=lwd.ab,lty=3,col="darkgray")
legend("topleft","c",text.font=2,cex=cexll,bty="n",inset=ll,xpd=NA)
##---------------------------------------------------## add effect anomaly
if(add_eff_anom){
eff_anom<-p3$res$visregRes-mean(p1$res$visregRes) ## anomaly?
eff_col<-as.character(factor(eff_anom>=0,labels=eff_cols))  
plot(years,eff_anom,type="h",lwd=lwd.h,col=eff_col,lend=1,xlim=xlim,ylim=ylim_anom, xlab="Year",ylab="Anomaly of partial effect") 
abline(h=0,lty=1,lwd=0.5)
}
##------------------------------------------------------## add time series
if(add_time_series){ 
yy<-covariates$BSsumT;ymean<-mean(yy);yanomaly<-yy-ymean
cols<-as.character(factor(yanomaly>=0,labels=colors))
plot(NA,NA,xlab="Year",ylab="Summer SST (°C)",ylim=newlim,xlim=xlim)
abline(h=ymean,lty=3)
points(xx,yy,type="o",lwd=0.5,bg=cols,pch=21,cex=1) 
legend("topleft","g",text.font=2,cex=cexll,bty="n",inset=ll,xpd=NA)
}
##========================================## ocean temperature last winter
covar<-mod_terms_ordered[4];covar<-"AIwinT"
xtrans<-function(x) { (x*data_sds[names(data_sds)==covar]+data_means[names(data_means)==covar]) }
##------------------------------------------------------## partial effects
newlim<-c(0,3.8)
p4<-visreg(mod,xvar=covar,xlab="Winter SST (°C)",partial=T, ylab=ylab,scale="response",points.par=list(cex=cexpt,pch=21,lwd=0.1,bg=bg.pt),fill.par=list(col=alpha(1,0.1)),line.par=list(lwd=2,col=1),xtrans=xtrans, ylim=ylim)#,xlim=newlim)
if(add.line) abline(h=0,lwd=lwd.ab,lty=3,col="darkgray")
legend("topleft","d",text.font=2,cex=cexll,bty="n",inset=ll,xpd=NA)
##---------------------------------------------------## add effect anomaly
if(add_eff_anom){
eff_anom<-p4$res$visregRes-mean(p1$res$visregRes) ## anomaly?
eff_col<-as.character(factor(eff_anom>=0,labels=eff_cols))  
plot(years,eff_anom,type="h",lwd=lwd.h,col=eff_col,lend=1,xlim=xlim,ylim=ylim_anom, xlab="Year",ylab="Anomaly of partial effect") 
abline(h=0,lty=1,lwd=0.5)
}
##------------------------------------------------------## add time series
if(add_time_series){ 
yy<-covariates$AIwinT;ymean<-mean(yy);yanomaly<-yy-ymean
cols<-as.character(factor(yanomaly>=0,labels=colors))
plot(NA,NA,xlab="Year",ylab="Winter SST (°C)",ylim=newlim,xlim=xlim)
abline(h=ymean,lty=3)
points(xx,yy,type="o",lwd=0.5,bg=cols,pch=21,cex=1)
legend("topleft","h",text.font=2,cex=cexll,bty="n",inset=ll,xpd=NA)
}
##-----------------------------------------------------------## save plot
dev.off()

##======================================================================##
##========================================## model time series predictions
##======================================================================##
newdata<-list(total_return=data$total_return,AIwinT=data$AIwinT,BSsumT_lag1=data$BSsumT_lag1,pink_tot_num_lag1=data$pink_tot_num_lag1)
predicted<-predict(mod,newdata=newdata,se.fit=T,interval="confidence",level=0.95,type="response")
pred<-data.frame(predicted$fit)
##=================================================================## plot
pdf("Model-predicted-and-observed.pdf",width=4.5,height=3.5)
par(mar=c(3.5,3.5,0.5,1),mgp=c(2,0.5,0),cex.axis=0.9,cex.lab=1.1,tcl=-0.3)
xlim<-c(min(data$year)+1,max(data$year)-1)
plot(NA,NA,xlim=xlim,ylim=c(-30,23),xlab="Year",ylab="Mean size-at-age anomaly (mm)")
##----------------------------------------------------------## predictions
col.poly<-"dimgray" ## "dimgray" or "grey90"
X.Vec<-c(data$year,tail(data$year,1),rev(data$year),data$year[1])
Y.Vec<-c(pred$lwr,tail(pred$upr,1),rev(pred$upr),pred$lwr[1])
polygon(X.Vec,Y.Vec,col="grey90",border=NA) 
lines(data$year,pred$upr,lwd=0.5,lty=1,col="grey50")
lines(data$year,pred$lwr,lwd=0.5,lty=1,col="grey50")
lines(data$year,pred$fit,lwd=1.0,lty=1,col="grey50")
##-------------------------------------------------## observed time-series
lines(data$year,data_other$SaA_anomaly,type="o",pch=16,cex=0.65,lwd=0.5)
#lines(data$year,data_other$SaA_anomaly,type="o",pch=21,bg="white",cex=0.6,lwd=0.5)
dev.off()

##======================================================================##
##======## observed size-at-age anomaly vs total return and pink abundance
##======================================================================##
pdat<-dplyr::select(alldata,year,SaA_anomaly,total_return,pink_tot_num_lag1)
ny<-dim(pdat)[1]
x<-pdat$total_return
y<-pdat$pink_tot_num_lag1
z<-pdat$SaA_anomaly
##================================================================## ggplot
pdf("Data-SaA-anamoly-vs-return-and-pinks-ggplot.pdf",width=5.5,height=4.2)
par(mar=c(3.5,3.5,1,1),mgp=c(2,0.5,0),cex.lab=1.1,cex.axis=0.9,tcl=-0.3,xaxs="i",yaxs="i")
pdat %>% ggplot(aes(x=total_return,y=pink_tot_num_lag1,size=SaA_anomaly,col=year))+ 
  geom_point(shape=1,fill=NA,color="black")+
  geom_point(alpha=0.5)+ 
  scale_radius(range=c(0.5,9))+ 
  scale_color_viridis(option="inferno")+
  scale_x_continuous(limits=c(0,75),breaks=seq(0,75,10))+
  scale_y_continuous(limits=c(0,750),breaks=seq(0,750,100))+
  theme_classic()+
  labs(x="Total return (millions)",y="Pink salmon abundance (millions)")+ 
  labs(size="Size-at-age\nanomaly (mm)",color="Return year")+
  theme(strip.background=element_blank(),
        axis.line=element_line(size=0.1),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        panel.border=element_rect(fill=NA,size=1),
        legend.key.size=unit(0.5,'cm'),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8)
        )
dev.off()

##======================================================================##
##===============================================## model cross validation
##======================================================================##
## out-of-sample predictions on randomly drawn training and test data
##=========================================================## model forms
mod_list<-get.models(mod_select,subset=delta<2) 
nM<-length(mod_list)
test_forms<-all_terms<-list()
for(i in 1:nM) {
  mymod<-mod_list[[i]]
  terms<-attributes(mymod$terms)$term.labels
  all_terms[[i]]<-terms
  nterms<-length(terms)
  if(nterms==0) { my_mod<-formula("SaA_anomaly~1") } else { my_mod<-formula(paste("SaA_anomaly~",paste0(terms,collapse="+"))) }
  test_forms[[i]]<-my_mod
}
## exclude models that include quadratic without linear effect of predictor
test_forms<-test_forms[-c(8,9,15)] 

##=====================================================## cross-validation
nS<-10000 ## number of draws/seeds 
nMod<-length(test_forms)
seeds<-sample(seq(1e6),nS,replace=FALSE)
RMSE<-array(dim=c(nS,nMod))
formulas<-list()
cnt<-1
'%!in%'<-function(x,y)!('%in%'(x,y))
start.time<-Sys.time()
for(i in 1:nS) {
  set.seed(seeds[cnt])
  train<-sort(sample(seq(nY),round(0.75*nY),replace=F)) ## use x% to train
  traindata<-data[train,] 
  test<-seq(nY)[seq(nY) %!in% train]
  testdata<-data[test,]
  ##------------------------------------------------## loop model structures
  for(j in 1:nMod) { 
    test_mod<-form<-test_forms[[j]]
    term_list<-strsplit(as.character(test_mod)," ")[[3]]
    term_list<-term_list[term_list!="+"]
    trainmod<-gls(test_mod,data=traindata,method="ML")
    predicted<-predict(trainmod,newdata=testdata,se.fit=T,type="response")
    ##--------------------------------------------## root mean squared error
    pred<-as.numeric(predicted$fit)
    true<-as.numeric(testdata$SaA_anomaly)
    RMSE[i,j]<-sqrt(sum((pred-true)^2)/nY)
    ##------------------------------------------------------## save formulas
    prev_mod<-trainmod
    save_terms<-attributes(prev_mod)$namBetaFull[-1]
    allterms<-paste(save_terms,collapse="+") 
    nt<-length(allterms)
    new_mod<-formula(paste("~",allterms,sep="")) 
    formulas[[j]]<-new_mod
  } ## end loop over models (j)
  cnt<-cnt+1
} ## end stochastic loop (i)
##----------------------------------------------------------## elapsed time
end.time<-Sys.time()
elapsed<-end.time-start.time
print(round(elapsed,2))
##-----------------------------------------------------## model forms short
mod_forms<-unlist(test_forms)
mod_forms<-gsub("SaA_anomaly","",mod_forms)
mod_forms<-gsub("total_return","R",mod_forms)
mod_forms<-gsub("pink_tot_num_lag1","P",mod_forms)
mod_forms<-gsub("AIwinT:regime","W:r",mod_forms)
mod_forms<-gsub("BSsumT_lag1:regime","S:r",mod_forms)
mod_forms<-gsub("AIwinT","W",mod_forms)
mod_forms<-gsub("BSsumT_lag1","S",mod_forms)
mod_forms<-gsub("1","",mod_forms)
mod_forms<-gsub(" ","",mod_forms)
mod_forms<-unlist(mod_forms)
##----------------------------------------------------------## median RMSEs
med_RMSE<-apply(RMSE,2,function(x) median(x,na.rm=T))
med_RMSE<-round(med_RMSE,4)
names(med_RMSE)<-mod_forms
data.frame(round(med_RMSE,2))

##======================================================================##
##======================================================================##
##======================================================================##