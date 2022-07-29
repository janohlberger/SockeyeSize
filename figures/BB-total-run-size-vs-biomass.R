##=====================================================================##
##                                                                     ##
##  Plot relationship between total run size and total return biomass  ##
##                                                                     ##
##=====================================================================##
# rm(list=ls(all=T)) 
pkgs<-c("tidyverse","dplyr","mgcv")
if(length(setdiff(pkgs,rownames(installed.packages())))>0) { install.packages(setdiff(pkgs,rownames(installed.packages())),dependencies=T) }
invisible(lapply(pkgs,library,character.only=T))
if(exists("homeDir")) { } else { homeDir<-setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) }

##===========================================================## load data
esc.data<-read.csv("/Users/janohlberger/Documents/Work/UW/SalmonSizeChange/DataBB/Tim/ResampledEscapement_fromBroodTable.csv") ## without Alagnak
harv.data<-read.csv("/Users/janohlberger/Documents/Work/UW/SalmonSizeChange/DataBB/Tim/ResampledCatch_fromBroodTable.csv") 
all.data<-data.frame(rbind(esc.data,harv.data))

##===================================================## filter/select data
data<-all.data
data<-data[data$System!= "Alagnak",]
data<-dplyr::filter(data,!is.na(Length),Length!=0)
data$length<-as.numeric(data$Length)
data$age<-as.character(data$Age)
data$year<-data$ReturnYear
data<-dplyr::select(data,year,age,length,Count)
data<-data[order(data$year,data$age,data$length),]
data$mass_kg<-1.539e-8*data$length^3.017 ## convert length to mass in kg

##=======================================================## summarize data
return_abundance_Y<-data %>% group_by(year) %>% summarize(numbers=length(rep(length,Count))) %>% data.frame()
return_biomass_Y<-data %>% group_by(year) %>% summarize(biomass=sum(rep(mass_kg,Count))) %>% data.frame()
return_fishmass_Y<-data %>% group_by(year) %>% summarize(fishmass=mean(rep(mass_kg,Count))) %>% data.frame()
df<-merge(return_abundance_Y,return_biomass_Y,by="year")
df<-merge(df,return_fishmass_Y,by="year")

##=====================================================## plot relationship
pdf("observed/Fishmass-vs-abundance.pdf",width=5,height=4)
par(mar=c(3.5,3.5,0.5,0.5),mgp=c(2,0.5,0),tcl=-0.3,cex.lab=1.2)
x<-df$numbers/1e6;xlab<-"Total run size (millions)"
y<-df$biomass/1e6;ylab<-"Total biomass (million kg)"
y<-df$fishmass;ylab<-"Average fish mass (kg)"
plot(x,y,pch=16,xlab=xlab,ylab=ylab) 
gam_y<-gam(y~s(x),kmethod="REML",gamma=1)
gam_pred<-visreg(gam_y,plot=F,alpha=0.05)
low<-gam_pred$fit$visregLwr
upp<-gam_pred$fit$visregUpr
fit<-gam_pred$fit$visregFit
yrs<-gam_pred$fit$x
poly.x<-c(yrs,rev(yrs))
poly.y<-c(low,rev(upp))
#polygon(poly.x,poly.y,lwd=0.1,col="gray90",border="black")
#lines(yrs,fit,lwd=2,col="black")
points(x,y,pch=16) 
dev.off()

##=====================================================================##
##=====================================================================##
##=====================================================================##