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

##============================================================## load data
setwd(paste0(homeDir,"/output/"))

##===========================================================## escapement
meansize_Y_esc<-read.csv("Mean_size_BB_wide_by_run_year_esc.csv")[,-1] 
meansize_YS_esc<-read.csv("Mean_size_by_system_by_run_year_esc.csv")[,-1] 

##================================================================## catch
meansize_Y_harv<-read.csv("Mean_size_BB_wide_by_run_year_harv.csv")[,-1]
meansize_YS_harv<-read.csv("Mean_size_by_system_by_run_year_harv.csv")[,-1]

##=================================================================## run
meansize_Y_all<-read.csv("Mean_size_BB_wide_by_run_year_all.csv")[,-1]
meansize_YS_all<-read.csv("Mean_size_by_system_by_run_year_all.csv")[,-1]

##==============================================## selection differentials
## selected mean trait minus population mean trait 
##-----------------------------------------------------------## each river
sel_diffs<-meansize_YS_esc[,-1]-meansize_YS_all[,-1]
sel_diffs[is.na(sel_diffs)]<-0
sel_diffs$year<-meansize_YS_esc[,1]
##-------------------------------------------------------------## bay-wide
sel_diff_BB<-data.frame(sel_diff=meansize_Y_esc[,2]-meansize_Y_all[,2])
sel_diff_BB$year<-meansize_Y_esc[,1]
write.csv(sel_diff_BB,"Selection_differentials_Y.csv")

##======================================================================##
