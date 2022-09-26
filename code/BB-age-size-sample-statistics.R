##=====================================================================##
##                                                                     ##
##    Summary statistics for catch and escapement age-length samples   ##
##                                                                     ##
##=====================================================================##
pkgs<-c("here","tidyr","dplyr")
if(length(setdiff(pkgs,rownames(installed.packages())))>0) { install.packages(setdiff(pkgs,rownames(installed.packages())),dependencies=T) }
invisible(lapply(pkgs,library,character.only=T))
homeDir<-here::here()
setwd(homeDir)

load("data/BBasl_to_2020_clean.Rdata")
dat<-BBasl_to_2020_clean
count(dat) ## total samples dataset

dat <- dat[dat$RiverSystem!="Alagnak",] ## analyses done without 'Alagnak'
count(dat) ## total samples used in analyses

## number of samples per year
nSy <- dat %>% 
  group_by(Year) %>% 
  summarize(n=n()) 

## number of samples by ASL project type
nSt <- dat %>% 
  group_by(ASLProjectType) %>% 
  summarize(n=n())

## number of samples by year and ASL project type
nSyt <- dat %>% 
  group_by(Year,ASLProjectType) %>% 
  summarize(n=n()) %>%
  pivot_wider(names_from=ASLProjectType,values_from=n) 

## number of samples by year and river
nSyr <- dat %>% 
  filter(!is.na(RiverSystem)) %>%
  group_by(Year,RiverSystem) %>% 
  summarize(n=n()) %>%
  pivot_wider(names_from=RiverSystem,values_from=n) 

## number of escapement samples by year and river
nSyr_esc <- dat %>% 
  filter(ASLProjectType=="escapement") %>%
  filter(!is.na(RiverSystem)) %>%
  group_by(Year,RiverSystem) %>% 
  summarize(n=n()) %>%
  pivot_wider(names_from=RiverSystem,values_from=n) # %>%
  # write.csv("data/AL_samples_esc_by_river.csv")

## number of fishery/catch samples by year and river
nSyr_catch <- dat %>% 
  filter(ASLProjectType=="commercial catch") %>%
  group_by(Year,DistrictName) %>% 
  summarize(n=n()) %>%
  pivot_wider(names_from=DistrictName,values_from=n) # %>%
  # write.csv("data/AL_samples_catch_by_district.csv")

##=====================================================================##
##=====================================================================##
##=====================================================================##