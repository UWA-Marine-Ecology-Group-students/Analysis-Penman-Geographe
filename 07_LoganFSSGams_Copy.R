# Clear memory ----
rm(list=ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
# To connect to life.history
library(httpuv)
library(googlesheets4)
# To tidy data
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(fst)

# Study name---
study<-"2014-12_Geographe.Bay_stereoBRUVs" ## change for your project

## Set your working directory ----
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own

## Save these directory names to use later---- 
tidy.dir<-paste(working.dir,"Tidy data",sep="/")
em.dir<-paste(working.dir,"EM Export",sep="/")

# Read in the data----
setwd(tidy.dir)
dir()

# Read in metadata----
metadata<-read_csv(file=paste(study,"checked.metadata.csv",sep = "."),na = c("", " "))%>%
  dplyr::mutate(id=paste(campaignid,sample,sep="."))%>%
  glimpse()

samples<-metadata%>%distinct(sample)

# read in complete maxn ----
maxn <- read_csv(file=paste(study,"complete.maxn.csv",sep = "."),na = c("", " "))%>%
  glimpse()

setwd(em.dir)
dir()
habitat<- read.csv("stereo-BRUVs_broad.percent.cover.csv")%>%
  dplyr::filter(campaignid=="2014-12_Geographe_Bay_stereoBRUVs")%>%
  ga.clean.names()%>%
  dplyr::select(-c("latitude", "longitude", "campaignid"))%>%
  glimpse()

pink.snapper <- maxn%>%
  filter(species%in%c("auratus"))

king.wrasse <- maxn%>%
  filter(species%in%c("auricularis"))

ta.sr <- maxn%>%
  group_by(scientific,sample)%>%
  dplyr::summarise(maxn = sum(maxn))%>%
  spread(scientific,maxn, fill = 0)%>%
  mutate(total.abundance=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>% #Add in Totals
  mutate(species.richness=rowSums(.[,2:159] > 0))%>%
  dplyr::select(sample,total.abundance,species.richness)%>%
  gather(.,"scientific","maxn",2:3)%>%
  left_join(metadata)

names(maxn)
unique(maxn$scientific)

#-------------------------if using wide data-------------------------------
wide.maxn<-maxn%>%
  dplyr::filter(scientific%in%c("Carangidae Pseudocaranx spp",
                                "Labridae Coris auricularis",
                                "Gerreidae Parequula melbournensis",
                                "Sparidae Chrysophrys auratus"))%>%
  dplyr::select(sample, scientific, maxn)%>%
  spread(key = scientific, value = maxn)

wide.tasr<-ta.sr%>%
  dplyr::select(sample, scientific, maxn)%>%
  spread(key = scientific, value = maxn)

gam.data<-metadata%>%
  dplyr::left_join(.,wide.maxn)%>%
  dplyr::left_join(.,wide.tasr)%>%
  dplyr::left_join(.,habitat)%>%
  glimpse()

#---------------------- now fit the models ---------------------------------------------------------
require(mgcv)
require(MuMIn)
require(doParallel)
require(plyr)

## Changed to Long data for reproducible example for Case Study 2 where response variables are stacked one upon each other
long.dat <- gam.data %>% 
  gather (Taxa, response, `Carangidae Pseudocaranx spp`:total.abundance)%>%
  glimpse()

#Set predictor variables -----
pred.vars=c("depth","macroalgae","turf.algae","unconsolidated","seagrasses","consolidated", "sponges", "stony.corals") 

#Check for correlations of predictor variables (Removing anything highly correlated (>0.95)--
round(cor(long.dat[,pred.vars]),2)

# Plot of likely transformations -----
par(mfrow=c(3,2))
for (i in pred.vars) {
  x<-long.dat[ ,i]
  x = as.numeric(unlist(x))
  hist((x))#Looks best
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}

#create new variable 'reef' (consolidated, sponges, stony.corals, macroalgae, turf.algae)
long.dat<- long.dat%>% 
  mutate(reef= consolidated + sponges + stony.corals + turf.algae + macroalgae)%>%
  na.omit()%>%
  glimpse()


#reset predictor variables (now containing reef instead)--check for correlations
pred.vars=c("depth","unconsolidated","seagrasses","reef")

round(cor(long.dat[,pred.vars]),2)

par(mfrow=c(3,2))
for (i in pred.vars) {
  x<-long.dat[ ,i]
  x = as.numeric(unlist(x))
  hist((x))#Looks best
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}

# Check to make sure Response vector has not more than 80% zeros----
unique.vars=unique(as.character(long.dat$Taxa))
unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=long.dat[which(long.dat$Taxa==unique.vars[i]),]
  if(length(which(temp.dat$response==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}
unique.vars.use  


# Abbreviate terms ---- IF NEEDED???------






#-----------Run the full subset model selection--------







