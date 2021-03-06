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
  
  


# read in length ----

length <- read_csv(file=paste(study,"complete.length.csv",sep = "."),na = c("", " "),col_types = cols(.default = "c"))%>%
  mutate(number=as.numeric(number))%>%
  glimpse()



url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master<-googlesheets4::read_sheet(url)%>%ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  filter(grepl('SW', marine.region))%>% # Select marine region (currently this is only for Australia)
  select(family,genus,species, minlegal.wa)%>%
  distinct()%>%
  glimpse()

no.legal.snapper <- length%>%
  left_join(master)%>%
  dplyr::filter(species%in%c("auratus"))%>%
  dplyr::mutate(legal=ifelse(length>minlegal.wa,"legal","non-legal"))%>%
  dplyr::group_by(sample,legal)%>%
  dplyr::summarise(number=sum(number))%>%
  dplyr::ungroup()%>%
  tidyr::complete(nesting(sample),nesting(legal)) %>%
  replace_na(list(number = 0))

no.legal.size.all<-length%>%
  left_join(master)%>%
  # dplyr::filter(species%in%c("auratus"))%>%
  dplyr::mutate(legal=ifelse(length>minlegal.wa,"legal","non-legal"))%>%
  dplyr::filter(!is.na(minlegal.wa))%>%
  dplyr::group_by(sample,legal)%>%
  dplyr::summarise(number=sum(number))%>%
  dplyr::ungroup()%>%
  tidyr::complete(nesting(sample),nesting(legal)) %>%
  replace_na(list(number = 0))


mass<-read_csv(file=paste(study,"complete.mass.csv",sep = "."),na = c("", " "),col_types = cols(.default = "c"))%>%
  mutate(mass.g=as.numeric(mass.g))%>%
  glimpse()

total.mass<-mass%>%
  replace_na(list(mass.g=0))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise(mass.total=sum(mass.g))%>%
  dplyr::ungroup()%>%
  tidyr::complete(nesting(sample)) %>%
  full_join(samples)%>%
  replace_na(list(mass.total = 0))

mass.30cm<-mass%>%
  mutate(length.cm=as.numeric(length.cm))%>%
  filter(length.cm>30)%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise(mass.30cm=sum(mass.g))%>%
  dplyr::ungroup()%>%
  tidyr::complete(nesting(sample)) %>%
  full_join(samples)%>%
  replace_na(list(mass.30cm = 0))

mass.20cm<-mass%>%
  mutate(length.cm=as.numeric(length.cm))%>%
  filter(length.cm>20)%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise(mass.20cm=sum(mass.g))%>%
  dplyr::ungroup()%>%
  tidyr::complete(nesting(sample)) %>%
  full_join(samples)%>%
  replace_na(list(mass.20cm = 0))

mass.summaries<-left_join(total.mass,mass.20cm)%>%
  left_join(.,mass.30cm)

setwd(tidy.dir)
dir()
write.csv(gam.data, file=paste(study,"gamdata.csv",sep = "."), row.names=FALSE)

write.csv(pink.snapper, file=paste(study,"pink.snapper.maxn.csv",sep = "."), row.names=FALSE)
write.csv(king.wrasse, file=paste(study,"king.wrasse.maxn.csv",sep = "."), row.names=FALSE)
write.csv(ta.sr, file=paste(study,"total.abundance.and.species.richness.csv",sep = "."), row.names=FALSE)
write.csv(no.legal.snapper, file=paste(study,"legal.sized.pink.snapper.csv",sep = "."), row.names=FALSE)
write.csv(no.legal.size.all, file=paste(study,"legal.sized.fish.csv",sep = "."), row.names=FALSE)
write.csv(mass.summaries, file=paste(study,"mass.summaries.csv",sep = "."), row.names=FALSE)
