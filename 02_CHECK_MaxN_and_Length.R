
### Error checks of MaxN and Length files created from EventMeasure or generic stereo-video annotations via GlobalArchive

## This script is designed to be used interatively to suggest species name and length corrections that should be made to original EventMeasure (.EMObs) or generic annotation files AND for subsequent data analysis.

# NOTE: ERRORS SHOULD BE FIXED IN THE .EMObs AND RE-UPLOADED TO GLOBAL ARCHIVE!


### OBJECTIVES ###
# 1. Import data and run BASIC error reports
# 2. Limit length data by range and precision rules
# 3. run SERIOUS error reports against a master species list
# 4. Visualise what MaxN are missing in the stereoMaxN
# 5. Write data for analysis that passes checks

### Please forward any updates and improvements to tim.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au or raise an issue in the "globalarchive-query" GitHub repository

# Please email tim.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au if you would like the life.history or synonyms googlesheets shared with you or to have your local species information added.


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
library(readr)
library(ggplot2)

## Set Study Name ----
# Change this to suit your study name. This will also be the prefix on your final saved files.
study<-"2014-12_Geographe.Bay_stereoBRUVs"

## Folder Structure ----
# This script uses one main folder ('working directory')

# Three subfolders will already be created within the 'working directory'. They are 'Downloads','Data to be checked' and 'Tidy data' (Script 1)

# In addition, this script will make new folders for:
#'Plots' to save initial checking plots
#'Errors to check' to save all the error files e.g. lists of taxa that are not in the life history sheet

## Set your working directory ----
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # sets working directory to that of this script - or type your own

## Save these directory names to use later----
to.be.checked.dir<-paste(working.dir,"Staging",sep="/") 
download.dir<-paste(working.dir,"Downloads",sep="/")
tidy.dir<-paste(working.dir,"Tidy data",sep="/")
plots.dir=paste(working.dir,"Plots",sep="/")
error.dir=paste(working.dir,"Errors to check",sep="/")

## Create a folder for Plots and Errors ----
# The two lines below will create the 'Plots' and 'Errors to check' subfolders within the working directory
dir.create(file.path(working.dir, "Plots"))
dir.create(file.path(working.dir, "Errors to check"))

# Import unchecked data from staging folder----
setwd(to.be.checked.dir)

# Import metadata ---
metadata<-read.csv(paste(study,"metadata.csv",sep="_"))

# Import MaxN file---
maxn<-read_csv(paste(study,"maxn.csv",sep="_"))%>%
  mutate(maxn=as.numeric(maxn))%>%
  mutate(species=tolower(species))%>%
  select(campaignid,sample,family,genus,species,maxn)%>%
  replace_na(list(family="Unknown",genus="Unknown",species="spp"))%>% # remove any NAs in taxa name
  glimpse()


# BASIC checks----


# SERIOUS data checks using the life.history googlesheet ----
# Checks on fish length vs their max.length in the life.history sheet will be done below

# life.history checks will:
# 1. Check for species occurence vs their known distribution
# 2. Check for any species that may have changed names and suggest synonyms
# 3. Check measured length vs max.length for that species

# Make sure to select the correct Country and Marine Region that matches your data (see the two filter lines below)
# Follow this link to see a map of the marine regions used in the life history sheet
#  https://soe.environment.gov.au/theme/marine-environment/topic/2016/marine-regions

# These Marine Region abbreviations are:
# 'SW' - South-west
# 'NW' - North-west
# 'N' - North
# 'CS' - Coral Sea
# 'TE' - Temperate East
# 'SE' - South-east
# 'Christmas.Island' - Christmas Island
# 'Cocos.Keeling' - Cocos (Keeling) Island
# 'Lord.Howe.Island' - Lord Howe Island

# Use the abbreviation in the code below
# currently set for the Pilbara, Australia example data set ('NW' for North-west)

url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master<-googlesheets4::read_sheet(url)%>%ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  filter(grepl('SW', marine.region))%>% # Select marine region (currently this is only for Australia)
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(a=as.numeric(a))%>%
  dplyr::mutate(b=as.numeric(b))%>%
  select(family,genus,species,marine.region,length.measure,a,b,all,bll,fb.length_max,fb.ltypemaxm)%>%
  distinct()%>%
  glimpse()

synonymsurl <- "https://docs.google.com/spreadsheets/d/1R0uU9Q0VkUDQFgGTK3VnIGxmc101jxhlny926ztWoiQ/edit?ts=5e6f37a2#gid=567803926"

synonyms<- googlesheets4::read_sheet(synonymsurl)%>%
  distinct()%>%
  ga.clean.names()%>%
  select(-comment)

# Update by synonyms ----
# This function will change the names of species that have been reclassified (i.e. Pagrus auratus to Chrysophrys auratus). This function also fixes some common spelling mistakes (i.e. Chyrosophyrs	auratus to Chrysophrys auratus)

# Use return.changes=T to view the taxa.names.updated
# Use save.report to save .csv file in your error directory

maxn<-ga.change.synonyms(maxn,return.changes=T,save.report = T)

# Check MaxN for species that have not previously been observed in your region ----
maxn.species.not.previously.observed<-master%>%
  anti_join(maxn,.,by=c("family","genus","species"))%>% 
  distinct(campaignid,sample,family,genus,species)%>% # use this line to show specific drops OR
  # distinct(family,genus,species)%>% # use this line to keep only fam, gen, spe
  filter(!species%in%c("spp"))%>% # Ignore spp in the report
  glimpse()

setwd(error.dir)
write.csv(maxn.species.not.previously.observed,file=paste(study,"maxn.species.not.previously.observed.csv",sep = "."), row.names=FALSE)


# WRITE FINAL checked data----
setwd(tidy.dir)
dir()

write.csv(metadata, file=paste(study,"checked.metadata.csv",sep = "."), row.names=FALSE)
write.csv(maxn, file=paste(study,"checked.maxn.csv",sep = "."), row.names=FALSE)

# Go to FORMAT script (3) 
