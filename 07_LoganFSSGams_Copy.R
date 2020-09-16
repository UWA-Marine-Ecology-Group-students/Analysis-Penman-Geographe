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
library(FSSgam)

# Study name---
study<-"2014-12_Geographe.Bay_stereoBRUVs" ## change for your project

## Set your working directory ----
setwd("~/Github/Analysis-Penman-Geographe")
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own

## Save these directory names to use later---- 
tidy.dir<-paste(working.dir,"Tidy data",sep="/")
em.dir<-paste(working.dir,"EM Export",sep="/")
models.dir<-paste(working.dir,"Models",sep="/")

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
  mutate(reef= consolidated + sponges + stony.corals + turf.algae + macroalgae + seagrasses)%>%
  na.omit()%>%
  glimpse()
  
 

#---------------------- now fit the models -------------------------#check long data change --------------------------------
require(mgcv)
require(MuMIn)
require(doParallel)
require(plyr)

## Changed to Long data for reproducible example for Case Study 2 where response variables are stacked one upon each other
long.dat <- gam.data %>% 
  gather (Taxa, Response, `Carangidae Pseudocaranx spp`:total.abundance)%>%
  glimpse()

dat <- long.dat
View(dat)

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

#remove stony corals, sponges, consolidated, macroalgae
#re-set predictor variables (now containing reef instead)--check for correlations
pred.vars=c("depth","turf.algae","seagrasses", "reef")


# Check to make sure Response vector has not more than 80% zeros----
dat <- long.dat
name<-"geographe"

unique.vars=unique(as.character(dat$Taxa))
unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$Taxa==unique.vars[i]),]
  if(length(which(temp.dat$response==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}
unique.vars.use     

# Run the full subset model selection----
resp.vars=unique.vars.use
use.dat=dat
# factor.vars=c("Status")# Status as a Factor with two levels
out.all=list()
var.imp=list()

setwd(models.dir)

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$Taxa==resp.vars[i]),]
  
  Model1=gam(response~s(depth,k=3,bs='cr')#+ s(Location,Site,bs="re")
             ,
             family=tw(),  data=use.dat)
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               pred.vars.cont=pred.vars,
                               #pred.vars.fact=factor.vars,
                               #linear.vars="Distance",
                               k=3#,
                               #null.terms="s(Location,Site,bs='re')"
                               )
  out.list=fit.model.set(model.set,
                         max.models=600,
                         parallel=T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=3),]
  out.all=c(out.all,list(out.i))
  # var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Either raw importance score
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name=as.character(out.i$modname[m])
    
    png(file=paste(name,m,resp.vars[i],"mod_fits.png",sep="_"))
    if(best.model.name!="null"){
      par(mfrow=c(3,1),mar=c(9,4,3,1))
      best.model=out.list$success.models[[best.model.name]]
      plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=2,text=resp.vars[i],outer=F)}  
    dev.off()
  }
}

# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits[,-2],file=paste(name,"all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(name,"all.var.imp.csv",sep="_"))

# Generic importance plots-
heatmap.2(all.var.imp,notecex=0.4,  dendrogram ="none",
          col=colorRampPalette(c("white","yellow","red"))(10),
          trace="none",key.title = "",keysize=2,
          notecol="black",key=T,
          sepcolor = "black",margins=c(12,8), lhei=c(4,15),Rowv=FALSE,Colv=FALSE)

