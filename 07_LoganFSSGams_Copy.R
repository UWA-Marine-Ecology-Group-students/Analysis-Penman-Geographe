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
library(cowplot)


# Study name---
study<-"2014-12_Geographe.Bay_stereoBRUVs" ## change for your project

## Set your working directory ----
setwd("~/Github/Analysis-Penman-Geographe")
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own

## Save these directory names to use later---- 
tidy.dir<-paste(working.dir,"Tidy data",sep="/")
em.dir<-paste(working.dir,"EM Export",sep="/")
models.dir<-paste(working.dir,"Models",sep="/")
plots.dir <- paste(working.dir,"Plots",sep="/")
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
  dplyr::ungroup()%>%
  dplyr::group_by(scientific,sample)%>%
  dplyr::summarise(maxn = sum(maxn))%>%
  spread(scientific,maxn, fill = 0)%>%
  dplyr::mutate(total.abundance=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>% #Add in Totals
  dplyr::mutate(species.richness=rowSums(.[,2:120] > 0))%>% # double check these
  dplyr::select(sample,total.abundance,species.richness)%>%
  gather(.,"scientific","maxn",2:3)%>%
  dplyr::left_join(metadata)

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
  mutate(reef= consolidated + sponges + stony.corals + turf.algae + macroalgae + seagrasses)%>%
  na.omit()%>%
  glimpse()

#Changed to Long data for reproducible example for Case Study 2
long.dat <- gam.data %>% 
  gather (Taxa, response, `Carangidae Pseudocaranx spp`:total.abundance)%>%
  glimpse()
#Set predictor variables -----
pred.vars=c("depth","macroalgae","turf.algae","unconsolidated","seagrasses","consolidated", "sponges", "stony.corals") 

#Check for correlations of predictor variables (Removing anything highly correlated (>0.95)--
round(cor(long.dat[,pred.vars]),2)
#re-set predictor variables (now containing reef instead)
pred.vars=c("depth","turf.algae","seagrasses", "reef")

# Check to make sure response vector has not more than 80% zeros----check for correlations
round(cor(long.dat[,pred.vars]),2)

dat <- long.dat
name<-"geographe"

#----------now fit the models---Using Tim Example 2-------------

# Part 1-FSS modeling----

# librarys----
detach("package:plyr", unload=TRUE)#will error - don't worry
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(RColorBrewer)
library(doParallel) #this can removed?
library(doSNOW)
library(gamm4)
library(RCurl) #needed to download data from GitHub

# install package----
library(FSSgam)

# Bring in and format data
name<-"geographe"

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

#---Model fits and importance---
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

#custom plot of importance scores----
setwd(models.dir)
dat.taxa <-read.csv("geographe_all.var.imp.csv")%>% #from local copy
  rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  glimpse()

# Plotting defaults----
library(ggplot2)
# Theme-
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill="white"),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=8),
    legend.title = element_text(size=8, face="bold"),
    legend.position = "top",
    legend.direction="horizontal",
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10,angle = 90, hjust=1,vjust=0.5),
    axis.text.y=element_text(size=10,face="italic"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())


# colour ramps-
re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)

# Labels-
legend_title<-"Importance"

# Annotations
dat.taxa.label<-dat.taxa%>%
  mutate(label=NA)%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Carangidae Pseudocaranx spp","X",label)) %>%
  mutate(label=ifelse(predictor=="seagrasses"&resp.var=="Gerreidae Parequula melbournensis","X",label)) %>% 
  mutate(label=ifelse(predictor=="reef"&resp.var=="Labridae Coris auricularis","X",label)) %>%
  mutate(label=ifelse(predictor=="reef"&resp.var=="species.richness","X",label)) %>%
  mutate(label=ifelse(predictor=="turf.algae"&resp.var=="total.abundance","X",label)) %>% 
  mutate(label=ifelse(predictor=="seagrasses"&resp.var=="Sparidae Chrysophrys auratus","X",label)) %>% 
  glimpse()

head(dat.taxa.label)

# Plot gg.importance.scores ----
gg.importance.scores <- ggplot(dat.taxa.label, aes(x=predictor,y=resp.var,fill=importance))+
  geom_tile(show.legend=T) +
  scale_fill_gradientn(legend_title,colours=c("white", re), na.value = "grey98",
                       limits = c(0, max(dat.taxa.label$importance)))+
  scale_x_discrete(limits=c("depth",
                            "turf.algae",
                            "seagrasses",
                            "reef"),
                   labels=c(
                     "Depth (m)",
                     "Turf Algae (%)",
                     "Seagrass (%)",
                     "Reef (%)"))+
  scale_y_discrete(limits = c("Carangidae Pseudocaranx spp",
                              "Gerreidae Parequula melbournensis",
                              "Labridae Coris auricularis",
                              "Sparidae Chrysophrys auratus",
                              "species.richness",
                              "total.abundance"),
                   labels=c("Pseudocaranx spp",
                            "Parequula melbournensis",
                            "Coris auricularis",
                            "Chrysophrys auratus",
                            "Species Richness",
                            "Total Abundance"))+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()+
  Theme1+
  geom_text(aes(label=label))
gg.importance.scores

setwd(plots.dir)
ggsave("Heatmap_Importance_outputs.png",gg.importance.scores,width = 5,height = 5)
#use dev.off if error above occurs
dev.off()


### now  make a nice plot of the most interesting models-----

library(gridExtra)
library(grid)
# Theme-
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(fill="white"),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=15),
    legend.title = element_blank(),
    legend.position = c(0.2, 0.8),
    text=element_text(size=15),
    strip.text.y = element_text(size = 15,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=15),
    axis.title.y=element_text(vjust=0.6, angle=90, size=15),
    axis.text.x=element_text(size=15),
    axis.text.y=element_text(size=15),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())


# Manually make the most parsimonious GAM models for each taxa.....6/6 done
setwd(models.dir)
  
# Model 1: --------Carangidae Pseudocaranx spp + Depth --------
dat.cps<-dat%>%filter(Taxa=="Carangidae Pseudocaranx spp") # "Carangidae Pseudocaranx spp"
gamm=gam(response~s(depth,k=3,bs='cr'),family=tw(),  data=dat.cps)
gamm <- gam(response~s(depth,k=3,bs='cr'),family=tw(),  data=dat.cps) # change predictor variable per Taxa --------

#Predict Pseudocaranx spp with Depth---
mod<-gamm
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20)) %>% 
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
predicts.cps.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.cps.depth,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.cps.depth<-read.csv("predicts.csv")%>%
  glimpse()


# Model 2: ----- Total abundance + Turf Algae ----
dat.ta<-dat%>%filter(Taxa=="total.abundance") # "Total Abundance"
gamm=gam(response~s(turf.algae,k=3,bs='cr'),family=tw(),  data=dat.ta)
gamm <- gam(response~s(turf.algae,k=3,bs='cr'),family=tw(),  data=dat.ta) # change predictor variable per Taxa --------

#Predict Total Abundance with turf.algae model ---
mod<-gamm
testdata <- expand.grid(turf.algae=seq(min(dat$turf.algae),max(dat$turf.algae),length.out = 20)) %>% 
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
predicts.ta.turf = testdata%>%data.frame(fits)%>%
  group_by(turf.algae)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.ta.turf,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.ta.turf<-read.csv("predicts.csv")%>%
  glimpse()


# Model 3: --------Gerreidae Parequula melbournensis + Seagrass --------
dat.mel<-dat%>%filter(Taxa=="Gerreidae Parequula melbournensis") # "Gerreidae Parequula melbournensis"
gamm=gam(response~s(seagrasses,k=3,bs='cr'),family=tw(),  data=dat.mel)
gamm <- gam(response~s(seagrasses,k=3,bs='cr'),family=tw(),  data=dat.mel) # change predictor variable per Taxa --------

#Predict Gerreidae Parequula melbournensis with seagrasses---
mod<-gamm
testdata <- expand.grid(seagrasses=seq(min(dat$seagrasses),max(dat$seagrasses),length.out = 20)) %>% 
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
predicts.mel.sea = testdata%>%data.frame(fits)%>%
  group_by(seagrasses)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.mel.sea,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.mel.sea<-read.csv("predicts.csv")%>%
  glimpse()


# Model 4: --------Chrysophrys auratus + Seagrass --------
dat.aur<-dat%>%filter(Taxa=="Sparidae Chrysophrys auratus") # "Sparidae Chrysophrys auratus"
gamm=gam(response~s(seagrasses,k=3,bs='cr'),family=tw(),  data=dat.aur)
gamm <- gam(response~s(seagrasses,k=3,bs='cr'),family=tw(),  data=dat.aur) # change predictor variable per Taxa --------

#Predict Chrysophrys auratus with seagrasses---
mod<-gamm
testdata <- expand.grid(seagrasses=seq(min(dat$seagrasses),max(dat$seagrasses),length.out = 20)) %>% 
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
predicts.aur.sea = testdata%>%data.frame(fits)%>%
  group_by(seagrasses)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.aur.sea,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.aur.sea<-read.csv("predicts.csv")%>%
  glimpse()


# Model 5: -------Coris auricularis + Reef --------
dat.cor<-dat%>%filter(Taxa=="Labridae Coris auricularis") # "Labridae Coris auricularis"
gamm=gam(response~s(reef,k=3,bs='cr'),family=tw(),  data=dat.cor)
gamm <- gam(response~s(reef,k=3,bs='cr'),family=tw(),  data=dat.cor) # change predictor variable per Taxa --------

#Predict Coris auricularis + Reef---
mod<-gamm
testdata <- expand.grid(reef=seq(min(dat$reef),max(dat$reef),length.out = 20)) %>% 
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
predicts.cor.reef = testdata%>%data.frame(fits)%>%
  group_by(reef)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.cor.reef,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.cor.reef<-read.csv("predicts.csv")%>%
  glimpse()


# Model 6: ------- Species Richness + Reef --------
dat.sr<-dat%>%filter(Taxa=="species.richness") # "species.richness"
gamm=gam(response~s(reef,k=3,bs='cr'),family=tw(),  data=dat.sr)
gamm <- gam(response~s(reef,k=3,bs='cr'),family=tw(),  data=dat.sr) # change predictor variable per Taxa --------

#Predict Chrysophrys auratus with seagrasses---
mod<-gamm
testdata <- expand.grid(reef=seq(min(dat$reef),max(dat$reef),length.out = 20)) %>% 
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)
predicts.sr.reef = testdata%>%data.frame(fits)%>%
  group_by(reef)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()
write.csv(predicts.sr.reef,"predicts.csv") #there is some BUG in dplyr - that this fixes
predicts.sr.reef<-read.csv("predicts.csv")%>%
  glimpse()


#-----------Plots-------------------------
#Plot Model 1: Pseudocaranx spp with Depth----
ggmod.cps.depth<- ggplot() +
  ylab("MaxN")+
  xlab("Depth (m)")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("red", "black"))+
  geom_point(data=dat.cps,aes(x=depth,y=response),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.cps.depth,aes(x=depth,y=response),alpha=0.5)+
  geom_line(data=predicts.cps.depth,aes(x=depth,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.cps.depth,aes(x=depth,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  #annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=5)+
  annotate("text", x = -Inf, y=Inf, label = "Pseudocaranx spp",vjust = 1, hjust = -.1,size=5,fontface="italic")+
  geom_blank(data=dat.cps,aes(x=depth,y=response*1.05))#to nudge data off annotations
ggmod.cps.depth

#Plot Model 2:  turf.algae from model for Total Abundance ----
ggmod.ta.turf<- ggplot() +
  ylab("MaxN")+
  xlab("Turf Algae (%)")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("red", "black"))+
  geom_point(data=dat.ta,aes(x=turf.algae,y=response),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.ta.turf,aes(x=turf.algae,y=response),alpha=0.5)+
  geom_line(data=predicts.ta.turf,aes(x=turf.algae,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.ta.turf,aes(x=turf.algae,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  #annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=5)+
  annotate("text", x = -Inf, y=Inf, label = "Total Abundance",vjust = 1, hjust = -.1,size=5,fontface="italic")+
  geom_blank(data=dat.ta,aes(x=turf.algae,y=response*1.05))#to nudge data off annotations
ggmod.ta.turf

#Plot Model 3: Gerreidae Parequula melbournensis with seagrasses----
ggmod.mel.sea<- ggplot() +
  ylab("MaxN")+
  xlab("Seagrass (%)")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("red", "black"))+
  geom_point(data=dat.mel,aes(x=seagrasses,y=response),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.mel.sea,aes(x=seagrasses,y=response),alpha=0.5)+
  geom_line(data=predicts.mel.sea,aes(x=seagrasses,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.mel.sea,aes(x=seagrasses,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  #annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=5)+
  annotate("text", x = -Inf, y=Inf, label = "Parequula melbournensis",vjust = 1, hjust = -.1,size=5,fontface="italic")+
  geom_blank(data=dat.mel,aes(x=seagrasses,y=response*1.05))#to nudge data off annotations
ggmod.mel.sea

#Plot Model 4: Chrysophrys auratus with seagrasses----
ggmod.aur.sea<- ggplot() +
  ylab("MaxN")+
  xlab("Seagrass (%)")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("red", "black"))+
  geom_point(data=dat.aur,aes(x=seagrasses,y=response),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=predicts.aur.sea,aes(x=seagrasses,y=response),alpha=0.5)+
  geom_line(data=predicts.aur.sea,aes(x=seagrasses,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.aur.sea,aes(x=seagrasses,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  #annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=5)+
  annotate("text", x = -Inf, y=Inf, label = "Chrysophrys auratus",vjust = 1, hjust = -.1,size=5,fontface="italic")+
  geom_blank(data=dat.aur,aes(x=seagrasses,y=response*1.05))#to nudge data off annotations
ggmod.aur.sea

#Plot Model 5:  Coris auricularis + reef----
ggmod.cor.reef<- ggplot() +
  ylab("MaxN")+
  xlab("Reef (%)")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("red", "black"))+
  geom_point(data=dat.cor,aes(x=reef,y=response),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=(predicts.cor.reef),aes(x=reef,y=response),alpha=0.5)+
  geom_line(data=(predicts.cor.reef),aes(x=reef,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=(predicts.cor.reef),aes(x=reef,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  #annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=5)+
  annotate("text", x = -Inf, y=Inf, label = "Coris auricularis",vjust = 1, hjust = -.1,size=5,fontface="italic")+
  geom_blank(data=dat.cor,aes(x=reef,y=response*1.05))#to nudge data off annotations
ggmod.cor.reef

#Plot Model 6:  species.richness + reef----
ggmod.sr.reef<- ggplot() +
  ylab("MaxN")+
  xlab("Reef (%)")+
  #scale_color_manual(labels = c("Fished", "SZ"),values=c("red", "black"))+
  geom_point(data=dat.sr,aes(x=reef,y=response),  alpha=0.75, size=2,show.legend=FALSE)+
  geom_line(data=(predicts.sr.reef),aes(x=reef,y=response),alpha=0.5)+
  geom_line(data=(predicts.sr.reef),aes(x=reef,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=(predicts.sr.reef),aes(x=reef,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  #annotate("text", x = -Inf, y=Inf, label = "(d)",vjust = 1, hjust = -.1,size=5)+
  annotate("text", x = -Inf, y=Inf, label = "Species Richness",vjust = 1, hjust = -.1,size=5,fontface="italic")+
  geom_blank(data=dat.sr,aes(x=reef,y=response*1.05))#to nudge data off annotations
ggmod.sr.reef

combined.plots <- plot_grid(ggmod.sr.reef, ggmod.ta.turf, 
                            ggmod.aur.sea, ggmod.cor.reef, 
                            ggmod.mel.sea, ggmod.cps.depth,
                            ncol = 2, labels = c("a)", "b)","c)","d)","e)","f)"))
combined.plots
setwd(plots.dir)
ggsave("GAM_outputs.png",combined.plots,width = 9,height = 11)
