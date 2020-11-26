# Script for creating bubblemaps 
#Created by Mike Taylor
rm(list=ls())

library(maptools)
library(png)
library(GISTools)
library(grid)
library(ggplot2)
library(gridExtra)
library(maps) 
library(ggmap)
library(rgdal)
library(dplyr)
library(cowplot)

#data.dir=("C:/Users/00095130/Google Drive/MEGFISH/Projects/BHP_BRUVs_Griffin_2017/Working/Data analysis")
data.dir=("C:/Users/00078110/Google Drive/BHP_BRUVs_Griffin_2017/Working/Data analysis")

plots=paste(data.dir,"Plots",sep="/")
tidy.data=paste(data.dir,"Data/Tidy data",sep="/")
gis.data=paste(data.dir,"Data/Shapefiles",sep="/")
plots.spatial=paste(data.dir,"Plots/Spatial",sep="/")
fish.pics=paste(data.dir,"Fish pics",sep="/")
functions=paste(data.dir,"Scripts/Functions",sep="/")

study<-"BHP_2017"
setwd(tidy.data)
dir()
maxn<-read.csv("BHP_Griffin_BRUVs_2017.maxn.taxa.L.factors..csv")
# Plotting themes pallettes and function----
Theme1 <-
  theme( # use theme_get() to see available options
    strip.text.x = element_text(size = 8,angle = 0,family="serif"),
    strip.text.y = element_text(size = 8,family="serif"),
    axis.title.x=element_text(vjust=-0.0, size=8,family="serif"),
    axis.title.y=element_text(vjust=0.0, angle=90, size=8,family="serif"),
    axis.text.x=element_text(size=8, angle=0,family="serif"),
    axis.text.y=element_text(size=8,family="serif"),
    legend.text = element_text(size=8,family="serif"),
    legend.title = element_text(size=8,family="serif"),
    panel.background = element_rect(fill = 'lightgrey', colour = 'lightgrey'),
    # plot.background = element_rect(fill = 'lightgrey', colour = 'lightgrey'),
    # annotate=element_text(size=10,family="TN"),
    #legend.margin = margin(t=-5, unit= "cm"),
    plot.title = element_text(hjust = 0, vjust=1,family="serif",face = "italic",size=12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(x=c(0.1,0.1,0.1,0.1),units="cm"))

theme_get()

Theme.axis.blank<-theme(
  axis.text.x=element_blank(),
  axis.text.y=element_blank(),
  axis.title.x=element_blank(),
  axis.title.y=element_blank())

Theme.y.axis.blank<-theme(
  axis.text.y=element_blank(),
  axis.title.y=element_blank())

Theme.x.axis.blank<-theme(
  axis.text.x=element_blank(),
  axis.title.x=element_blank())

# Theme1 <-
#   theme( # use theme_get() to see available options
#     strip.text.x = element_text(size = 12,angle = 0,family="TN"),
#     strip.text.y = element_text(size = 12,family="TN"),
#     axis.title.x=element_text(vjust=-0.0, size=16,family="TN"),
#     axis.title.y=element_text(vjust=0.0, angle=90, size=16,family="TN"),
#     axis.text.x=element_text(size=14, angle=0,family="TN"),
#     axis.text.y=element_text(size=14,family="TN"),
#     legend.text = element_text(size=14,family="TN"),
#     legend.title = element_text(size=16,family="TN"),
#     panel.background = element_rect(fill = 'lightgrey', colour = 'lightgrey'),
#     # plot.background = element_rect(fill = 'lightgrey', colour = 'lightgrey'),
#     # annotate=element_text(size=10,family="TN"),
#     #legend.margin = margin(t=-5, unit= "cm"),
#     plot.title = element_text(hjust = 0, vjust=2.12,family="TN",face = "italic",size=18),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     plot.margin = unit(x=c(0.1,0.1,0.1,0.1),units="cm"))

# Read in Shapefiles ----
setwd(gis.data)
dir()
OZ<-readOGR("aust_cd66states.shp")
OZ2<-readOGR("Australia coastline polygon medium 180220.shp")
Z<-fortify(OZ)
m50<-readOGR("bathy20-50.shp")
m100<-readOGR("bathy100-200.shp")
pipeline <- readOGR("Griffin pipeline WGS84.shp")
# plot(pipeline, axes=TRUE)

# Create Basemap ----
# Map for top left with depths labelled
au.map.TL<-ggplot()+
  geom_polygon(data = OZ, aes(x=long, y = lat, group = group),col="black",fill="white",size=0.4)+
  geom_polygon(data = m50, aes(x=long, y = lat, group = group),col="white",fill=NA,size=0.4)+
  annotate("text", x = 115.083, y = -21.167, angle=40, size=2.5,label = "20 m",col="white",family="serif")+
  annotate("text", x = 115.00, y = -21.19, angle=54, size=2.5,label = "50 m",col="white",family="serif")+
  geom_polygon(data = m100, aes(x=long, y = lat, group = group),col="white",fill=NA,size=0.4)+
  annotate("text", x = 114.388, y = -21.485, angle=25, size=2.5,label = "100 m",col="white",family="serif")+
  annotate("text", x = 114.38, y = -21.30, angle=62, size=2.5,label = "200 m",col="white",family="serif")+
  geom_path(data = pipeline, aes(x=long, y = lat, group = group),col="grey30",linetype=3,linejoin="round")+
  # geom_point(data=maxn,aes(Longitude,Latitude),shape=4,size=2,col="black",fill="black")+xlab('Longitude')+ylab('Latitude')+
  Theme1+
  xlab('Longitude')+
  ylab('Latitude')+
  # coord_fixed(1.5)+
  coord_cartesian(xlim=c(114.39,115.23),ylim=c(-21.75,-21.17))+
  scale_y_continuous(breaks=c(-21.7,-21.6,-21.5,-21.4,-21.3,-21.2))

#General Map
au.map<-ggplot()+
  geom_polygon(data = OZ, aes(x=long, y = lat, group = group),col="black",fill="white",size = 0.6)+
  geom_polygon(data = m50, aes(x=long, y = lat, group = group),col="white",fill=NA)+
  # annotate("text", x = 114.39, y = -21.605, angle=25, size=4,label = "20 m",col="lightgrey",family="TN")+
  # annotate("text", x = 114.39, y = -21.575, angle=10, size=4,label = "50 m",col="lightgrey",family="TN")+
  geom_polygon(data = m100, aes(x=long, y = lat, group = group),col="white",fill=NA)+
  # annotate("text", x = 114.388, y = -21.493, angle=25, size=4,label = "100 m",col="lightgrey",family="TN")+
  # annotate("text", x = 114.38, y = -21.31, angle=62, size=4,label = "200 m",col="lightgrey",family="TN")+
  geom_path(data = pipeline, aes(x=long, y = lat, group = group),col="grey30",linetype=3,linejoin="round")+
  # geom_point(data=maxn,aes(Longitude,Latitude),shape=4,size=2,col="black",fill="black")+xlab('Longitude')+ylab('Latitude')+
  Theme1+
  xlab('Longitude')+
  ylab('Latitude')+
  # coord_fixed(1.5)+
  coord_cartesian(xlim=c(114.39,115.23),ylim=c(-21.75,-21.17))+
  scale_y_continuous(breaks=c(-21.7,-21.6,-21.5,-21.4,-21.3,-21.2))
 #au.map

# Read in Fish Pics ----
setwd(fish.pics)
dir()

pm <- readPNG("Pristipomoides multidens nb.png")
pm <- as.raster(pm)
lm <- readPNG("Lutjanus malabaricus nb.png")
lm <- as.raster(lm)
lr <- readPNG("Lutjanus russellii nb.png") 
lr <- as.raster(lr)
sd <- readPNG("Seriola dumerili nb.png") 
sd <- as.raster(sd)
ns <- readPNG("Nemipterus  spp nb dark.png") 
ns <- as.raster(ns)
cch <- readPNG("Carangoides_chrysophrys_nb_c_BORNT.png") 
cch <- as.raster(cch)
ls <- readPNG("Lutjanus_sebae.png") 
ls <- as.raster(ls)
as <- readPNG("Argyrops_spinifer_nb_BORNT_s_d.png") 
as <- as.raster(as)
cca <- readPNG("Carangoies_caeruleopinnatus_nb_c_BORNT.png") 
cca <- as.raster(cca)
su <- readPNG("Saurida_undosquamis_nb_BORNT_sm.png") 
su <- as.raster(su)
ll <- readPNG("Lagocephalus_lunaris_nb_c_BORNT.png") 
ll <- as.raster(ll)
gb <- readPNG("glaucosoma_buergeri_nb.png") 
gb <- as.raster(gb)
pi <- readPNG("Parupeneus indicus nb.png") 
pi <- as.raster(pi)
sd <- readPNG("Seriola dumerili nb.png") 
sd <- as.raster(sd)
ss <- readPNG("Scomberomorus spp nb.png") 
ss <- as.raster(ss)
cc <- readPNG("Carangoides_chrysophrys_nb_BORNT.png") 
cc <- as.raster(cc)
ps <- readPNG("Plectropomus nb.png") 
ps <- as.raster(ps)
le <- readPNG("Lutjanus erythropterus nb.png") 
le <- as.raster(le)
lv <- readPNG("Lutjanus vitta nb.png") 
lv <- as.raster(lv)
ln <- readPNG("Lethrinus nebulosus nb.png") 
ln <- as.raster(ln)
lp <- readPNG("Lethrinus punctulatus nb.png") 
lp <- as.raster(lp)
em <- readPNG("epinephelus_multinotatus_nb.png") 
em <- as.raster(em)
gl <- readPNG("Lethrinus nb.png") 
gl <- as.raster(gl)
lp <- readPNG("Lethrinus punctulatus nb.png") 
lp <- as.raster(lp)
ld <- readPNG("Labroides dimidiatus nb pale.png") 
ld <- as.raster(ld)
sm <- readPNG("Scolopsis monogramma_nb.png") 
sm <- as.raster(sm)
pe <- readPNG("Pentapodus emeryii_nb.png") 
pe <- as.raster(pe)
pp <- readPNG("Pentapodus porosus nb.png") 
pp <- as.raster(pp)
sf <- readPNG("sufflamen_fraenatum_nb.png") 
sf <- as.raster(sf)
sl <- readPNG("Slearoides_leptolepis_TAYLOR_nb.png") 
sl <- as.raster(sl)
tj <- readPNG("Terapon_jarbua_TAYLOR_nb.png") 
tj <- as.raster(tj)
ds <- readPNG("Decapterus_TAYLOR_nb.png") 
ds <- as.raster(ds)
am <- readPNG("Atule_mate_TAYLOR_nb.png") 
am <- as.raster(am)
gg <- readPNG("Gymnocranius_grandoculis_TAYLOR_nb.png") 
gg <- as.raster(gg)
nt <- readPNG("Netuma_thalassina_TAYLOR_nb.png") 
nt <- as.raster(nt)
gf <- readPNG("Goat fish l nb.png") 
gf <- as.raster(gf)

# Create required maps ----
# Figure 3-----
setwd(plots.spatial)

name <- "Labroides dimidiatus"

spatial.L.dim <- au.map.TL+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance==0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue2",fill=NA,alpha=0.5)+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance>0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.5)+
  scale_size_continuous(range = c(1,8))+#, breaks=seq(0,10, by=2))+
  guides(size = guide_legend(override.aes = list(fill = c(NA,"dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),colour = c("dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),alpha=0.75),title="Abundance"))+
  # annotation_raster(ld, xmin=114.36, xmax=114.52, ymin=-21.235, ymax=-21.18)+
  Theme1+
  Theme.x.axis.blank+
  # Theme.y.axis.blank+
  ggtitle(name)
  # annotate("text",x=114.365, y=-21.165,label=name,color="Black",hjust=0,vjust=0,fontface="italic",family="TN",size = 5)

# spatial.L.dim
# ggsave(spatial.L.dim,file=paste(Sys.Date(),study, name, "ggplot.spatial.png",sep = "_"), width = 25, height = 18,units = "cm")

name <- "Nemipterus spp"

spatial.N.spp <- au.map+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance==0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue2",fill=NA,alpha=0.5)+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance>0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.5)+
  scale_size_continuous(range = c(1,8),breaks=seq(0,20, by=5))+
  guides(size = guide_legend(override.aes = list(fill = c(NA,"dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),colour = c("dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),alpha=0.75),title="Abundance"))+
  # (annotation_raster)(ns, xmin=114.36, xmax=114.54, ymin=-21.27, ymax=-21.17)+
  Theme1+
  Theme.x.axis.blank+
  Theme.y.axis.blank+
  ggtitle(name)
  # annotate("text",x=114.36, y=-21.16,label=name,color="Black",hjust=0,vjust=0,fontface="italic",family="TN",size = 5)


# ggsave(spatial.N.spp,file=paste(Sys.Date(),study, name, "ggplot.spatial.png",sep = "_"), width = 25, height = 18,units = "cm")

name<-"Pentapodus emeryii"

spatial.P.eme <- au.map+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance==0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue2",fill=NA,alpha=0.5)+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance>0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.5)+
  scale_size_continuous(range = c(1,8), breaks=seq(0,10, by=2))+
  guides(size = guide_legend(override.aes = list(fill = c(NA,"dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),colour = c("dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),alpha=0.75),title="Abundance"))+
  # (annotation_raster)(pe, xmin=114.36, xmax=114.54, ymin=-21.24, ymax=-21.18)+
  Theme1+
  Theme.x.axis.blank+
  # Theme.y.axis.blank+
  ggtitle(name)
  # annotate("text",x=114.36, y=-21.16,label=name,color="Black",hjust=0,vjust=0,fontface="italic",family="TN",size = 5)


# ggsave(spatial.P.eme,file=paste(Sys.Date(),study, name, "ggplot.spatial.png",sep = "_"), width = 25, height = 18,units = "cm")

name<-"Pentapodus porosus"

spatial.P.por <- au.map+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance==0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue2",fill=NA,alpha=0.5)+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance>0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.5)+
  scale_size_continuous(range = c(1,8))+
  guides(size = guide_legend(override.aes = list(fill = c(NA,"dodgerblue2","dodgerblue2","dodgerblue2"),colour = c("dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),alpha=0.75),title="Abundance"))+
  # (annotation_raster)(pp, xmin=114.36, xmax=114.54, ymin=-21.24, ymax=-21.18)+
  Theme1+
  Theme.x.axis.blank+
  Theme.y.axis.blank+
  ggtitle(name)
  # annotate("text",x=114.36, y=-21.16,label=name,color="Black",hjust=0,vjust=0,fontface="italic",family="TN",size = 5)


# ggsave(spatial.P.por,file=paste(Sys.Date(),study, name, "ggplot.spatial.png",sep = "_"), width = 25, height = 18,units = "cm")


name<-"Scolopsis monogramma"

spatial.S.mon <- au.map+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance==0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue2",fill=NA,alpha=0.5)+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance>0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.5)+
  scale_size_continuous(range = c(1,8), breaks=seq(0,10, by=2))+
  guides(size = guide_legend(override.aes = list(fill = c(NA,"dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),colour = c("dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),alpha=0.75),title="Abundance"))+
  # (annotation_raster)(sm, xmin=114.36, xmax=114.52, ymin=-21.24, ymax=-21.18)+
  Theme1+
  Theme.x.axis.blank+
  # Theme.y.axis.blank+
  ggtitle(name)
  # annotate("text",x=114.36, y=-21.16,label=name,color="Black",hjust=0,vjust=0,fontface="italic",family="TN",size = 5)


# ggsave(spatial.S.mon,file=paste(Sys.Date(),study, name, "ggplot.spatial.png",sep = "_"), width = 25, height = 18,units = "cm")

name<-"Selaroides leptolepis"

spatial.S.lep <- au.map+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance==0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue2",fill=NA,alpha=0.5)+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance>0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.5)+
  scale_size_continuous(range = c(1,8))+
  guides(size = guide_legend(override.aes = list(fill = c(NA,"dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),colour = c("dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),alpha=0.75),title="Abundance"))+
  # (annotation_raster)(sl, xmin=114.36, xmax=114.54, ymin=-21.24, ymax=-21.18)+
  Theme1+
  #Theme.x.axis.blank+
  Theme.y.axis.blank+
  ggtitle(name)
  # annotate("text",x=114.36, y=-21.16,label=name,color="Black",hjust=0,vjust=0,fontface="italic",family="TN",size = 5)


# ggsave(spatial.S.lep,file=paste(Sys.Date(),study, name, "ggplot.spatial.png",sep = "_"), width = 25, height = 18,units = "cm")

name<-"Sufflamen fraenatum"

spatial.S.fre <- au.map+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance==0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue2",fill=NA,alpha=0.5)+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance>0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.5)+
  scale_size_continuous(range = c(1,8))+
  guides(size = guide_legend(override.aes = list(fill = c(NA,"dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),colour = c("dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),alpha=0.75),title="Abundance"))+
  # (annotation_raster)(sf, xmin=114.36, xmax=114.50, ymin=-21.25, ymax=-21.18)+
  Theme1+
  #Theme.x.axis.blank+
  # Theme.y.axis.blank+
  ggtitle(name)
  # annotate("text",x=114.36, y=-21.16,label=name,color="Black",hjust=0,vjust=0,fontface="italic",family="TN",size = 5)
# spatial.S.fre

# ggsave(spatial.S.fre,file=paste(Sys.Date(),study, name, "ggplot.spatial.png",sep = "_"), width = 25, height = 18,units = "cm")

fig3<-plot_grid(spatial.L.dim,spatial.N.spp,spatial.P.eme,spatial.P.por,spatial.S.mon,spatial.S.lep,spatial.S.fre,ncol=2,align="vh")+
  (annotation_raster)(ld,xmin=0.28, xmax=0.38, ymin=0.975, ymax=0.995)+
  (annotation_raster)(ns,xmin=0.78, xmax=0.88, ymin=0.97, ymax=1.00)+
  (annotation_raster)(pe,xmin=0.275, xmax=0.38, ymin=0.723, ymax=0.75)+
  
  (annotation_raster)(pp,xmin=0.775, xmax=0.88, ymin=0.72, ymax=0.755)+
  (annotation_raster)(sm,xmin=0.279, xmax=0.382, ymin=0.473, ymax=0.505)+
  
  (annotation_raster)(sl,xmin=0.775, xmax=0.88, ymin=0.466, ymax=0.511)+
  (annotation_raster)(sf,xmin=0.281, xmax=0.377, ymin=0.223, ymax=0.257)

fig3
dev.off()

warnings()

ggsave(fig3,file=paste(Sys.Date(),"ll.gg.fig3_long.png",sep="_"),width=20,height=24,units="cm") # need to fix axis labels




# Figure 5 ----

name <- "Argyrops spinifer"

spatial.A.spi <- au.map.TL+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance==0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue2",fill=NA,alpha=0.5)+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance>0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.5)+
  scale_size_continuous(range = c(1,8))+#, breaks=seq(0,10, by=2))+
  guides(size = guide_legend(override.aes = list(fill = c(NA,"dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),colour = c("dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),alpha=0.75),title="Abundance"))+
  # annotation_raster(as, xmin=114.36, xmax=114.52, ymin=-21.235, ymax=-21.18)+
  Theme1+
  Theme.x.axis.blank+
  # Theme.y.axis.blank+
  ggtitle(name)
# annotate("text",x=114.365, y=-21.165,label=name,color="Black",hjust=0,vjust=0,fontface="italic",family="TN",size = 5)

# spatial.A.spi
# ggsave(spatial.A.spi,file=paste(Sys.Date(),study, name, "ggplot.spatial.png",sep = "_"), width = 25, height = 18,units = "cm")

name <- "Atule mate"

spatial.A.mat <- au.map+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance==0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue2",fill=NA,alpha=0.5)+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance>0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.5)+
  scale_size_continuous(range = c(1,8))+#,breaks=seq(0,20, by=5))+
  guides(size = guide_legend(override.aes = list(fill = c(NA,"dodgerblue2","dodgerblue2","dodgerblue2"),colour = c("dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),alpha=0.75),title="Abundance"))+
  # (annotation_raster)(am, xmin=114.36, xmax=114.54, ymin=-21.27, ymax=-21.17)+
  Theme1+
  Theme.x.axis.blank+
  Theme.y.axis.blank+
  ggtitle(name)
# annotate("text",x=114.36, y=-21.16,label=name,color="Black",hjust=0,vjust=0,fontface="italic",family="TN",size = 5)


# ggsave(spatial.A.mat,file=paste(Sys.Date(),study, name, "ggplot.spatial.png",sep = "_"), width = 25, height = 18,units = "cm")

name<-"Carangoides chrysophrys"

spatial.C.chr <- au.map+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance==0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue2",fill=NA,alpha=0.5)+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance>0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.5)+
  scale_size_continuous(range = c(1,8))+#, breaks=seq(0,10, by=2))+
  guides(size = guide_legend(override.aes = list(fill = c(NA,"dodgerblue2","dodgerblue2","dodgerblue2"),colour = c("dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),alpha=0.75),title="Abundance"))+
  # (annotation_raster)(cc, xmin=114.36, xmax=114.54, ymin=-21.24, ymax=-21.18)+
  Theme1+
  Theme.x.axis.blank+
  # Theme.y.axis.blank+
  ggtitle(name)
# annotate("text",x=114.36, y=-21.16,label=name,color="Black",hjust=0,vjust=0,fontface="italic",family="TN",size = 5)

# ggsave(spatial.C.chr,file=paste(Sys.Date(),study, name, "ggplot.spatial.png",sep = "_"), width = 25, height = 18,units = "cm")

name<-"Nemipterus spp"

spatial.N.spp <- au.map+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance==0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue2",fill=NA,alpha=0.5)+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance>0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.5)+
  scale_size_continuous(range = c(1,8),breaks=seq(0,20, by=5))+
  guides(size = guide_legend(override.aes = list(fill = c(NA,"dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),colour = c("dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),alpha=0.75),title="Abundance"))+
  # (annotation_raster)(ns, xmin=114.36, xmax=114.54, ymin=-21.27, ymax=-21.17)+
  Theme1+
  Theme.x.axis.blank+
  Theme.y.axis.blank+
  ggtitle(name)
# annotate("text",x=114.36, y=-21.16,label=name,color="Black",hjust=0,vjust=0,fontface="italic",family="TN",size = 5)

# ggsave(spatial.N.spp,file=paste(Sys.Date(),study, name, "ggplot.spatial.png",sep = "_"), width = 25, height = 18,units = "cm")

name<-"Pristipomoides multidens"

spatial.P.mul <- au.map+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance==0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue2",fill=NA,alpha=0.5)+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance>0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.5)+
  scale_size_continuous(range = c(1,8),breaks=seq(0,8, by=2))+
  guides(size = guide_legend(override.aes = list(fill = c(NA,"dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),colour = c("dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),alpha=0.75),title="Abundance"))+
  # (annotation_raster)(pm, xmin=114.36, xmax=114.54, ymin=-21.27, ymax=-21.17)+
  Theme1+
  # Theme.y.axis.blank+
  ggtitle(name)
# annotate("text",x=114.36, y=-21.16,label=name,color="Black",hjust=0,vjust=0,fontface="italic",family="TN",size = 5)

# ggsave(spatial.P.mul,file=paste(Sys.Date(),study, name, "ggplot.spatial.png",sep = "_"), width = 25, height = 18,units = "cm")

name<-"Terapon jarbua"

spatial.T.jar <- au.map+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance==0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue2",fill=NA,alpha=0.5)+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance>0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.5)+
  scale_size_continuous(range = c(1,8))+#,breaks=seq(0,20, by=5))+
  guides(size = guide_legend(override.aes = list(fill = c(NA,"dodgerblue2","dodgerblue2","dodgerblue2"),colour = c("dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),alpha=0.75),title="Abundance"))+
  # (annotation_raster)(tj, xmin=114.36, xmax=114.54, ymin=-21.27, ymax=-21.17)+
  Theme1+
  Theme.y.axis.blank+
  ggtitle(name)
# annotate("text",x=114.36, y=-21.16,label=name,color="Black",hjust=0,vjust=0,fontface="italic",family="TN",size = 5)

# ggsave(spatial.N.spp,file=paste(Sys.Date(),study, name, "ggplot.spatial.png",sep = "_"), width = 25, height = 18,units = "cm")
fig5<-plot_grid(spatial.A.spi,spatial.A.mat,spatial.C.chr,spatial.N.spp,spatial.P.mul,spatial.T.jar,ncol=2,align="vh")+
  (annotation_raster)(as,xmin=0.31, xmax=0.38, ymin=0.9625, ymax=1.00)+
  (annotation_raster)(am,xmin=0.78, xmax=0.89, ymin=0.9625, ymax=1.00)+
  (annotation_raster)(cc,xmin=0.31, xmax=0.40, ymin=0.63, ymax=0.67)+
  (annotation_raster)(ns,xmin=0.775, xmax=0.88, ymin=0.63, ymax=0.67)+
  (annotation_raster)(pm,xmin=0.30, xmax=0.41, ymin=0.295, ymax=0.34)+
  (annotation_raster)(tj,xmin=0.775, xmax=0.89, ymin=0.295, ymax=0.345)

ggsave(fig5,file=paste(Sys.Date(),"ll.gg.fig5.png",sep="_"),width=19,height=18,units="cm") # need to fix axis labels




#fig 7 ----

name <- "Decapterus sp1"

spatial.D.sp1 <- au.map+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance==0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue2",fill=NA,alpha=0.5)+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance>0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.5)+
  scale_size_continuous(range = c(1,8),breaks=seq(0,60,by=20))+
  guides(size = guide_legend(override.aes = list(fill = c(NA,"dodgerblue2","dodgerblue2","dodgerblue2"),colour = c("dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),alpha=0.75),title="Abundance"))+
  # (annotation_raster)(ds, xmin=114.36, xmax=114.54, ymin=-21.27, ymax=-21.17)+
  Theme1+
  Theme.x.axis.blank+
  ggtitle(name)
# annotate("text",x=114.36, y=-21.16,label=name,color="Black",hjust=0,vjust=0,fontface="italic",family="TN",size = 5)


# ggsave(spatial.D.sp1,file=paste(Sys.Date(),study, name, "ggplot.spatial.png",sep = "_"), width = 25, height = 18,units = "cm")

name<-"Lutjanus malabaricus"

spatial.L.mal <- au.map+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance==0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue2",fill=NA,alpha=0.5)+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance>0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.5)+
  scale_size_continuous(range = c(1,8))+#, breaks=seq(0,10, by=2))+
  guides(size = guide_legend(override.aes = list(fill = c(NA,"dodgerblue2","dodgerblue2","dodgerblue2"),colour = c("dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),alpha=0.75),title="Abundance"))+
  # (annotation_raster)(lm, xmin=114.36, xmax=114.54, ymin=-21.24, ymax=-21.18)+
  Theme1+
  Theme.x.axis.blank+
  Theme.y.axis.blank+
  ggtitle(name)
# annotate("text",x=114.36, y=-21.16,label=name,color="Black",hjust=0,vjust=0,fontface="italic",family="TN",size = 5)


# ggsave(spatial.L.mal,file=paste(Sys.Date(),study, name, "ggplot.spatial.png",sep = "_"), width = 25, height = 18,units = "cm")

name<-"Nemipterus spp"

spatial.N.spp <- au.map+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance==0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue2",fill=NA,alpha=0.5)+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance>0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.5)+
  scale_size_continuous(range = c(1,8),breaks=seq(0,20, by=5))+
  guides(size = guide_legend(override.aes = list(fill = c(NA,"dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),colour = c("dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),alpha=0.75),title="Abundance"))+
  # (annotation_raster)(ns, xmin=114.36, xmax=114.54, ymin=-21.27, ymax=-21.17)+
  Theme1+
  Theme.x.axis.blank+
  ggtitle(name)
# annotate("text",x=114.36, y=-21.16,label=name,color="Black",hjust=0,vjust=0,fontface="italic",family="TN",size = 5)

# ggsave(spatial.N.spp,file=paste(Sys.Date(),study, name, "ggplot.spatial.png",sep = "_"), width = 25, height = 18,units = "cm")

name<-"Pristipomoides multidens"

spatial.P.mul <- au.map+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance==0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue2",fill=NA,alpha=0.5)+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance>0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.5)+
  scale_size_continuous(range = c(1,8),breaks=seq(0,8, by=2))+
  guides(size = guide_legend(override.aes = list(fill = c(NA,"dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),colour = c("dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),alpha=0.75),title="Abundance"))+
  # (annotation_raster)(pm, xmin=114.36, xmax=114.54, ymin=-21.27, ymax=-21.17)+
  Theme1+
  Theme.x.axis.blank+
  Theme.y.axis.blank+
  ggtitle(name)
# annotate("text",x=114.36, y=-21.16,label=name,color="Black",hjust=0,vjust=0,fontface="italic",family="TN",size = 5)

# ggsave(spatial.P.mul,file=paste(Sys.Date(),study, name, "ggplot.spatial.png",sep = "_"), width = 25, height = 18,units = "cm")

name<-"Seriola dumerili"

spatial.S.dum <- au.map+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance==0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue2",fill=NA,alpha=0.5)+
  geom_point(data=filter(maxn,Genus_species%in%c(name)&Abundance>0),aes(Longitude,Latitude,size=Abundance),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.5)+
  scale_size_continuous(range = c(1,8))+#,breaks=seq(0,8, by=2))+
  guides(size = guide_legend(override.aes = list(fill = c(NA,"dodgerblue2","dodgerblue2","dodgerblue2"),colour = c("dodgerblue2","dodgerblue2","dodgerblue2","dodgerblue2"),alpha=0.75),title="Abundance"))+
  # (annotation_raster)(sd, xmin=114.36, xmax=114.54, ymin=-21.27, ymax=-21.17)+
  Theme1+
  #Theme.x.axis.blank+
  ggtitle(name)
# annotate("text",x=114.36, y=-21.16,label=name,color="Black",hjust=0,vjust=0,fontface="italic",family="TN",size = 5)

# ggsave(spatial.S.dum,file=paste(Sys.Date(),study, name, "ggplot.spatial.png",sep = "_"), width = 25, height = 18,units = "cm")

fig7<-plot_grid(spatial.D.sp1,spatial.L.mal,spatial.N.spp,spatial.P.mul,spatial.S.dum,ncol=2,align="vh")+
  (annotation_raster)(ds,xmin=0.275, xmax=0.38, ymin=0.96, ymax=1.002)+
  (annotation_raster)(lm,xmin=0.787, xmax=0.88, ymin=0.962, ymax=1.002)+
  (annotation_raster)(ns,xmin=0.275, xmax=0.38, ymin=0.627, ymax=0.675)+
  (annotation_raster)(pm,xmin=0.8, xmax=0.915, ymin=0.625, ymax=0.68)+
  (annotation_raster)(sd,xmin=0.27, xmax=0.38, ymin=0.295, ymax=0.345)

ggsave(fig7,file=paste(Sys.Date(),"gg.fig7_2_long.png",sep="_"),width=19,height=18,units="cm") # need to fix axis labels and move images

