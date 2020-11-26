dir()

setwd("~/Github/Analysis-Penman-Geographe/Spatial")
labsheet<-read.csv("MEG_Labsheets_2020 - 2014-12_Geographe.Bay_stereoBRUVs.csv")%>%
  ga.clean.names()%>%
  dplyr::select(-c(latitude,longitude))

setwd("~/Github/Analysis-Penman-Geographe/Tidy data/Curtin")
dir()
metadata<-read.csv("2014-12_Geographe.Bay_stereoBRUVs.checked.metadata.csv")%>%
  dplyr::select(sample,latitude,longitude)

new.data <-left_join(labsheet,metadata)%>%
  filter(!is.na(latitude))

setwd("~/Github/Analysis-Penman-Geographe/Spatial")
write.csv(new.data,"analysed.videos.csv")
