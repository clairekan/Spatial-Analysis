library(GISTools)
library(spdep)
library(rgdal)
library(dplyr)
library(ggplot2)

Taipei_Vill <- readOGR(dsn = ".", layer = "Taipei_Vill", encoding="utf8")
Taipei_Vill$CENSUS<-as.numeric(as.character(Taipei_Vill@data$CENSUS))
flood <- readOGR(dsn = ".", layer = "flood50", encoding="utf8")
flood <- spTransform(flood,Taipei_Vill@proj4string )

Overlay_flood_popu <- gIntersection(Taipei_Vill, flood, byid = T)#面跟面的交集，資料格是為spatialpolypon
tmp_1 <- strsplit(names(Overlay_flood_popu), " ") #strsplit 前面放要切的，後面放要切的根據(空格)
tp_ID<-unlist(lapply(tmp_1, function(x) x[1]))
tp_ID<-as.numeric(tp_ID)
fd_ID<-unlist(lapply(tmp_1, function(x) x[2]))
fd_ID<-as.numeric(fd_ID)

df_1=data.frame(tp_ID, fd_ID )
Taipei_Vill$AREA<-poly.areas(Taipei_Vill)

for (i in 1:5705) {
  df_1$area_vili[i] <- Taipei_Vill$AREA[as.numeric(tp_ID[i])+1] #因為之前的資料rowname有0
  df_1$popu[i] <- Taipei_Vill$CENSUS[as.numeric(tp_ID[i])+1] #area1是當面積分母用的
  df_1$level[i]<-flood@data$grid_code[as.numeric(fd_ID[i])+1]
}
Overlay_flood_popuNew<- SpatialPolygonsDataFrame(Overlay_flood_popu, data=df_1,match.ID = F)#把df當屬性資料表加上去，並轉呈空間資料
Overlay_flood_popuNew$area_QQ<- poly.areas(Overlay_flood_popuNew)
area_1 <- Overlay_flood_popuNew$area_vili
area_2 <- Overlay_flood_popuNew$area_QQ
popu1<- Overlay_flood_popuNew$popu
Overlay_flood_popuNew$popu_pct<- as.integer ((area_2/area_1) * popu1)

popu_pct<-Overlay_flood_popuNew$popu_pct
fd_level<-Overlay_flood_popuNew$level

fd.popu<- xtabs(popu_pct ~ fd_level )#按照gridlayer.id的ID加起來
flood_harm<-data.frame(fd.popu)
flood_harm
