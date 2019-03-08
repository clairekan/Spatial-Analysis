rm(list = ls())

head(airpolu)

library(spdep)
library(rgdal)
library(ggplot2)

airpolu <- readOGR(dsn = ".", layer = "EPA_STN1", encoding="utf8")
Popn_TWN2 <- readOGR(dsn = ".", layer = "Popn_TWN2", encoding="utf8")

#PSI機率=0.3
#1
qnorm(0.3,mean=mean(PSI),sd=sd(PSI),lower.tail = F)
#2
par(mfrow=c(1,2))
head(airpolu)
attach(as.data.frame(airpolu))

air_qua<-subset(airpolu,PSI<=qnorm(0.3,mean=mean(PSI),sd=sd(PSI),lower.tail = F))
air_unq<-subset(airpolu,PSI>qnorm(0.3,mean=mean(PSI),sd=sd(PSI),lower.tail = F))

library(GISTools)


# Map overlay
proj4string(Popn_TWN2)#確定圖層投影是否一樣
Popn_TWN2$area<- poly.areas(Popn_TWN2)/10^6#poly.areas()用來算面積
Popn_TWN2.f <- fortify(Popn_TWN2, region="TOWN")#fortify()把每個轉折點獨立成一筆資料
Popn_TWN2.f <- merge(Popn_TWN2.f, Popn_TWN2@data, by.x = "id", by.y = "TOWN")#將屬性資料接起來，把剛剛四萬多筆的資料跟原先的連結

map.0<-ggplot()+geom_polygon(data =Popn_TWN2.f, aes(x=long, y = lat, group = group),fill="gray", color="white")+xlim(0,400000)+ylim(2400000,2800000)

qua_coor_x<- air_qua@coords[,1]
qua_coor_y<- air_qua@coords[,2]
qua_point<-map.0+geom_point(aes(x=qua_coor_x,y=qua_coor_y),col="blue")+coord_fixed(1.0)

unq_coor_x<- air_unq@coords[,1]
unq_coor_y<- air_unq@coords[,2]
qua_point+geom_point(aes(x=unq_coor_x,y=unq_coor_y),col="red")

#3
library(dplyr)
air_unq<-as.data.frame(air_unq)
air_unq$SiteType<-as.character(air_unq$SiteType)
airpolu_OVER<-filter(air_unq,SiteType=="一般測站"|SiteType=="工業測站"|SiteType=="交通測站")
ggplot(airpolu_OVER,aes(x=SiteType,y=PSI))+geom_boxplot()

###############################################################################################

#Q2-1
library(GISTools)
library(rgdal)library(sp)

locator()
proj4string(Popn_TWN2)#確定圖層投影是否一樣
Popn_TWN2$area<- poly.areas(Popn_TWN2)/10^6#poly.areas()用來算面積
Popn_TWN2$density<- (Popn_TWN2$A0A14_CNT+ Popn_TWN2$A15A64_CNT+Popn_TWN2$A65UP_CNT)/ Popn_TWN2$area#新開一個欄位
#畫面量圖
choropleth(Popn_TWN2, Popn_TWN2$density,xlim=c(-73217.35,525151.00),ylim=c(2386326,2929339))#畫面量圖，什麼圖層、什麼欄位
vacant.shades = auto.shading(Popn_TWN2$density,n=6)#切n層
choro.legend(371786.94,2701332,vacant.shades,cex=0.6)
north.arrow(13392.28, 2597371, "N", len=8000, col="light gray")
map.scale(35288.13,2482789,100000,"100 km", 1, subdiv=1, tcol='black',scol='gray', sfcol='black')

#Q2-2
Popn_TWN2$old_rate<-Popn_TWN2$A65UP_CNT/(Popn_TWN2$A0A14_CNT+ Popn_TWN2$A15A64_CNT+Popn_TWN2$A65UP_CNT)
Popn_TWN2_d<-as.data.frame(Popn_TWN2)
Popn_TWN2_d<-filter(Popn_TWN2_d,Popn_TWN2_d$COUNTY=='臺北市'|Popn_TWN2_d$COUNTY=='新北市'|Popn_TWN2_d$COUNTY=='宜蘭縣'|Popn_TWN2_d$COUNTY=='桃園市'|Popn_TWN2_d$COUNTY=='基隆市')
Popn_TWN2_d<-arrange(Popn_TWN2_d,desc(Popn_TWN2_d$old_rate))
Popn_TWN2_d[1:15,]
big_taipei<-subset(Popn_TWN2,Popn_TWN2$COUNTY=='臺北市'|Popn_TWN2$COUNTY=='新北市'|Popn_TWN2$COUNTY=='宜蘭縣'|Popn_TWN2$COUNTY=='桃園市'|Popn_TWN2$COUNTY=='基隆市')
big_taipei$COUNTY<-as.character(big_taipei$COUNTY)
old_enough<-subset(big_taipei,big_taipei$old_rate>=0.1737619)#把前20%選出來了


proj4string(Popn.TWN)
TWN.LongLat <- spTransform(Popn.TWN, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
bbox(TWN.LongLat)#找出範圍最大最小直
bbox(TWN.LongLat)[2,2]
library(OpenStreetMap)
### OpenStreepMap ###
# define upper left, lower right corners
ul <- c(25.92151,120.1624) # lat-long coord
lr <- c(23.76910,122.9044)# lat-long coord
# download the map tile
MyMap <- openmap(ul,lr,9, "esri-topo")#把給出的左上右下座標的端點找到並把範圍的圖割下來，9代表我要第九層的圖磚，越底層越豐富的資訊
# now plot the layer and the backdrop
par(mar = c(0,0,0,0))
plot(MyMap, removeMargin=FALSE) 

old_enough1 <- spTransform(old_enough, osm())
plot(old_enough1, col =rgb(1,0,0,0.6), add = TRUE)

locator()


#Q3

Popn_TWN2_d1<-select(Popn_TWN2_d,TOWN,density,old_rate)
for(i in 1:length(Popn_TWN2_d1$TOWN)){
  if(Popn_TWN2_d1$density[i]>=10000){
    Popn_TWN2_d1$level[i]<-"high"
  }
  else if(Popn_TWN2_d1$density[i]<10000&Popn_TWN2_d1$density[i]>2000){
    Popn_TWN2_d1$level[i]="medium"
  }
  else if(Popn_TWN2_d1$density[i]<=2000){
    Popn_TWN2_d1$level[i]="low"
  }
}
Popn_TWN2_d2<-filter(Popn_TWN2_d1,level=='low'|level=='high')
#畫圖
Popn_TWN2_d2$level<-as.factor(Popn_TWN2_d2$level)
ggplot(Popn_TWN2_d2,aes(x=level,y=old_rate))+geom_boxplot()


