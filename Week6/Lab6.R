library(rgdal)
library(aspace)
library(GISTools)
library(spdep)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(RColorBrewer)#畫圖畫顏色的套件
library(classInt)

event <- readOGR(dsn = ".", layer = "point_event", encoding="utf8")
event.attr<-data.frame(event)
ggplot(event.attr,aes(x=event.attr$WEEK))+geom_bar()+scale_x_continuous(breaks=seq(0, 53, 5))+xlab("Week")+labs(title="By Week")

for (i in 1:nrow(event.attr)){
  if (event.attr$WEEK[i]%in%c(1:7)){
    event.attr$period[i]<-1
  }else if (event.attr$WEEK[i]%in%c(8:15)){
    event.attr$period[i]<-2
  }else if (event.attr$WEEK[i]%in%c(16:23)){
    event.attr$period[i]<-3
  }else if (event.attr$WEEK[i]%in%c(24:31)){
    event.attr$period[i]<-4
  }else if (event.attr$WEEK[i]%in%c(32:39)){
    event.attr$period[i]<-5
  }else if (event.attr$WEEK[i]%in%c(40:47)){
    event.attr$period[i]<-6
  }else {
    event.attr$period[i]<-7
  }
}
ggplot(event.attr,aes(x=event.attr$period))+geom_bar()+xlab("Period")+labs(title="Period")


#Q2
EVENT<-SpatialPointsDataFrame(cbind(event$X,event$Y),event.attr,proj4string=CRS("+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))
TWN<- readOGR(dsn = ".", layer = "Popn_TWN2", encoding=utf8)
kao<-subset(TWN,COUNTY=="高雄市")

Overlay <- gIntersection(EVENT, kao, byid = T) #byid = TRUE 把ID留下來
tmp_1 <- strsplit(rownames(data.frame(Overlay))
, " ") #strsplit 前面放要切的，後面放要切的根據(空格)
EVENT_ID<-unlist(lapply(tmp_1, function(x) x[1]))
EVENT_ID<-as.numeric(EVENT_ID)
kao_ID<-unlist(lapply(tmp_1, function(x) x[2]))
kao_ID<-as.numeric(kao_ID)

df_1=data.frame(EVENT_ID, kao_ID)
n<-nrow(df_1)
for (i in 1:n) {
  df_1$type[i] <- event.attr$period[as.numeric(EVENT_ID[i])] #因為之前的資料rowname有0
  df_1$coord.x[i] <- event.attr$coords.x1[as.numeric(EVENT_ID[i])] #因為之前的資料rowname有0
  df_1$coord.y[i] <- event.attr$coords.x2[as.numeric(EVENT_ID[i])] #因為之前的資料rowname有0
  
  
}
plot(kao,xlim=c(165899.0,192266.9),ylim=c(2491747, 2527843))
color<-c('red','orange','gold','green','cyan','dodgerblue3','darkorchid3')

for(i in 1:7){
koa.1<-subset(df_1,type==i)
koa.1_SDD<- calc_sdd(id=1, points=koa.1[,4:5])
plot_sdd(plotnew=F, plotcentre=T,centre.col=color[i],
         sdd.col=color[i],sdd.lwd=1,titletxt="", plotpoints=F,points.col="black")

}










locator()
class(df_1$coord.x)
Overlay@data
class(event.attr$period)
proj4string(TWN)
plot(Overlay.N)
head(koa.1@data)
rownames(data.frame(Overlay))
#記得把xy座標改成numeric