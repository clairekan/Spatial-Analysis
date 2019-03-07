rm(list = ls())


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
plot(Popn_TWN2,col='lightgreen', border="gray", bg='aliceblue')
plot(air_qua, col = "blue", pch = 20, add = TRUE)
plot(air_unq, col = "red", pch = 20, add = TRUE)

#3
library(dplyr)
air_unq<-as.data.frame(air_unq)
air_unq$SiteType<-as.character(air_unq$SiteType)
airpolu_OVER<-filter(air_unq,SiteType=="一般測站"|SiteType=="工業測站"|SiteType=="交通測站")
boxplot(PSI~SiteType,data=airpolu_OVER,xlab = "SiteType",ylab = "PSI ",cex.lab=1)


Pollution_Map<-function( agr1 ){
  library(spdep)
  library(rgdal)
  library(ggplot2)
  
  airpolu <- readOGR(dsn = ".", layer = "EPA_STN1", encoding="utf8")
  Popn_TWN2 <- readOGR(dsn = ".", layer = "Popn_TWN2", encoding="utf8")
  
  #1
  return(qnorm(agr1,mean=mean(PSI),sd=sd(PSI),lower.tail = F))
  #2
  par(mfrow=c(1,2))
  head(airpolu)
  attach(as.data.frame(airpolu))
  air_qua<-subset(airpolu,PSI<=qnorm(agr1,mean=mean(PSI),sd=sd(PSI),lower.tail = F))
  air_unq<-subset(airpolu,PSI>qnorm(agr1,mean=mean(PSI),sd=sd(PSI),lower.tail = F))
  
  library(GISTools)
  plot(Popn_TWN2,col='lightgreen', border="gray", bg='aliceblue')
  plot(air_qua, col = "blue", pch = 20, add = TRUE)
  plot(air_unq, col = "red", pch = 20, add = TRUE)
  
  #3
  library(dplyr)
  air_unq<-as.data.frame(air_unq)
  air_unq$SiteType<-as.character(air_unq$SiteType)
  airpolu_OVER<-filter(air_unq,SiteType=="一般測站"|SiteType=="工業測站"|SiteType=="交通測站")
  boxplot(PSI~SiteType,data=airpolu_OVER,xlab = "SiteType",ylab = "PSI ")
  
  
}

Pollution_Map(0.3)





