rm(list = ls())
library(spdep)
library(rgdal)
library(ggplot2)
library(GISTools)

TWPOP <- readOGR(dsn = '.', layer = "Popn_TWN2", encoding="utf8")
plot(TWPOP)
head(TWPOP@data)
TWTable<-TWPOP@data
TWPOP@data$pop<-TWTable$A0A14_CNT+TWTable$A15A64_CNT+TWTable$A65UP_CNT
TWPOP@data$density<-TWPOP@data$pop/poly.areas(TWPOP)
#1
TWN_nb<-poly2nb(TWPOP)
summary(TWN_nb)
number<-c(0:11)
count<-c(11,4,10,29,45,96,90,52,22,6,2,1)
nei_count<-data.frame(number,count)
ggplot(nei_count,aes(x=number,y=count))+geom_bar(stat = 'identity')

#2
TWTable[230,]

#3
coords<-coordinates(TWPOP)#產生polygon中心點座標
IDs <-TWPOP@data$TOWN_ID
TWN_kn1<-knn2nb(knearneigh(coords, k=1), row.names=IDs)#找最近的兩個
neighbor<-c(1:368)
for(i in 1:368){
  neighbor[i]<-TWPOP@data[unlist(TWN_kn1[i]),16]
}
TWPOP@data$neighbor<-neighbor

shades = auto.shading(TWPOP@data$neighbor, n = 5, cols = brewer.pal(5, "Greens")) 
choropleth(TWPOP, TWPOP@data$neighbor, shades,xlim=c(97131.72,327759.52),ylim = c(2409189,2818715)) 

nrow(TWTable)









