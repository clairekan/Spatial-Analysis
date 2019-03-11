library(GISTools)
library(spdep)
library(rgdal)
library(dplyr)

#Q1
Taipei_Vill <- readOGR(dsn = ".", layer = "Taipei_Vill", encoding="utf8")
Tpe_Fastfood <- readOGR(dsn = ".", layer = "Tpe_Fastfood", encoding="utf8")
MIC<-Tpe_Fastfood[Tpe_Fastfood$STORE=="MIC",]
buf_fast <- gBuffer(MIC, width = 1000, byid = T, id = MIC$ ID )#width = 1000寬度
MIC.count <- poly.counts(MIC, buf_fast) 
head(MIC.count)
n<-nrow(data.frame(MIC.count))
max_mic<-subset(MIC,MIC$ID==names(which.max(MIC.count)))

plot(Taipei_Vill,col='lightgoldenrodyellow',lwd = 0.5, border = "grey50",xlim=c(293170.1,320206.8),
     ylim=c(2761120, 2790526))
plot(max_mic,col = "red", pch = 20,add=T)
print(max_mic$ALIAS)

locator()

#Q2
centroids_V <- gCentroid(Taipei_Vill, byid = T, id = rownames(Taipei_Vill))#gCentroid找出中心點座標
proj4string(centroids_V)<- CRS(proj4string(Taipei_Vill)) 
distances_v <- gWithinDistance(MIC, centroids_V, byid = T, dist =1000)
#buf_V <- gBuffer(centroids_V, width = 1000, byid = T, id = centroids_V$ ID )#width = 1000寬度
#MIC.count_v <- poly.counts(MIC, buf_V) 
store_number <- apply(distances_v,1, sum)#1代表由上往下算
x<-which.max(store_number)
max_V<-Taipei_Vill[x,]
#找麥當勞
distances_M <- gWithinDistance(gCentroid(max_V, byid = T),MIC, byid = T, dist =1000)
mic_number<-MIC[as.vector(distances_M),]
plot(Taipei_Vill,col='lightgoldenrodyellow',lwd = 0.5, border = "grey50",xlim=c(293170.1,320206.8),
     ylim=c(2761120, 2790526))
plot(max_V,add=T,col='whit',lwd = 0.5, border = "red",xlim=c(293170.1,320206.8),
     ylim=c(2761120, 2790526))
plot(mic_number,col = "red", pch = 20,add=T,cex = 0.3)





