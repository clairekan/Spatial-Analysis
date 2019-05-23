rm(list = ls())
library(spdep)
library(rgdal)
TWPOP <- readOGR(dsn = '.', layer = "Popn_TWN2", encoding="utf8")

head(TWPOP@data)
TWTable<-TWPOP@data

#Moran’s I coefficient
TWN_nb<-poly2nb(TWPOP) #QUEEN = TRUE，紀錄每個polygon的鄰居數
TWN_nb_w<- nb2listw(TWN_nb, zero.policy=T) # default: style = "W" #列標準化
Popn<-TWTable$A0A14_CNT+TWTable$A15A64_CNT+TWTable$A65UP_CNT
Popn<-as.numeric(paste(Popn))
M<-moran.test(Popn, listw=TWN_nb_w, zero.policy=T); M  #人口資料，listw:空間權重，zero:如果有孤立點的話怎麼半

#很顯著的正相關

#Monte-Carlo simulation
bperm<-moran.mc(Popn,listw=TWN_nb_w,nsim=999, zero.policy=T)#做999次的排列檢定(拆開重新亂排排列)
hist(bperm$res, freq=TRUE, breaks=20, xlab="Simulated Moran's I")
abline(v=0.442, col="red")#顯示統計顯著，真實狀況不是亂排的
#很顯著的正相關

#Moran scatter plot
moran.plot (Popn, TWN_nb_w, zero.policy=T)

# Correlogram
cor<-sp.correlogram(TWN_nb, Popn, order=10, method="I", style="W",zero.policy=T)#第1,2,3階鄰居
print(cor); plot(cor)




#Moran’s I coefficient
#相接相鄰
TWN_nb<-poly2nb(TWPOP) #QUEEN = TRUE，紀錄每個polygon的鄰居數
TWN_nb_w<- nb2listw(TWN_nb, zero.policy=T) # default: style = "W" #列標準化
Popn<-TWTable$A0A14_CNT+TWTable$A15A64_CNT+TWTable$A65UP_CNT
Popn<-as.numeric(paste(Popn))
M<-moran.test(Popn, listw=TWN_nb_w, zero.policy=T); M  #人口資料，listw:空間權重，zero:如果有孤立點的話怎麼半


#K
IDs <-TWTable$TOWN_ID
coords<-coordinates(TWPOP)#產生polygon中心點座標
TWN_kn1<-knn2nb(knearneigh(coords, k=2), row.names=IDs)#找最近的兩個
TWN_kn1_w<- nb2listw(TWN_kn1, zero.policy=T) # default: style = "W" #列標準化
M_k<-moran.test(Popn, listw=TWN_kn1_w, zero.policy=T); M_k  #人口資料，listw:空間權重，zero:如果有孤立點的話怎麼半

#Distance-based
TWN_ran1<-dnearneigh(coords, d1=0, d2=20000, row.names=IDs)#d1:中心點，d2:範圍
TWN_ran1_w<- nb2listw(TWN_ran1, zero.policy=T) # default: style = "W" #列標準化
M_d<-moran.test(Popn, listw=TWN_ran1_w, zero.policy=T); M_d  #人口資料，listw:空間權重，zero:如果有孤立點的話怎麼半



head(TWTable)













