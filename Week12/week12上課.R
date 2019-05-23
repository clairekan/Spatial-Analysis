#########################################################
#                                                       #
# Spatial Analysis (Geog 2017; NTU Geography)           #
#                                                       #
# Lecture 10: Spatial Autocorrelation                   #
#                                                       #
# Instructor: Dr. Tzai-Hung Wen                         #
#                                                       #
#                                                       #
# Date: 2019-05-20                                      #
#                                                       #
#########################################################


rm(list = ls())
library(spdep)
library(rgdal)
TWPOP <- readOGR(dsn = '.', layer = "Popn_TWN2", encoding="utf8")

head(TWPOP@data)
TWTable<-TWPOP@data

County<-TWPOP$COUNTY
Sel<- County =='新北市' | County == '臺北市' 
TWN_North<- TWPOP[Sel,]
length(TWN_North)
plot(TWN_North)

#####################
## Spatial Neighbors
#####################

### 1. Contiguity: QUEEN vs. ROOK

#?poly2nb: Construct neighbours list 
TWN_nb<-poly2nb(TWN_North) #QUEEN = TRUE，紀錄每個polygon的鄰居數
summary(TWN_nb)#兩個鄰居的polygon有三個鄰居，11個鄰居的有一個

TWN_nb2<-poly2nb(TWN_North,queen=FALSE ) # ROOK

# 1.1. Finding neighbors of a district
TWN_nb[1]

# 1-2 Buiding Neighborhood Matrix
TWN_nb_w.mat <-nb2mat(TWN_nb, style="B"); TWN_nb_w.mat#用矩陣方式來看
# style = B is the basic binary coding, 
# W is row standardised, C is globally standardised

# 1.3. Plot the Neighborhood Matrix
coords<-coordinates(TWN_North)#產生polygon中心點座標
plot(TWN_North)
plot(TWN_nb, coords, add=T)


### 2. K-nearest Neighbors (KNN)#找離自己最近的k個

IDs <-TWN_North@data$TOWN_ID
TWN_kn1<-knn2nb(knearneigh(coords, k=2), row.names=IDs)#找最近的兩個
plot(TWN_North)
plot(TWN_kn1, coords, add=T)

TWN_kn1[1]


### 3. Distance-based (fixed distance band)#定義一個中心點範圍內都是鄰居

TWN_ran1<-dnearneigh(coords, d1=0, d2=20000, row.names=IDs)#d1:中心點，d2:範圍
plot(TWN_North)
plot(TWN_ran1, coords, add=T)

#########################
## From Spatial Neighbors to ListW (Weighting matrix)
#########################

TWN_nb_w<- nb2listw(TWN_nb, zero.policy=T) # default: style = "W" #列標準化
TWN_nb_w$weight[1]

#########################
## Spatial Autocorrelation 
#########################

Popn<-TWN_North@data$A65UP_CNT;
Popn<-as.numeric(paste(Popn))

# 1. Mapping the attribute
library(GISTools)
shades = auto.shading(Popn, n = 5, cols = brewer.pal(5, "Greens")) 
choropleth(TWN_North, Popn, shades) 

# 2.Moran I Statistic

M<-moran.test(Popn, listw=TWN_nb_w, zero.policy=T); M  #人口資料，listw:空間權重，zero:如果有孤立點的話怎麼半

# 3.Monte-Carlo simulation of Moran䏭 I
bperm<-moran.mc(Popn,listw=TWN_nb_w,nsim=999)#做999次的排列檢定(拆開重新亂排排列)
hist(bperm$res, freq=TRUE, breaks=20, xlab="Simulated Moran's I")
abline(v=0.442, col="red")#顯示統計顯著，真實狀況不是亂排的

# 4.Moran Spatial Correlogram
cor<-sp.correlogram(TWN_nb, Popn, order=3, method="I", style="W")#第1,2,3階鄰居
print(cor); plot(cor)

# 5.Moran Scatter Plot
nci <- moran.plot (Popn, TWN_nb_w, labels=IDs , xlab="Popn", ylab="SL Popn")

# 6.Getis-Ord General G Statistic
TWN_ran1_wb<-nb2listw(TWN_ran1, style="B", zero.policy=T)
G<-globalG.test(Popn, listw=TWN_ran1_wb); G

