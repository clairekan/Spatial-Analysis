library(spdep)
library(rgdal)
library(GISTools)

TWPOP <- readOGR(dsn = '.', layer = "Popn_TWN2", encoding="utf8")

head(TWPOP@data)

TWPOP$old_rate <- TWPOP@data$A65UP_CNT/(TWPOP@data$A65UP_CNT+TWPOP@data$A0A14_CNT+TWPOP@data$A15A64_CNT)
old<-TWPOP@data$A65UP_CNT/(TWPOP@data$A65UP_CNT+TWPOP@data$A0A14_CNT+TWPOP@data$A15A64_CNT)
#真實數值
lm.palette=colorRampPalette(c( "white","orange","red"), space = "rgb")
spplot(TWPOP, zcol="old_rate", col.regions=lm.palette(20), main="老年人口比例")

#LISA
TWN_nb<-poly2nb(TWPOP)# Defining Spatial Neighbors
TWN_nb_w<- nb2listw(TWN_nb, zero.policy=T)# 2. Building Weighting Matrix
LISA <- localmoran(TWPOP$old_rate, TWN_nb_w, zero.policy=T,alternative = 'two.sided')
printCoefmat(data.frame(LISA))
diff = old - mean(old) # diff看自己和平均比起來算是H還是L
z = LISA[,4]
quad = c()
quad[diff>0 & z>0] = 1 # H-H
quad[diff<0 & z>0] = 2 # L-L
quad[diff>0 & z<0] = 3 # H-L
quad[diff<0 & z<0] = 4 # L-H
quad[LISA[, 5]>0.05]=5 # 不顯著，設定雙尾所以用0.05比較就可以
colors=c("red", "blue", "lightpink", "skyblue2", rgb(.95, .95, .95))
plot(TWPOP, border="grey", col=colors[quad], main = "LISA Map",xlim=c(97131.72,327759.52),ylim = c(2409189,2818715))
legend("bottomright",legend=c("HH","LL","HL","LH","NS"),fill=colors,bty="n",cex=0.7,y.intersp=1,x.intersp=1)

#G*
TW.nb.in = include.self(TWN_nb)
TW.nb.w.in = nb2listw(TW.nb.in)
Gi = localG(old,TW.nb.w.in)
LG = as.vector(Gi)
quad = c()
quad[LG>=1.645] = 1 # cluster
quad[LG <1.645] = 2 # non-cluster
colors=c("red", "lightgray")
plot(TWPOP, border="grey", col=colors[quad], main = "Cluster Map",xlim=c(97131.72,327759.52),ylim = c(2409189,2818715))
legend("bottomright", c("Cluster","Non-cluster"), fill=colors,bty="n",cex=0.7,y.intersp=1,x.intersp=1)
