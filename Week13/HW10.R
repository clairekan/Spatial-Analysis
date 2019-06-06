library(spdep)
library(rgdal)
library(GISTools)

Tainan_li <- readOGR(dsn = '.', layer = "Tainan_li", encoding="utf8")
Tainan_town <- readOGR(dsn = '.', layer = "Tainan_town", encoding="utf8")
School <- readOGR(dsn = ".", layer = "Schools", encoding="utf8")

#村里學校總數
Tainan_li$all_school<-poly.counts(School,Tainan_li)
li_count<-Tainan_li$all_school

TN_nb<-poly2nb(Tainan_li)
TN_nb_in<-include.self(TN_nb);#把自己加進去
TN_nb_in_w<- nb2listw(TN_nb_in, zero.policy=T)
LG.li<-localG(li_count, TN_nb_in_w); LG#只跑出z分數

LG1.li<-0
for (i in 1:800){LG1.li[i]<-LG.li[i]}
Tainan_li$LG<-LG1.li

# Mapping Hotspot Areas (using Local Gi*, p < 0.05)
LGV<-Tainan_li$LG
chk<-LGV-1.65

quadrant <- vector(mode="numeric",length=800)
quadrant[chk>0] <- 1 # Cluster
quadrant[chk<0] <- 2 # Non-cluster

colors <- c("red", "gray98")
plot(Tainan_li, border="darkgrey", col=colors[quadrant], main = "Cluster Map: School counts by li")
legend("bottomright",legend=c("Cluster","Non-cluster"), fill=colors,bty="n",cex=0.7,y.intersp=1,x.intersp=1)

#村里學校密度
Tainan_li$school_density<-poly.counts(School,Tainan_li)/poly.areas(Tainan_li)*10^6
school_density<-Tainan_li$school_density

TN_nb.d<-poly2nb(Tainan_li)
TN_nb_in.d<-include.self(TN_nb.d);#把自己加進去
TN_nb_in_w.d<- nb2listw(TN_nb_in.d, zero.policy=T)
LG.li.d<-localG(school_density, TN_nb_in_w.d); LG#只跑出z分數

LG1.li.d<-0
for (i in 1:800){LG1.li.d[i]<-LG.li.d[i]}
Tainan_li$LG.d<-LG1.li.d

# Mapping Hotspot Areas (using Local Gi*, p < 0.05)
LGV.d<-Tainan_li$LG.d
chk.d<-LGV.d-1.65

quadrant <- vector(mode="numeric",length=800)
quadrant[chk.d>0] <- 1 # Cluster
quadrant[chk.d<0] <- 2 # Non-cluster

colors <- c("red", "gray98")
plot(Tainan_li, border="darkgrey", col=colors[quadrant], main = "Cluster Map: School density by li")
legend("bottomright",legend=c("Cluster","Non-cluster"), fill=colors,bty="n",cex=0.7,y.intersp=1,x.intersp=1)


#鄉鎮學校總數
Tainan_town$all_school<-poly.counts(School,Tainan_town)
town_count<-Tainan_town$all_school

TN_nb.t<-poly2nb(Tainan_town)
TN_nb_in.t<-include.self(TN_nb.t);#把自己加進去
TN_nb_in_w.t<- nb2listw(TN_nb_in.t, zero.policy=T)
LG.li.t<-localG(town_count, TN_nb_in_w.t); LG#只跑出z分數

LG1.li.t<-0
for (i in 1:38){LG1.li.t[i]<-LG.li.t[i]}
Tainan_town$LG.t<-LG1.li.t

# Mapping Hotspot Areas (using Local Gi*, p < 0.05)
LGV<-Tainan_town$LG.t
chk<-LGV-1.65

quadrant <- vector(mode="numeric",length=38)
quadrant[chk>0] <- 1 # Cluster
quadrant[chk<0] <- 2 # Non-cluster

colors <- c("red", "gray98")
plot(Tainan_town, border="darkgrey", col=colors[quadrant], main = "Cluster Map: School counts by li")
legend("bottomright",legend=c("Cluster","Non-cluster"), fill=colors,bty="n",cex=0.7,y.intersp=1,x.intersp=1)

