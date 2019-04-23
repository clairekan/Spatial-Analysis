library(rgdal)
library(spatstat)
library(GISTools)

School <- readOGR(dsn = ".", layer = "Schools", encoding="utf8")
TWCounty <- readOGR(dsn = ".", layer = "Taiwan_county", encoding="utf8")

#step 1
bnd<-bbox(School)
x.coor<-School$coords.x1
y.coor<-School$coords.x2
x.range<-bnd[1,]
y.range<-bnd[2,]

School.ppp<-ppp(x.coor,y.coor,x.range,y.range)
TN.Windows<-owin(xrange=x.range, yrange=y.range)#window確定產生邊邊的邊界

#step 2
nn1<-rpoint(424, win=TN.Windows)

#step 3
nnd<-nncross(nn1,School.ppp, k=1)#前面放逐點找的，後面放找的範圍

#step 4
F = ecdf(nnd$dist)
plot(F)

#step 5


for(i in 1:100){
  nn2<-rpoint(424, win=TN.Windows)
  nn1.d<-nncross(nn1,nn2)
  G.ran = ecdf(nn1.d$dist) 
  lines(G.ran, main="F function",xlim=c(0,5000))
}
lines(F, main="F function",xlim=c(0,5000),col="red")


CI=envelope(School.ppp, Fest, nsim=99, nrank=1)
plot(CI)









