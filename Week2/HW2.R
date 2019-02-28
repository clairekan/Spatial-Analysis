library(rgdal)
library(ggplot2)
FastFood <- readOGR(dsn = ".", layer = "Tpe_Fastfood", encoding="utf8")
FastFood<-as.data.frame(FastFood)
#將家數從factor轉換成numeric
FastFood$TYPE_90<- as.numeric(as.character(FastFood$TYPE_90))#將factor轉換
FastFood$TYPE_99<- as.numeric(as.character(FastFood$TYPE_99))#將factor轉換
#選出要的資料
library(dplyr)
FastFood1<-select(FastFood,ALIAS,TYPE_90,TYPE_99,TOWN,STORE)
#計算家數的變化，形成新的column
FastFood1<-mutate(FastFood1,diff=TYPE_99-TYPE_90)
#把數值變化轉成rise、remain、fall
for(i in 1:length(FastFood1$diff)){
  if(FastFood1$diff[i]>0){
    FastFood1$diff[i]<-"rise"
  }
  else if(FastFood1$diff[i]==0){
    FastFood1$diff[i]="remain"
  }
  else if(FastFood1$diff[i]<0){
    FastFood1$diff[i]="fall"
  }
}
#畫圖
FastFood1$diff<-as.factor(FastFood1$diff)
ggplot(FastFood1,aes(x=TOWN,fill=diff))+geom_bar(position='stack')+facet_grid(STORE~.)

