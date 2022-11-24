# growth_meteorology.R

#
load("../data/Abies277_h600.RData")
#
load("../data/Takayama_Temperature_1900-2018.RData")

plot(ya,type="b")

met

TreeRingIndex <- function(ya){
  sm<-smooth.spline(ya,spar=0.8)
  idx<-ya[,2]/predict(sm,ya[,1])$y
  return(list(spline=sm,idx=data.frame(year=ya[,1],TreeRingIndex=idx)))
}

sm <- TreeRingIndex(ya)$spline
idx<- TreeRingIndex(ya)$idx

windows()
par(mfrow=c(2,1))
plot(ya,type="b",ylab="年輪面積(m*m)",xlab="西暦年")　　　　　　　　　　　####yaを点と線のグラフにしてラベルつけた
lines(sm,col="red",lw=3)

plot(idx,type="b",ylab="基準化年輪面積",xlab="西暦年")　　　　　　####yearとindexラベルつけてグラフにした
abline(h=1,col="red")　　　　　　　　　　　　　　　　　　　　　　　　　　　　　####高さ1のところに赤い補助線つけた
abline(v=seq(1800,2020,10),col="gray")　


