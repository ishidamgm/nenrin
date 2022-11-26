# growth_meteorology_test.R

#  277_h600 ####
wd<-"../../Abies_mariesii/277_h600"
setwd(wd)
dir()
L <- ReadShapefile_AnnualRings("年輪補完_277_h600",ring.tag="ring")
Lplot(L)
ya<-YearArea(L,yr_end=2017)   # 要注意　2017には倒れていた?
idx<-TreeRingIndex(ya)$idx
r12<-cor2yr(idx,met,y1=1901,y2=1955)
bp<-barplot(r12,main=wd)
abline(h=c(0),lty=1,lwd=3,col="red")
abline(v=c(mean(bp[12:13])),lty=2,lwd=3)
text(bp,r12*0.9,c(1:12,1:12))

#  277_h400 ####
wd<-"../../Abies_mariesii/277_h400"
setwd(wd)
dir()
L <- ReadShapefile_AnnualRings("年輪補完_277_h400_3_next_plus_脱matrix_2",ring.tag="ring")
Lplot(L)
ya<-YearArea(L,yr_end=2018)   # 要注意　2017には倒れていた?
idx<-TreeRingIndex(ya)$idx
r12<-cor2yr(idx,met,y1=1901,y2=1955)
bp<-barplot(r12,main=wd)
abline(h=c(0),lty=1,lwd=3,col="red")
abline(v=c(mean(bp[12:13])),lty=2,lwd=3)
text(bp,r12*0.9,c(1:12,1:12))



