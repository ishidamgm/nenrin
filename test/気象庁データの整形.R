### 気象庁データの整形.R
# 気象庁データの読み込み ####
fld<-"~/Dropbox/00D/00/ring/_R/nenrin/test"
setwd(fld)
dir()
met.file<-"Takayama_1900-2022.csv"
m.<-read.csv(met.file,as.is=TRUE)　　　　　　　　　　　　　　　　
#　必要とする平均気温のmatrix構成 ####
y1<-1900 ; y2 <- 2018
rn<-y2-y1+1
m<-matrix(m.[1:(12*rn),2],rn,12,byrow=TRUE)
colnames(m) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
rownames(m) <- y1:y2

# check
plot(as.vector(m[,1]),type="l")
m_ <- apply(m,1,mean)
plot(m_,type="b")

#
met<-cbind(m,mean=m_)
#write.csv(m,"Takayama_Temperature_1900-2018.csv")
#save(met,file="../data/Takayama_Temperature_1900-2018.RData")

