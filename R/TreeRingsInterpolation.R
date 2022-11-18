# TreeRingsInterpolation.R ####

#' TreeRingsPoints read TreeRingsPoints shape file, check and save parameters
#'
#' @param TR is tree ring  class (class_TreeRingsInterpolate )
#'
#' #@return list of  (P,P00,YR_P,n_id,YR_P,n_YR)
#' @export
#'
#' @examples
#' P_filename<-"points277_h600"		#####　<<<　各人シェープファイル名入力　(拡張子はいらない)
#' TreeRingsPoints(TR)
#'
#' load(paste0(P_filename,".RData"))
TreeRingsPoints <-function(TR){
  d <- ReadShapefile_AnnualRingPoints(TR@P_filename,id.tag=TR@P_id.tag,ring.tag=TR@P_ring.tag)
  id <- d$id
  yr <- d$yr
  # 髄の中心，髄がない場合は放射計測線の中心点を【0,0】にする #
  i<-which(d$id==0); (P00<-c(d$x[i],d$y[i]))		###　原点 ■P00
  d$x<-d$x-P00[1];d$y<-d$y-P00[2]		###  原点 -> 0,0

  i<-order(d$id,d$yr)		# データの並べ替え #
  (d<-d[i,])			#　Qgisではデータ順をソートして保存できないため #
  r<-sqrt((d$x^2+d$y^2))	#### 髄からの距離
  l<-sqrt(diff(d$x)^2+diff(d$y)^2)	# 点間距離　年輪が歪んでいるときは点間距離が適当
  rad<-atan2(d$y,d$x)
  deg<-rad/degree
  P<-data.frame(d,rad,deg,r)	####　放射計測点データフレーム　■P
  #	id=0の中心点除去
  P<-subset(P,id!=0)
  P<-na.omit(P)
  head(P)
  YR_P<-unique(P$yr)   ##### ■　P$yr
  n_YR<-length(YR_P)		　　　　##### ■推定部分も含めた総年輪総数　(外側の形成層も含む)　
  n_id<-length(unique(P$id))		##### ■放射計測線本数　id0 髄データ(髄がない場合中心データは除く)

  tail(d)						#  確認 #
  str(d)
  table(yr)
  plot(d$yr)
  plot(d$id)
  plot(sort(d$yr))
  plot(r)
  plot(deg,r,xlab="放射線角度",ylab="髄からの距離")
  plot(d$id[-1],l,xlab="id",ylab="点間距離")

  TR@P <- P ; TR@P00 <- P00 ; TR@n_id<-n_id ; TR@YR_P <-YR_P ;TR@n_YR<-n_YR

  return(TR)

  # save(P,P00,YR_P,n_id,YR_P,n_YR,file=paste0(P_filename,".RData"))
  # return(list(P=P,P00=P00,YR_P=YR_P,n_id=n_id,YR_P=YR_P,n_YR=n_YR))

}


#' TreeRingsLines
#'
#' @param  TR is tree ring  class (class_TreeRingsInterpolate )
#'
#' @return
#' @export
#'
#' @examples
#'  TreeRingsLines(TR)
TreeRingsLines　<-function(TR){
  L<-TR@L ; P00 <-TR@P00 ;P <- TR@P ;L_ring_colname<-TR@L_ring.tag
  #　■L　年輪ポリゴンデータの追加と完成　■YR_L■ln #
  d<-read.shapefile(TR@L_filename)  	### ライン・データの読み込み
  ring<-as.numeric(as.vector(d$dbf$dbf[,L_ring_colname]))


  # 入力順に格納されているのを小さい順に直す　jjjはその順番 #
  jjj <- order(ring)
  (ln<-length(d$shp$shp))
  L<-c()
  for (i in 1:ln)L<-c(L,list(as.matrix(d$shp$shp[[jjj[i]]]$points))) # Lの追加

  (YR_L<-c(as.numeric(as.vector(d$dbf$dbf[,L_ring_colname])))[jjj])

  names(L)<-YR_L #　年輪数の名前を追加 #

  # 座標修正　髄の中心P00と【0,0】の差分  #
  for (i in 1:ln)L[[i]]<-t(t(L[[i]])-P00)

  # 年輪・計測線図化 #
  plot(L[[1]],type="l",col="red"); lapply(L,lines)
  points(P$x,P$y,pch=".",col="red");abline(h=0,v=0,col="blue")

  # Llist2dataframe #
  L_ <- Llist2dataframe(L)

  #　0～2πで髄からの距離はどのような関係にあるか? #
  #  年輪ポリゴン　【-180～180度×髄からの距離】#
  x<-L[[1]][,1];y<-L[[1]][,2]
  plot(atan2(y,x)/degree,sqrt(x^2+y^2),type="n", ylim=c(0,max(P$r,na.rm=TRUE)),
       xlab="-180～180度",ylab="髄からの距離 (m)")
  for (ii in 1:length(L)){
    x<-L[[ii]][,1];y<-L[[ii]][,2]
    points(atan2(y,x)/degree,sqrt(x^2+y^2),pch=".")
  }


  # !!! 放射計測線 new #
  points(P$deg,P$r,col="red",pch=".")

  #save(L,L_,file=paste0(L_filename,".RData"))
  TR@L<-L ; TR@L_<-L_ ; TR@YR_L <-YR_L ; TR@ln <- ln
  return(TR)

}


#' TreeRingsInterpolation_calc　interpolation tree ring between manual input tree rings with tree ring points
#'
#' @param TR
#'
#' @return TR
#' @export
#'
#' @examples
#' TreeRingsInterpolation_calc(TR)
#'
TreeRingsInterpolation_calc <-function(TR){

  TR@P -> P ; TR@P00 -> P00 ; TR@n_id -> n_id ; TR@YR_P -> YR_P ; TR@n_YR -> n_YR
  TR@L->L ; TR@L_->L_ ; TR@YR_L ->YR_L ; TR@ln -> ln

  ##YRn<-10:11 #### 補間用年輪ポリゴンの番号  <<<<< 数字を変えてスプライン曲線確認 2本以上
  YRn<-1:length(YR_L)##length(YR_L) #### 全データ実行

  (i12<-match(YR_L[YRn],YR_P))	# 放射計測点との合致と参照番号を確認(年+1，一番外側の年輪は年が　0　から始まるため)

  (YR2<-YR_L[YRn[1]]:YR_L[rev(YRn)[1]] ) #新たに構成する年輪ベクトル


  #　　>>> 年輪補間計算ループ開始 　

  L2<-c()	#　内挿用年輪ポリゴンデータL2初期化

  for(iii in 2:length(YRn)){	##個別実行　iii<-3　##全実行　2:length(YRn)
    yr12 <- YR_L[iii-1]:YR_L[iii]　###　検討区間年
    i12_ <-i12[iii-1]:i12[iii] ### 参照番号　年+1なので注意!!
    if(length(i12_ )==2) {   # !!!!!  間に推定すべき年輪がない場合スルー #
      L2<-c(L2,list(L[[iii-1]]))
      next}

    yr_pn <- c()
    for(i in 1:length(i12_)){
      yr_pn <- c(yr_pn,list(which(P$yr==yr12[i])))
    }
    names(yr_pn) <- yr12

    #### 年輪図と最近傍点
    L_out<-L[[YRn[iii-1]]];L_in<-L[[YRn[iii]]]
    np<-nstP(L_out,L_in)
    plot(L[[1]],type="l",col="red"); lapply(L,lines)
    segments(L_out[,1],L_out[,2],L_in[np,1],L_in[np,2],col="blue")


    #####　図化
    rdst.<-c()
    for( i in 2:(length(yr12)-1)){
      rdst.<-c(rdst.,list(rdst_MerginePlus(L,P,yr12[i])))
    }


    plot(0,type="n",ylim=c(0,1),xlim=c(-180,180),
         main=paste(yr12[1],rev(yr12)[1],sep = "-"),
         xlab="-180～180度",ylab="年輪間の距離の比率")
    for (i in 1:length(rdst.))points(rdst.[[i]],col=i)

    #### スプライン曲線推定　<<<線が乱れていたら入力ミスを確認!!!!　

    spl<-c()
    for (ii in 1:length(rdst.)){	##ii<-2
      #j <- yr_pn[[ii]]
      #px<-rdst.[[ii]];py<-P$y[j]
      #j<-!(is.na(px) | is.na(py))					#####　NA　除去
      (spline<-smooth.spline(rdst.[[ii]], spar =0.0002))
      xxx<-seq(-200,200,1)
      lines(predict(spline,xxx),col="red")
      spl<-c(spl,list(spline))  ##### 曲線関数格納
    }
    names(spl)<-yr12[2:(length(yr12)-1)]


    ######　年輪補間
    L2<-c(L2,list(L_out))
    for (jj in 1:length(spl)){	#jj<-36 <<<<1:length(spl)
      spline<-spl[[jj]]
      #plot(predict(spline,-180:180))
      dL<-L_out-L_in[np,]			###差分
      L.ave<-(L_out+L_in[np,])/2			###平均座標
      deg_spl<-atan2(L.ave[,2],L.ave[,1])/degree	####　髄からの角度
      ratio_<-predict(spline,deg_spl)$y
      L_cal<-L_in[np,]+dL*ratio_
      lines(L_cal,col="red")
      L2<-c(L2,list(L_cal))
    }

  }
  L2<-c(L2,list(L[[length(YRn)]]))	#	最内側の年輪を最後に追加 length(L2) length(YR2)
  #<<<　　年輪補間計算ループ終了

  names(L2)<-YR2		##	各年輪ポリゴンに名前をつける

  plot(L[[1]],type="l",col="red"); lapply(L2,lines,col="blue")
  points(P$x,P$y,pch=".",col="red")

  TR@L2 <- L2

  return(TR)





}




#' TreeRingInterpolation
#'
#' @param P_filename
#' @param L_filename
#' @param L2_filename
#' @param P_id.tag
#' @param P_ring.tag
#' @param L_ring.tag
#'
#' @return
#' @export
#'
#' @examples
#' TR<-TreeRingsInterpolation(
#' P_filename="points277_h600",
#' L_filename="277_h600不完全年輪追加",
#' L2_filename="Abies277_h600_interpolated",
#' P_id.tag="id",P_ring.tag="ring",
#' L_id.tag="id",L_ring.tag="ring")
#'
#' str(L)
#' Lplot2(TR@L,1:length(TR@L))
#'
#' #save(TR,file=paste0(TR@L2_filename,".RData"))
TreeRingsInterpolation <- function(P_filename,L_filename,L2_filename,
                                   P_id.tag="id",P_ring.tag="ring",L_id.tag="id",L_ring.tag="ring"){
  #  P_filename="points277_h600"; L_filename="line277_h600_2";L2_filename="Abies277_h600_interpolated"
  #  P_id.tag="id";P_ring.tag="ring";  L_id.tag="";L_ring.tag="id"

  # tree ring points
  TR<- new("class_TreeRingsInterpolate ") # treering_cls #str(TR)
  TR@P_filename<-P_filename #"points277_h600"
  TR@P_id.tag<-"id"
  TR@P_ring.tag<-"ring"
  # tree ring lines
  TR@L_filename<-L_filename # "line277_h600_2"
  TR@L_id.tag<-L_id.tag
  TR@L_ring.tag<-L_ring.tag
  # tree ring interpolate
  TR@L2_filename<-L2_filename #

  TR <- TreeRingsPoints(TR)
  #return(TR@L2_filename)
  TR <- TreeRingsLines(TR)

  # tree ling interpolation
  TR <- TreeRingsInterpolation_calc(TR)

  return(TR)

}
