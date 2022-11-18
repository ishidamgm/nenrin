# setClass.R ####


# library    library(shapefiles)


degree<-pi/180  #### ラジアン定数


# setClass  class_DiskInfo ####
setClass (
  "class_DiskInfo",
  slots=c(
    id="character",
    code="character",
    species="character",
    place="character",
    latitude="numeric",
    longitude="numeric",
    altitude="numeric",
    year_Pith="numeric",
    year_OutmostRing="numeric",
    DiskSamplingDate="character",
    DiskSampler = "character",
    analyst="character",
    memo="character"
  )
)

# Analysis ####
# setClass  class_YearArea ####
setClass (
  "class_YearArea",
  slots=c(
    wd="character",
    L2="list",
    L3="list",
    Year_Area0="data.frame",
    Year_CrackArea="data.frame",
    Year_Area="data.frame"
  )
)




# setClass_class_TreeRingsInterpolate ####

setClass (
  "class_TreeRingsInterpolate ",                         # クラス名

  slots=c(                   # スロットの型指定
    P_filename ="character",
    P_id.tag="character",
    P_ring.tag="character",

    P    = "data.frame", #放射計測点:データフレーム　x,y,id,yr,r,deg
    P00  = "numeric", #  原点:c(px00,py00) 髄の中心　通常　0,0 の近く　放射計測点.shp　id==0の点に格納
    n_id = "numeric",	#　放射計測線本数　	:計測線本数　length(unique(P$id)) ただし原点(髄)id0を除いたP
    YR_P = "numeric", #　総年輪		　	:ベクトル一番外側の形成層も含む推定後の年輪の年　=　unique(P$ring)　
    n_YR = "numeric", # 総年輪数			:整数　一番外側の形成層も含む推定後の総年輪数　 = unique(P$yr), length(L2)

    L_filename ="character",
    L_id.tag="character",
    L_ring.tag="character",
    L = "list",	      #　年輪ポリゴン　		:リスト$年数-マトリクスx,y
    L_= "data.frame", #データフレーム　x,y,r,rad,deg
    YR_L= "numeric",  #ベクトル　(外側から)　年輪ポリゴンの外から数えた年数　=dbf$ring,　names(L)
    ln= "numeric",    #ン数		:整数　length(L)
    L2= "list",        # list : tree ring polygons interpolated
    L2_filename ="character"
    #maLr= "numeric",		:整数　n_YR-1
    #PL= "numeric",	####　放射計測点		:マトリクス年数(1年刻み),ライン番号
  )
)


# setClass_class_TreeRingsInterpolate ####

setClass(
  "class_IncompleteTreeRingsFix",                         # クラス名

  slots=c(                   # スロットの型指定
    P00="numeric",     #x,y coordinates of a center point of tree rings
    L_filename ="character",
    L_id.tag="character",
    L_ring.tag="character",
    L = "list",	      #　年輪ポリゴン　		:リスト$年数-マトリクスx,y
    L_= "data.frame", #データフレーム　x,y,r,rad,deg
    yr = "numeric",  #ベクトル　(外側から)　年輪ポリゴンの外から数えた年数　=dbf$ring,　names(L)
    rn = "numeric",  #ベクトル　(外側から)　年輪ポリゴンの外から数えた参照番号
    inc.ring = "data.frame", #同一点を相互に有する年輪番号。不完全年輪修正にかかわる。
    inc_filename ="character",    #  inc.ring の保存ファイル名(.csv)
    pn ="table",
    inc_edit="data.frame",
    ln= "numeric",    #入力年輪ポリゴン数		:整数　length(L)
    L2= "list",        # list : tree ring polygons fixed
    L2_filename ="character" #  file name of L2
  )
)

