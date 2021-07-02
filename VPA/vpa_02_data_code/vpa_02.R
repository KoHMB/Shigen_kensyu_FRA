#2020年VPA実践研修

devtools::install_github("ichimomo/frasyr@dev")　#最新版のFrasyrのインストール
library(frasyr)　　　　　　　　　　　　　　　　　#Frasyrの呼び出し


#データの読み込み----

caa <- read.csv("caa.csv",row.names=1)           #caaの読み込み
waa <- read.csv("waa.csv",row.names=1) 　　　　　#waaの読み込み
maa <- read.csv("maa.csv",row.names=1)           #maaの読み込み
M   <- read.csv("M.csv",row.names=1)             #Mの読み込み

dat <- data.handler(caa=caa,waa=waa,maa=maa,M=M) #データをVPA用の関数に変換する

dat

#チューニングなしVPAの実行----

res <- vpa(dat,                     #VPAを行うデータ
           tf.year=2017:2019,       #ターミナルFをどの年の平均にするか
           Pope = TRUE,             #Popeの近似式を使うならTRUE, Baranovを用いるならFALSE
           tune = FALSE,            #チューニングはしないのでFALSE（デフォルトはFALSE）
           last.catch.zero = FALSE, #最終年の漁獲尾数が全部0の場合はTRUE(デフォルトはFALSE)
           plus.group = TRUE,       #最高齢はプラスグループか否か（デフォルトはTRUE)
           term.F ="max",           #ターミナルFの最高齢だけ推定なら"max", 全年齢なら"all"
           p.init = 1,
           plot = TRUE,             #チューニングに使った資源量指標値に対するフィットのプロット
           plot.year =2012:2018)

#チューニングなしVPAの結果----

res$term.f    #推定されたターミナルＦの値をみる

res$faa       #推定された年別年齢別漁獲係数

res$naa       #推定された年別年齢別資源尾数

res$saa       #推定された年別年齢別選択率

res$baa       #推定された年別年齢別資源重量（バイオマス）

res$ssb       #推定された年別年齢別産卵親魚量

#VPAの結果を可視化する
plot_vpa(res, plot_year=2012:2020)

plot_vpa(res,what.plot="SSB",plot_year=2012:2020)














































plot(res$faa$`2020`,type="b",xlab="Age",ylab="F",ylim=c(0,max(res$faa$`2020`)))

plot(res$naa$`2020`,type="b",xlab="Age",ylab="number",ylim=c(0,max(res$naa$`2020`)))



plot_residual_vpa(res)












































#ケース1（選択率更新法) 体重実数平方和，チューニングには1歳から6歳の指標のみ使用----

res<- vpa(dat,
          last.catch.zero=FALSE,
          min.age=c(0,1,2,3,4,5,6),
          max.age=c(0,1,2,3,4,5,6),
          Pope=TRUE,
          tune=TRUE,
          sel.update=FALSE,
          sel.f=NULL,
          sel.def="maxage",
          term.F="max",
          est.method="ls_nolog",#
          b.est=FALSE,
          p.init=1.1,
          abund=c("B","B","B","B","B","B","B"),
          use.index=c(2:7),# 1歳から6歳までにあてはめ．0歳ぬかし
          tf.year=2016:2018,
          plus.group=TRUE,
		  madara=TRUE
         
)
res$term.f
plot_residual_vpa(res)

res<- vpa(dat,
          last.catch.zero=FALSE,
          min.age=c(1,2,3,4,5,6),
          max.age=c(1,2,3,4,5,6),
          Pope=TRUE,
          tune=TRUE,
          sel.update=FALSE,
          sel.f=NULL,
          sel.def="maxage",
          term.F="max",
          est.method="ls_nolog",#
          b.est=FALSE,
          p.init=1.1,
          abund=c("B","B","B","B","B","B"),
          #use.index=c(2:7),# 1歳から6歳までにあてはめ．0歳ぬかし
          tf.year=2016:2018,
          plus.group=TRUE,
          madara=TRUE
          
)
res$term.f
plot_residual_vpa(res)