#2020年VPA実践研修

devtools::install_github("ichimomo/frasyr@dev")　#最新版のFrasyrのインストール
library(frasyr)　　　　　　　　　　　　　　　　　#Frasyrの呼び出し

#データの読み込み----

caa <- read.csv("caa.csv",row.names=1)           #caaの読み込み
waa <- read.csv("waa.csv",row.names=1) 　　　　　#waaの読み込み
maa <- read.csv("maa.csv",row.names=1)           #maaの読み込み
M   <- read.csv("M.csv",row.names=1)             #Mの読み込み
index <-read.csv("Index.csv",row.names=1)        #indexの読み込み

dat <- data.handler(caa=caa,waa=waa,maa=maa,M=M,index=index) 

dat

#チューニングなしVPAの実行----

res <- vpa(dat,                     #VPAを行うデータ
           tf.year=2017:2019,       #ターミナルFをどの年の平均にするか
           Pope = TRUE,             #Popeの近似式を使うならTRUE, Baranovを用いるならFALSE
           tune = FALSE,            #チューニングはしないのでFALSE（デフォルトはFALSE）
           last.catch.zero = FALSE, #最終年の漁獲尾数が全部0の場合はTRUE(デフォルトはFALSE)
           plus.group = TRUE,       #最高齢はプラスグループか否か（デフォルトはTRUE)
           p.init = 1)              #ターミナルFに与える初期値


#チューニングなしVPAの結果----

res$term.f    #推定されたターミナルＦの値をみる

res$faa       #推定された年別年齢別漁獲係数

res$naa       #推定された年別年齢別資源尾数

res$saa       #推定された年別年齢別選択率

res$baa       #推定された年別年齢別資源重量（バイオマス）

res$sbb       #推定された年別年齢別産卵親魚量


#選択率の仮定をどうするか----

sel_1 <- res$saa$`2018` #2018年の選択率と同じと仮定する場合
sel_1

sel_2 <- res$saa$`2019` #2019年の選択率と同じと仮定する場合
sel_2

sel_3 <- rowMeans(res$saa[,c("2017","2018","2019")])  #2017年～2019年の選択率と同じと仮定する場合
sel_3 



#チューニングありVPAの実行（二段階法）----

res <- vpa(dat,                     #VPAを行うデータ
           tf.year=2017:2019,       #ターミナルFをどの年の平均にするか
           Pope = TRUE,             #Popeの近似式を使うならTRUE, Baranovを用いるならFALSE
           tune = TRUE,             #チューニングはするのでTRUE（デフォルトはFALSE）
           last.catch.zero = FALSE, #最終年の漁獲尾数が全部0の場合はTRUE(デフォルトはFALSE)
           plus.group = TRUE,       #最高齢はプラスグループか否か（デフォルトはTRUE)
           p.init = 1,              #ターミナルFに与える初期値
           term.F = "max",          #ターミナルFの最高齢だけ推定
           sel.f = sel_3,           #最終年の選択率の仮定を指定
           abund = c("N","N","N","N","N","N"), #チューニングの際の資源量指標値の属性
           min.age=c(0,0,0,0,0,0),　#チューニング指標の年齢参照範囲の下限
           max.age=c(3,3,0,0,3,3),　#チューニング指標の年齢参照範囲の上限
           est.method = "ls",       #推定方法（ls=最小二乗，ml=最尤法）
           b.est = FALSE           #bを推定するか否か
           #plot = TRUE,             #チューニングに使った資源量指標値に対するフィットのプロット
           #plot.year =2012:2018　　 #上のプロットの参照年
           )


#チューニングありVPAの実行（選択率更新法）----

res <- vpa(dat,                     #VPAを行うデータ
           tf.year=2017:2019,       #ターミナルFをどの年の平均にするか
           Pope = TRUE,             #Popeの近似式を使うならTRUE, Baranovを用いるならFALSE
           tune = TRUE,             #チューニングはするのでTRUE（デフォルトはFALSE）
           last.catch.zero = FALSE, #最終年の漁獲尾数が全部0の場合はTRUE(デフォルトはFALSE)
           plus.group = TRUE,       #最高齢はプラスグループか否か（デフォルトはTRUE)
           p.init = 1,              #ターミナルFに与える初期値
           term.F = "max",          #ターミナルFの最高齢だけ推定
           sel.update = TRUE,       #選択率更新法を指定
           sel.def = "max",          #選択率更新法で選択率をどのように定義するか
           abund = c("N","N","N","N","N","N"), #チューニングの際の資源量指標値の属性
           min.age=c(0,0,0,0,0,0),　#チューニング指標の年齢参照範囲の下限
           max.age=c(3,3,0,0,3,3),　#チューニング指標の年齢参照範囲の上限
           est.method = "ls",       #推定方法（ls=最小二乗，ml=最尤法）
           b.est = FALSE)           #bを推定するか否か



#チューニングありVPAの実行（全Ｆ推定法）----

res <- vpa(dat,                     #VPAを行うデータ
           #tf.year=2017:2019,       #ターミナルFをどの年の平均にするか
           fc.year=2018:2019,
           Pope = TRUE,             #Popeの近似式を使うならTRUE, Baranovを用いるならFALSE
           tune = TRUE,             #チューニングはするのでTRUE（デフォルトはFALSE）
           last.catch.zero = FALSE, #最終年の漁獲尾数が全部0の場合はTRUE(デフォルトはFALSE)
           plus.group = TRUE,       #最高齢はプラスグループか否か（デフォルトはTRUE)
          # p.init = c(0.2,0.4,0.8,0.8), #ターミナルFに与える初期値
          p.init = 1,
           term.F = "all",          #ターミナルFは全年齢で推定
           #sel.update = TRUE,       #選択率更新法を指定
           #sel.def = "mean",          #選択率更新法で選択率をどのように定義するか
           abund = c("N","N","N","N","N","N"), #チューニングの際の資源量指標値の属性
           min.age=c(0,0,0,0,0,0),　#チューニング指標の年齢参照範囲の下限
           max.age=c(3,3,0,0,3,3),　#チューニング指標の年齢参照範囲の上限
           est.method = "ml",       #推定方法（ls=最小二乗，ml=最尤法）
           b.est = FALSE)           #bを推定するか否か

#チューニングありVPAの結果----

res$term.f    #推定されたターミナルＦの値をみる
res$faa       #推定された年別年齢別漁獲係数
res$naa       #推定された年別年齢別資源尾数
res$saa       #推定された年別年齢別選択率
res$baa       #推定された年別年齢別資源重量（バイオマス）
res$ssb       #推定された年別年齢別産卵親魚量


res$pred.index  #モデルから推定された指標値 
res$q           #推定された比例定数q
res$b           #資源量指標値の非線形性に関するパラメータb
res$logLik      #推定された対数尤度


plot(res$faa$`2020`,type="b",xlab="Age",ylab="F",ylim=c(0,max(res$faa$`2020`)))

plot(res$naa$`2020`,type="b",xlab="Age",ylab="number",ylim=c(0,max(res$naa$`2020`)))



plot_residual_vpa(res, plot_year=2012:2018)

