#もし`frasyr`をインストールされていない場合は、(https://github.com/ichimomo/frasyr)のサイトを参考にインストールを試みてください

library(frasyr)

caa  <- read.csv("data/caa0_2020.csv", row.names = 1)
waa  <- read.csv("data/waa1_2020.csv", row.names = 1)
maa  <- read.csv("data/maa2_2020.csv", row.names = 1)
cpue <- read.csv("data/cpue2_aicc2020.csv", row.names = 1)
dat  <- data.handler(caa, waa, maa, cpue, M=0.4)
dat$index[1:2,"2018"] <- NA 

cpue2 <- cpue
cpue2[1,] <- (cpue[1,]/
                mean(as.numeric(cpue[1,]), na.rm=TRUE))
cpue2[2,] <- (cpue[2,]/
                mean(as.numeric(cpue[2,]), na.rm=TRUE))
cpue2[3,] <- (cpue[3,]/
                mean(as.numeric(cpue[3,]), na.rm=TRUE))
cpue2[4,] <- (cpue[4,]/
                mean(as.numeric(cpue[4,]), na.rm=TRUE))
matplot(1970:2018,t(cpue2), type = "l", xlim = c(2000,2018),
        ylab = "基準化CPUE")
legend("topleft", legend = c("index1","index2","index3","index4"),
       col = 1:4, lty=1:4, ncol=2)

## 2-4. 全F推定法 (資源管理研修VPA編スクリプト.Rmdにあるモデルから拝借)

#この例題のケースでは高速計算可能なTMB=TRUEが使えるのでTMBのパッケージをあらかじめインストールしておくこと

library(TMB)
use_rvpa_tmb()

res5.1 <- vpa(dat,
              tf.year = 2015:2017,
              tune = TRUE,
              term.F = "all",
              alpha = 1,
              abund = c("N","N","SSB","SSB"),
              min.age = c(0,0,0,0),
              max.age = c(0,0,6,6),
              est.method="ml", #重み推定（最尤法）
              b.est = TRUE, #b推定
              sel.def="max",
              #b.fix = c(1,1,NA,NA),
              last.catch.zero=TRUE,
              fc.year=2016:2018,
              plot = TRUE,
              plot.year = 2002:2018,
              use.index =1:4,
              sigma.constraint = c(1,1,2,2),
              TMB = TRUE
)

#直近5年の推定された年齢別Fの値（リッジVPAなし）
res5.1$faa[,as.character(2013:2017)]

#レトロスペクティブ解析（リッジVPAなし）
retro_5.1 <- do_retrospective_vpa(res5.1, 
                                       n_retro=5, 
                                       b_reest =FALSE)

#res5.1のモデルの設定でλの探索
ridge_res<-autocalc_ridgevpa(
  input = res5.1$input,
  target_retro="F",
  n_retro=5,
  b_fix=TRUE,
  bin=0.1)

#探索されたλを代入したリッジVPAモデル
res5.1_ridge <- vpa(dat,
                    tf.year = 2015:2017,
                    tune = TRUE,
                    term.F = "all",
                    alpha = 1,
                    abund = c("N","N","SSB","SSB"),
                    min.age = c(0,0,0,0),
                    max.age = c(0,0,6,6),
                    est.method="ml", #重み推定（最尤法）
                    b.est = TRUE, #b推定
                    sel.def="max",
                    #b.fix = c(1,1,NA,NA),
                    last.catch.zero=TRUE,
                    fc.year=2016:2018,
                    plot = TRUE,
                    plot.year = 2002:2018,
                    use.index =1:4,
                    sigma.constraint = c(1,1,2,2),
                    lambda = 0.01, #ridge penaltyの大きさ
                    penalty = "p", #penalty項の与え方
                    beta =2,#penaltyの種類：1=lasso, 2=ridge
                    TMB = TRUE
)

#直近5年の推定された年齢別Fの値（リッジVPAあり）
res5.1_ridge$faa[,as.character(2013:2017)]

#レトロスペクティブ解析（リッジVPAあり）
retro_5.1ridge <- do_retrospective_vpa(res5.1_ridge, 
                                       n_retro=5, 
                                       b_reest =FALSE)

#etaなども用いる場合のリッジVPAモデル
res5.1_ridge2 <- vpa(dat,
                    tf.year = 2015:2017,
                    tune = TRUE,
                    term.F = "all",
                    alpha = 1,
                    abund = c("N","N","SSB","SSB"),
                    min.age = c(0,0,0,0),
                    max.age = c(0,0,6,6),
                    est.method="ml", #重み推定（最尤法）
                    b.est = TRUE, #b推定
                    sel.def="max",
                    #b.fix = c(1,1,NA,NA),
                    last.catch.zero=TRUE,
                    fc.year=2016:2018,
                    plot = TRUE,
                    plot.year = 2002:2018,
                    use.index =1:4,
                    sigma.constraint = c(1,1,2,2),
                    lambda = 0.01, #ridge penaltyの大きさ
                    penalty = "p", #penalty項の与え方
                    beta =2,　#penaltyの種類：1=lasso, 2=ridge
                    eta=0.99, #penaltyを年齢で分けて与えるときにeta.ageで指定した年齢への相対的なpenalty (0~1)
                    eta.age=0, #penaltyを年齢で分けるときにetaを与える年齢(0 = 0歳（加入）,0:1 = 0~1歳)
                    TMB =TRUE
)



