# 2022　資源管理研修 Type2-01

# frasyr23のインストールとライブラリーの読み込み ----
devtools::install_github("ichimomo/frasyr23")
library(frasyr23)

# data_akaの読み込み
data("data_aka")
# 読み込んだデータの確認
head(data_aka)
tail(data_aka)

# ABCを算出 ----
abc2_aka<-calc_abc2(ccdata = data_aka,
          BT = 0.8,
          PL=0.7,
          PB=0,
          tune.par = c(0.5,0.4,0.4),
          n.catch = 5,
          n.cpue = 3,
          beta = 1,
          D2alpha = NULL)

names(abc2_aka)

# 結果をプロット ----
graph2_aka<- plot_abc2(res = abc2_aka,
                       fishseason = 0,
                       detABC = 0,
                       proposal = F)

names(graph2_aka)

# calc_abc2の結果オブジェクトからHCRについての図をプロット----
# BT07のHCRの結果
abc2_aka_BT07<-calc_abc2(ccdata = data_aka,
                    BT = 0.7,
                    tune.par = c(0.4,0.7,1.0))

# 基本ルール(BT=8)の結果とBT07のHCRの結果を同時にプロット
plot_hcr2(res.list = list(abc2_aka,abc2_aka_BT07),
          proposal = F,vline = T,vlineBan = T,
          hscale = "middle",hline = "dense")

# 追加ルール ----
# 迅速にデータが得られる場合A
abc2_aka_timelag0a<-calc_abc2(ccdata = data_aka,
                    BT = 0.6,
                    tune.par = c(0.4,0.7,1.0),
                    timelag0 = T)

# 迅速にデータが得られる場合B
data_aka2<-data_aka
data_aka2$catch[nrow(data_aka2)]<-NA
tail(data_aka2)
#最終年catchがNAなので6年平均とすると、NAを考慮して5年平均する
abc2_aka_timelag0b<-calc_abc2(ccdata = data_aka2,
                              BT = 0.65,
                              tune.par = c(0.5,0.7,1.0),
                              timelag0 = T,
                              n.catch = 6) 

#n.catch=6を指定しているので、デフォルト設定だと「6年平均漁獲量」となってしまう
graph2_aka_timelag0b<-plot_abc2(abc2_aka_timelag0b,latest_Catch_na = T)

# 変動緩和の場合
abc2_aka_resp04<-calc_abc2(ccdata = data_aka,
                              tune.par = c(0.3,0.6,0.3),
                              resp = 0.4)

# 積極的な漁獲1
abc2_aka_BT07<-calc_abc2(ccdata = data_aka,
                         BT = 0.7,
                         tune.par = c(0.4,0.7,1.0))

# 積極的な漁獲2
abc2_aka_BT065<-calc_abc2(ccdata = data_aka,
                         BT = 0.65,
                         tune.par = c(0.4,0.7,1.0))
