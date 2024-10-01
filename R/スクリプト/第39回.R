# vegan packageのrda関数を利用
# install.packages("vegan")
library(vegan)

# データ読み込み
FishColEnv <- read.csv("FishColEnv.csv")
names(FishColEnv)

# 体色についてのデータのデータフレーム（目的変数）
FishColTrait <- FishColEnv[,c(2:7)]
# 生息環境についてのデータのデータフレーム（説明変数）
FishHabitatEnv <- FishColEnv[,c(8:12)]

# 説明変数外観
plot(FishHabitatEnv)
# VIFを計算してみる
1/(1-cor(FishHabitatEnv)^2)

# decorana関数で説明変数の傾向チェック
res_dca<-decorana(FishHabitatEnv)
# decorana関数は正の値にのみ有効なので、データの中の負の値を処理する
# 今回は水温-1℃のみで、0にしてしまう。絶対温度をとっても良い（+275℃でK）
FishHabitatEnv$minTemp[1]<-0
res_dca<-decorana(FishHabitatEnv)
plot(res_dca)

# RDA解析（ただし、変数のスケールをとる）
res_rda <- rda(Y=FishColTrait,X=FishHabitatEnv,scale=T)
summary(res_rda)
plot(res_rda)
