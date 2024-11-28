# vegan packageのrda関数を利用
# install.packages("vegan")
library(vegan)

# データ読み込み
FishColEnv <- read.csv("FishColEnv.csv")
names(FishColEnv)
head(FishColEnv)
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
# 今回は水温-1℃のみで、0にしてしまう。絶対温度をとっても良い（+273.15℃でK）
FishHabitatEnv$minTemp[1]<-0
res_dca<-decorana(FishHabitatEnv)

# RDA解析（ただし、変数のスケールをとる）
res_rda <- rda(FishColTrait~.,data=FishHabitatEnv,scale=T)
summary(res_rda)
res_rda$tot.chi
plot(res_rda)
row.names(FishColTrait)<-FishColEnv$Species
res_rda <- rda(FishColTrait~.,data=FishHabitatEnv,scale=T)
plot(res_rda)
par(family="Osaka")

FishColTraitDorsal<-data.frame(FishColTrait$DorsalRed,FishColTrait$DorsalGreen,FishColTrait$DorsalBlue)
row.names(FishColTraitDorsal)<-FishColEnv$Species
res_rda2 <- rda(FishColTraitDorsal~.,data=FishHabitatEnv,scale=T)
summary(res_rda2)
plot(res_rda2)
