# データ読み込み
FishTrait2 <- read.csv("FishBodyTraitData2.csv",row.names = 1)

# データを眺める
View(FishTrait2) # ID、種、尾ビレの形以外の数値は全長に対する比

# 尾ビレの形が円形のものは尾叉長/全長の比率はNAになっているので、解析のため1とする
FishTrait2$ratio.Fork.length<-ifelse(is.na(FishTrait2$ratio.Fork.length),1,FishTrait2$ratio.Fork.length)

# PCAは数値データが対象なので、種名と尾ビレの形を除いて解析
FishTrait_pca <- FishTrait2[,-c(1,2,8)]
row.names(FishTrait_pca) <- FishTrait2[,c(1)]
# 各数値は大きさがバラバラで同じ基準で分散を比べられないのでscale=Tとする
res_pca<-prcomp(FishTrait_pca,scale=T)

names(res_pca)

#各主成分の標準偏差との各変数からの重み（合成ベクトル）
print(res_pca)

# 結果のまとめ
summary(res_pca) #各主成分の標準偏差、説明率、累積説明率

# 変数が５つなので第５主成分までの分散の大きさをプロット
plot(res_pca)

# 第１主成分と第２主成分を軸に取り、各変数のベクトルとデータを表示
# par(family="HiraginoSans-W3") # Macだと日本語が文字化けするのでフォント指定
biplot(res_pca)
biplot(res_pca,scale=0) #　主成分を標準化をせずに表示
