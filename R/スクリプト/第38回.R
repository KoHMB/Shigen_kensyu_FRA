# classical MDA
# データ読み込み
FishTrait2 <- read.csv("FishBodyTraitData2.csv")

# データを眺める
View(FishTrait2) # ID、種、尾ビレの形以外の数値は全長に対する比

# 尾ビレの形が円形のものは尾叉長/全長の比率はNAになっているので、解析のため1とする
FishTrait2$ratio.Fork.length<-ifelse(is.na(FishTrait2$ratio.Fork.length),1,FishTrait2$ratio.Fork.length)

# 数値データが対象なので、種名と尾ビレの形を除いて解析
FishTrait_num <- FishTrait2[,-c(1,2,8)]
row.names(FishTrait_num) <- FishTrait2[,c(1)]

# 各数値は大きさがバラバラで同じ基準で距離を比べられないのでscaleしてから距離を求める
FishTrait_dist<-dist(scale(FishTrait_num))
res_mds<-cmdscale(FishTrait_dist)
res_mds # デフォルトで２次元の結果を出力, 引数kで次元を指定

plot(res_mds)
plot(res_mds,type ="n")
text(res_mds,row.names(FishTrait_num))
# 日本語フォント指定 for Mac 
# text(res_mds,row.names(FishTrait_num),family= "HiraKakuProN-W3")

# LDA
# irisデータを使う
data(iris)
iris
nrow(iris)
# Sepal.Length,Sepal.Width(額の長さ、幅)と、
# Petal.Length,Petal.Width(花弁の長さ、幅)の数量データを用いて
# カテゴリSpceies(3種)を判別する
# 教師あり解析だが、データを訓練・テスト用に分割して
# どれだけ予測できたかを確認する(1種につき50レコードなので、5データずつテスト用に抜く)
unique(iris$Species)

iris.setosa <- subset(iris, iris$Species=="setosa")
test.id <- sample(nrow(iris.setosa),size = 5,replace = F)
iris.test.setosa <- iris.setosa[test.id,]
iris.train.setosa <- iris.setosa[-test.id,]

iris.versicolor <- subset(iris, iris$Species=="versicolor")
iris.test.versicolor <- iris.versicolor[(test.id),]
iris.train.versicolor <- iris.versicolor[-(test.id),]

iris.virginica <- subset(iris, iris$Species=="virginica")
iris.test.virginica <- iris.virginica[(test.id),]
iris.train.virginica <- iris.virginica[-(test.id),]

iris.test <- rbind(iris.test.setosa, iris.test.versicolor, iris.test.virginica)
iris.train <- rbind(iris.train.setosa, iris.train.versicolor, iris.train.virginica)
nrow(iris.train)

# 訓練データで判別分析
library(MASS)
res_lda_iris <- lda(Species~ ., data=iris.train)
res_lda_iris

plot(res_lda_iris, col = as.numeric(iris.train$Species), abbrev = 1) 

# 判別式による数量データからの種分類予測
pred_train_lda_iris <- predict(res_lda_iris)
pred_train_lda_iris$class
(pred_train_lda_iris$class == iris.train$Species)
table(pred_train_lda_iris$class,iris.train$Species)

# テストデータでの予測
pred_test_lda_iris <- predict(res_lda_iris, newdata=iris.test)
table(pred_test_lda_iris$class,iris.test$Species)
