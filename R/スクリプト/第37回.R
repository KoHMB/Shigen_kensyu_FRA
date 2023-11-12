# 解析するデータは第34回と同じ
head(FishTrait_pca)
nrow(FishTrait_pca)

# kmean関数で5つに分ける
res_km <- kmeans(FishTrait_pca,5,iter.max = 10)

# 初期値依存性が高い
res_km_iter <- list()
center.fork.length <- c()
iter.count<-c()
for(i in 1:10){
  res_km_iter[[i]] <- kmeans(FishTrait_pca,3,iter.max = 10000)
  center.fork.length <- cbind(center.fork.length,res_km_iter[[i]]$centers[,1])
  iter.count<- c(iter.count,res_km_iter[[i]]$iter)
}
center.fork.length
iter.count

# 結果を図示
library(cluster)
clusplot(FishTrait_pca, res_km$cluster, color=T, shade=T, labels=2, lines=1,family="Osaka")

# color:T/F; TRUEのとき密度が高くなるにつれ、水色、薄緑、赤、紫になる
# shade; 密度に応じて楕円（クラスター）に影づけ
# label 0~5; 0はラベルなし、1は各点と楕円を識別可能にする（非推奨）、2は各点と楕円にラベル、3は各点にラベル、4は楕円にラベル、5はプロット内で楕円にラベルづけして点を識別（非推奨）
# lines 0~2; 0は距離線なし、1はクラスター重心間の線分、2は境界線間の線分

