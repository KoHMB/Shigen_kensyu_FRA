# 解析するデータは第34回と同じ
head(FishTrait_pca)
nrow(FishTrait_pca)

# kmean関数で5つに分ける
res_km <- kmeans(FishTrait_pca,5)

# 結果を図示
library(cluster)
clusplot(FishTrait_pca, res_km$cluster, color=TRUE, shade=TRUE, labels=2, lines=0,family="Osaka")

