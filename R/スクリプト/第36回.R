# 解析するデータは第32回と同じ
head(FishTrait_pca)
nrow(FishTrait_pca)

# 全てのデータを解析すると結果のプロットで大変なので、1/4くらい選択
FishTrait_hclust <- FishTrait_pca[1:(nrow(FishTrait_pca)/4),]
nrow(FishTrait_hclust)

# データ項目毎にスケールが違う場合はscale関数で標準化する．
scaled_FishTrait_hclust <- scale(FishTrait_hclust)
#mean(scaled_FishTrait_hclust[,1]) 大体０
#var(scaled_FishTrait_hclust[,1])　１

# データの距離行列を求める
dist_mat_Fishtrait <- dist(scaled_FishTrait_hclust)

# hclust関数で階層的クラスター解析
res_hclust <- hclust(dist_mat_Fishtrait)

# デンドログラムの表示
plot(res_hclust) 
# plot(res_hclust,family="Osaka") # Macはfontを指定

# 樹状図を距離が遠いところからいくつかに切り分ける
cutree(res_hclust,k=3) # 3つに切り分け
