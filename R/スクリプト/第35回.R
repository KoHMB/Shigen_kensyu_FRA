# 解析するデータは第33回と同じ
head(FishTrait_pca)

# plotして変数同士の関連を概観
plot(FishTrait_pca)

# 相関行列で変数のまとまりを概観
cor_mat_Fishtrait <- round(cor(FishTrait_pca),3) # 正の相関と負の相関の２つに分けられそう

# 相関行列の固有値を確認
eigen(cor(FishTrait_pca)) # 1以上は２つ、５つ目で急減

# 2つの因子で分析
res_factanal <- factanal(FishTrait_pca,factors = 2)

