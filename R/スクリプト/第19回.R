
# 真珠の個数データオブジェクト作成
pearl_data <- c(1,2,1,3,0,2,3,4,2,1)

# ベクトルオブジェクトをdpoisに入れて期待値λ=3のときの出現確率を計算
pearl_pois_prob <- dpois(lambda = 3,pearl_data)

# 出現確率を掛け合わせて尤度を計算
pearl_likelyhood <- prod(pearl_pois_prob)
pearl_likelyhood

# 対数をとって対数尤度を計算
log(pearl_likelyhood)

# log変換してから足し算して対数尤度を計算
pearl_log_likelyhood <- sum(log(pearl_pois_prob))
pearl_log_likelyhood
