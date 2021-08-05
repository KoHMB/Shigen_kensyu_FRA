L_mean = 100 # L∞の平均値
L_std = 1 # L∞の標準偏差
K_mean = 0.3 # kの平均値
K_std = 0.0 # kの標準偏差
t0 = -0.5 # t0（決め打ち）
curve(L_mean*(1-exp(-K_mean*(x-t0))),xlim=c(t0,20),ylim = c(0,100))
n1 = n2 = n3 = 30 # ここでは３回の放流を想定
n = n1 + n2 + n3 # 合計90個体を放流
L = rnorm(n = n, mean = L_mean, sd = L_std) # 個体差を考慮
K = rnorm(n = n, mean = K_mean, sd = K_std) # 個体差を考慮
t_start = c( rep(1, n1), rep(2, n2), rep(3, n3) ) # 放流開始時間
t_end = rgeom(n,0.1) + t_start + rep(1, n) # 捕獲時間
t_diff = t_end - t_start # 放流期間
VB = function(L, K, t, t0){  L * ( 1 - exp(-K * (t-t0)) )} # 個体ごとに呼び出される成長式
L_start = L_end = NULL
for(i in 1:n){ # サイズの個体差は、放流開始時＆パラメータの個体差で決まる
  L_start[i] = VB(L[i], K[i], t_start[i], t0) # 放流時のサイズ
  L_end[i] = VB(L[i], K[i], t_end[i], t0) # 捕獲時のサイズ
}
L_diff = L_end - L_start # サイズの変化量
res = nls( L_diff ~ (a - L_start) * (1-exp(-b*t_diff)), start = c(a = 200, b = 1.5) ) # 非線形回帰
L_est = coef(res)[[1]] # L∞推定値の抽出
K_est = coef(res)[[2]] # k推定値の抽出
cat("L_est =", L_est, "; K_est =", K_est, "\n") # 推定結果