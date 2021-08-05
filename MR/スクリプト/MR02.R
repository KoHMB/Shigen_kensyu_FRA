N = 10000 # 放流個体数
T = 100 # 計算させる最大単位時間
dat = matrix( 0, nrow = T+1, ncol = N ) # 全個体の生死を格納
dat[1, ] = rep(1,N) # t=1では全個体生存
F = 0.1 # 捕獲（漁獲）死亡係数
M = 0.1 # 自然係数
Z = F + M # 全死亡係数
n = 0 # 捕獲個体数の初期値
C_time = rep(0, N) # 捕獲時間の初期値
for( i in 1:N ) {
  for( t in 2:(T+1) ) { 
    if( dat[t-1, i] == 1 ) {# 生存している個体について
      if( exp(-Z) > runif(1) ) {# 生存
        dat[t, i] = 1
      }else if( F/Z > runif(1) ) {# 死亡が捕獲かどうか判定
        n = n + 1 # 捕獲個体数の上書き
        C_time[i] = t-1 # 捕獲時間
      }
    }
  }  
}
T_max = max( C_time ) # 最長捕獲時間を捕獲期間とする
Z_est = n / sum( C_time ) # Zの推定値
F_est = n * Z_est / N / ( 1 - exp(-Z_est * T_max) ) # Fの推定値
M_est = Z_est - F_est # Mの推定値
cat( "Z_est =", Z_est, "; F_est =", F_est, "; M_est =", M_est, "\n" ) # 推定結果