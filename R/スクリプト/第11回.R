#サンプルサイズの決定
pop_num <- 1000
#サンプルサイズをpop_numとして、男性女性の身長の平均と標準偏差から正規乱数でデータ生成
height_m <- rnorm(pop_num,167.6,7.0)
height_f <- rnorm(pop_num,154.1,6.9)

#Welchの方法でt検定
t.test(height_m,height_f)

#t検定の結果のオブジェクトを格納
res_ttest<-t.test(height_m,height_f)

#t検定の結果オブジェクトから、t値、p値を取り出す
res_ttest$statistic #t-value
res_ttest$p.value   #p-value

#t検定の結果オブジェクトから、どの検定を行ったか取り出す
res_ttest$method
