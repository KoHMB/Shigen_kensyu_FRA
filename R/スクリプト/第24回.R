# 第23回作成データの読み込みと確認
load("res_lm_catch.rda")
head(res_lm_catch)

# lm結果の残差プロット
plot(res_lm_catch$residuals)
hist(res_lm_catch$residuals)

# 正規乱数発生とqq plotを自力で
stdnorm.smp <- rnorm(100,0,1)
hist(stdnorm.smp)
ordnum <- seq(1,100)
prob <- (ordnum-0.5)/(max(ordnum))
theo.smp<-qnorm(prob,0,1)
ord.stdnorm.smp <- stdnorm.smp[order(stdnorm.smp,decreasing = F)]
plot(theo.smp,ord.stdnorm.smp)

# 正規乱数のqqplot
qqnorm(stdnorm.smp)

# lm結果の残差に対してqqplot
qqnorm(res_lm_catch$residuals)

# lm結果の残差に対してShapiro-Wikl test
shapiro.test(res_lm_catch$residuals)
