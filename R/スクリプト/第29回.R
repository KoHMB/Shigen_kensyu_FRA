# read data fram csv file
catch_data2 <- read.csv("catch_data2.csv")
barplot(table(catch_data2$area))

# install glmmML
# install.packages("glmmML")
library(glmmML)

res_glmm_catch <- glmmML(catch~vessel+temp,family = poisson(link = "log"), data = catch_data2, cluster = as.factor(area))
summary(res_glmm_catch)

# glmm結果から推定係数の取り出し
#固定効果
res_glmm_catch$coefficients
#ランダム効果
res_glmm_catch$sigma

# glmm結果から推定係数の誤差の取り出し
res_glmm_catch$coef.sd
# glmm結果からランダム効果の誤差の取り出し（ばらつきの大きさの推定値にも誤差）
res_glmm_catch$sigma.sd

