
# read data fram csv file
catch_data <- read.csv("catch_data2.csv")

# install glmmML
install.packages("glmmML")
library(glmmML)

res_glmm_catch <- glmmML(catch~vessel+temp,family = poisson(link = "log"), data = catch_data, cluster = as.factor(area), method = "Laplace")
summary(res_glmm_catch)

# compare aic
res_glm_catch$aic   # glm; catch ~ vessel + temp
res_glm_catch2$aic  # glm; catch ~ vessel + temp + area
res_glmm_catch$aic  # glmm;catch ~ vessel + temp + random(area) (glmmML)

# glmm結果から推定係数の取り出し
#固定効果
res_glmm_catch$coefficients
#ランダム効果
res_glmm_catch$sigma

# glmm結果から推定係数の誤差の取り出し
res_glmm_catch$coef.sd
