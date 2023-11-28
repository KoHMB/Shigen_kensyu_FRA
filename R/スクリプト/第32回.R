# 分析するデータを作成する
# 次数が3となるデータに誤差を足して適当なデータを生成する
# y = 4*x1 - 2*x2 + x3 + e
x1 <- rnorm(100, 10, 5)
x2 <- (rnorm(100, 20, 3))^2
x3 <- (rnorm(100, 10, 0.5))^3
x4 <- x1 + rnorm(100, 0, 1)
x5 <- x1 + x2 + rnorm(100, 0, 1)
X <- cbind(x1, x2, x3, x4, x5)
Y <- 4 * x1 - 2 * x2 + x3 + rnorm(100, 0, 1)
plot(x1,Y)

res_lm <- lm(Y~x1+x2+x3+x4+x5)
summary(res_lm)

#install.packages("glmnet")
library(glmnet)

lasso.model.cv <- cv.glmnet(x = X, y = Y, nfolds = 10, alpha = 1)
lasso.model.cv
plot(lasso.model.cv)
lasso.model.cv$lambda.min
lasso.model <- glmnet(x = X, y = Y, lambda = lasso.model.cv$lambda.min, alpha = 1)
lasso.model$beta

ridge.model.cv <- cv.glmnet(x = X, y = Y, alpha = 0)
plot(ridge.model.cv)
ridge.model.cv$lambda.min
ridge.model <- glmnet(x = X, y = Y, lambda = ridge.model.cv$lambda.min, alpha = 0)
ridge.model$beta


cpue_data <- read.csv("https://raw.githubusercontent.com/KoHMB/Shigen_kensyu_FRA/main/R/cpuestandardization.csv",header=T,row.names = 1)
head(cpue_data)

res_loglm <- lm(log(cpue)~lon+I(lon^2)+I(lon^3)+I(lon^4)+lat+I(lat^2)+I(lat^3)+I(lat^4),data=cpue_data)
summary(res_loglm)

cpue_data$log_cpue<-log(cpue_data$cpue)
cpue_X<-cpue_data[,2:3]
cpue_X$lon2 <- (cpue_X$lon)^2
cpue_X$lon3 <- (cpue_X$lon)^3
cpue_X$lon4 <- (cpue_X$lon)^4
cpue_X$lat2 <- (cpue_X$lat)^2
cpue_X$lat3 <- (cpue_X$lat)^3
cpue_X$lat4 <- (cpue_X$lat)^4
head(cpue_X)
res_ridge_cv_lonlat <- cv.glmnet(y=cpue_data$log_cpue, x=as.matrix(cpue_X), nfolds = 10, alpha=0)
coef(res_ridge_cv_lonlat, s="lambda.min")

