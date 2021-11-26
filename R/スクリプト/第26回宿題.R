catch_data <- read.csv("catch_data2.csv")
# glm analysis
res_glm_catch <- glm(catch~as.factor(vessel)+temp-1,family = poisson(link=log),data = catch_data)
summary(res_glm_catch)

# check overdispersion by plot
plot(catch~temp,data=catch_data,ylim=c(0,max(catch_data$catch)))
par(new=T)
plot(res_glm_catch$fitted.values~catch_data$temp,col="red",ylim=c(0,max(catch_data$catch)))

# 船v1の結果を取り出す
catch_data_v1 <- subset(catch_data,catch_data$vessel=="v1")
res_glm_fitted_v1 <-res_glm_catch$fitted.values[which(catch_data$vessel=="v1")]

library(MASS)
CI_param_glm_catch <- confint(res_glm_catch)

CI_glm_catch_lwr<-c(exp(CI_param_glm_catch[1,1]+CI_param_glm_catch[4,1]*catch_data$temp[which(catch_data$vessel=="v1")]))
CI_glm_catch_upr<-c(exp(CI_param_glm_catch[1,2]+CI_param_glm_catch[4,2]*catch_data$temp[which(catch_data$vessel=="v1")]))

# obs data, fitted, and 95% CI
plot(catch_data_v1$temp,catch_data_v1$catch,ylim=c(0,max(catch_data_v1$catch)),xlab="Temperature",ylab="Catch")
par(new=T)
plot(catch_data_v1$temp,res_glm_fitted_v1,col="red",ylim=c(0,max(catch_data_v1$catch)),xlab="",ylab = "")
par(new=T)
plot(catch_data_v1$temp,CI_glm_catch_upr,col="blue",ylim=c(0,max(catch_data_v1$catch)),xlab="",ylab = "")
par(new=T)
plot(catch_data_v1$temp,CI_glm_catch_lwr,col="blue",ylim=c(0,max(catch_data_v1$catch)),xlab="",ylab = "")

