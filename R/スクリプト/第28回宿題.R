catch_data <- read.csv("catch_data2.csv")
# glm analysis
res_glm_catch2 <- glm(catch~as.factor(vessel)+temp-1,family = poisson(link=log),data = catch_data)
summary(res_glm_catch2)

# check overdispersion by plot
plot(catch~temp,data=catch_data,ylim=c(0,max(catch_data$catch)))
par(new=T)
plot(res_glm_catch2$fitted.values~catch_data$temp,col="red",ylim=c(0,max(catch_data$catch)))

# 船v1の結果を取り出す
catch_data_v1 <- subset(catch_data,catch_data$vessel=="v1")
res_glm_fitted_v1 <-res_glm_catch2$fitted.values[which(catch_data$vessel=="v1")]

PI_glm_catch_lwr <- qpois(res_glm_fitted_v1,p = 0.025)
PI_glm_catch_upr <- qpois(res_glm_fitted_v1,p = 0.975)

# obs data, fitted, and 95% PI
plot(catch_data_v1$temp,catch_data_v1$catch,ylim=c(0,max(catch_data_v1$catch)),xlab="Temperature",ylab="Catch")
par(new=T)
plot(catch_data_v1$temp,res_glm_fitted_v1,col="red",ylim=c(0,max(catch_data_v1$catch)),xlab="",ylab = "")
par(new=T)
plot(catch_data_v1$temp,PI_glm_catch_upr,col="blue",ylim=c(0,max(catch_data_v1$catch)),xlab="",ylab = "")
par(new=T)
plot(catch_data_v1$temp,PI_glm_catch_lwr,col="blue",ylim=c(0,max(catch_data_v1$catch)),xlab="",ylab = "")

