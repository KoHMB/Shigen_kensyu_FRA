# lm で信頼区間を示す
# 第12回作成データの読み込みと確認
load("catch_data.rda")
head(catch_data)
# lm
res_lm_catch <- lm(log(catch)~vessel+temp-1,data=catch_data)
# 95%PI
PI95 <-predict(res_lm_catch, interval="prediction")
#確認
head(PI95)

#vessel1について温度を横軸にlog(catch)を縦軸にプロット
PI95v1<-PI95[which(catch_data$vessel=="v1"),]
yrange=c(min(PI95v1[,2]),max(PI95v1[,3]))

plot(catch_data$temp[which(catch_data$vessel=="v1",)],log(catch_data$catch[which(catch_data$vessel=="v1",)]),xlab="temp",ylab="log(catch)",ylim=yrange)
par(new=T)
plot(catch_data$temp[which(catch_data$vessel=="v1",)],PI95v1[,2],type="l",col="blue",xlab="",ylab="",ylim=yrange)
par(new=T)
plot(catch_data$temp[which(catch_data$vessel=="v1",)],PI95v1[,3],type="l",col="blue",xlab="",ylab="",ylim=yrange)
par(new=T)
plot(catch_data$temp[which(catch_data$vessel=="v1",)],PI95v1[,1],type="l",col="red",xlab="",ylab="",ylim=yrange)

#予測区間を手動で計算
pred.pi<-expand.grid(vessel="v1",temp=unique(catch_data$temp) )

pred.pi$pred <- predict(res_lm_catch,newdata = pred.pi)
pred.pi$pilwr <- qnorm(mean = (res_lm_catch$coefficients[1] + pred.pi$temp*res_lm_catch$coefficients[4]), sd = sqrt(var(res_lm_catch$residuals)), p = 0.025 ) 
#pred.pi$pilwr <- qnorm(mean = res_lm_catch$fitted.values, sd = sqrt(var(res_lm_catch$residuals)), p = 0.025 ) 
pred.pi$piupr <- qnorm(mean = (res_lm_catch$coefficients[1] + pred.pi$temp*res_lm_catch$coefficients[4]), sd = sqrt(var(res_lm_catch$residuals)), p = 0.975 ) 
#pred.pi$piupr <- qnorm(mean = res_lm_catch$fitted.values, sd = sqrt(var(res_lm_catch$residuals)), p = 0.975 ) 

xrangepi<-c(min(pred.pi$temp),max(pred.pi$temp))
plot(catch_data$temp[which(catch_data$vessel=="v1",)],log(catch_data$catch[which(catch_data$vessel=="v1",)]),xlab="temp",ylab="log(catch)",xlim=xrangepi,ylim=yrange)
par(new=T)
plot(pred.pi$temp,pred.pi$pred,type="l",xlim=xrangepi,ylim=yrange,xlab="",ylab="")
par(new=T)
plot(pred.pi$temp,pred.pi$pilwr,type="l",lwd=3,col='red',xlim=xrangepi,ylim=yrange,xlab="",ylab="")
par(new=T)
plot(pred.pi$temp,pred.pi$piupr,type="l",lwd=3,col='red',xlim=xrangepi,ylim=yrange,xlab="",ylab="")

#predictの結果と合っているか確認
par(new=T)
plot(catch_data$temp[which(catch_data$vessel=="v1",)],PI95v1[,2],type="l",col="blue",xlab="",ylab="",xlim=xrangepi, ylim=yrange)
par(new=T)
plot(catch_data$temp[which(catch_data$vessel=="v1",)],PI95v1[,3],type="l",col="blue",xlab="",ylab="",xlim=xrangepi, ylim=yrange)

# GLMで95%PI
data(gala,package = "faraway")
names(gala)
# glm
res_glm_gala <- glm(Species~. - Endemics, family=poisson(link = "log"),data=gala)
# 95% PI
PI95_lwr_gala <- qpois(res_glm_gala$fitted.values,p = 0.025)
PI95_upr_gala <- qpois(res_glm_gala$fitted.values,p = 0.975)

plot(log(gala$Area),res_glm_gala$fitted.values,ylim=c(0,max(PI95_upr_gala)),ylab="")
par(new=T)
plot(log(gala$Area),PI95_lwr_gala,col="green",ylim=c(0,max(PI95_upr_gala)),ylab="")
par(new=T)
plot(log(gala$Area),PI95_upr_gala,col="green",ylim=c(0,max(PI95_upr_gala)),ylab="")


# check df
summary(res_lm_catch) #lm case
res_glm_gala #glm case

# chi squire test
RPS <- c(40,20,40)
names(RPS_obs)<-c("Rock","Paper","Scissors")
chisq.test(RPS)

# read csv data
catch_data2 <- read.csv("catch_data2.csv")

# glm（カテゴリで60個あるareaを説明変数に）
res_glm_catch <- glm(catch~vessel+temp+as.factor(area)-1,family = poisson(link = "log"), data = catch_data2)

# glm（areaを説明変数に入れず）
res_glm_catch2 <- glm(catch~vessel+temp-1,family = poisson(link = "log"), data = catch_data2)

# plotして予測値と実際の目的変数のばらつきをみる (95%PIは宿題)
plot(catch_data2$temp,catch_data2$catch,ylim = c(min(catch_data2$catch),max(catch_data2$catch)),xlab="temp",ylab="catch")
par(new=T)
plot(catch_data2$temp,res_glm_catch2$fitted.values,col="red",ylim = c(min(catch_data2$catch),max(catch_data2$catch)),xlab="",ylab="")

# check over dispersion
library(performance)
over_dispersion <- check_overdispersion(res_glm_catch)
over_dispersion2 <- check_overdispersion(res_glm_catch2)
