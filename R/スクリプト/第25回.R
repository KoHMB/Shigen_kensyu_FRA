# 第12回作成データの読み込みと確認
load("catch_data.rda")
head(catch_data)

# lm
res_lm_catch <- lm(log(catch)~vessel+temp-1,data=catch_data)

# lm結果オブジェクトの中身確認
names(res_lm_catch)

# lm結果の予測値
(res_lm_catch$fitted.values)

# lm結果の保存
save(res_lm_catch,file = "res_lm_catch.rda")

#vessel=v1, temp=25での予測
new.data<-data.frame(vessel="v1",temp=25)
predict(res_lm_catch,newdata=new.data)

#vessel=c(v1,v2) temp=c(15,20,25,30)での予測
pred_target<-expand.grid(vessel=c("v1","v2"), temp=seq(15,30,by=5) )
pred_catch<-predict(res_lm_catch,newdata=pred_target)
pred_res <- cbind(pred_target, as.data.frame(pred_catch))

#係数推定に利用したデータ範囲での予測
pred.fitted <- predict(res_lm_catch)
# fitted.valuesとpredictとの比較
plot(pred.fitted,res_lm_catch$fitted.values) #理論的に1:1だがpredictの丸め誤差のズレは生じる

#交互作用ありのlm
res_lm_catch2 <- lm(log(catch)~vessel+temp+vessel:temp-1, data=catch_data)
#vessel=c(v1,v2) temp=c(15,20,25,30)での予測
pred_catch2 <-predict(res_lm_catch2,newdata=pred_target)
pred_res <- cbind(pred_res, as.data.frame(pred_catch2))

#船効果の抽出
tapply(pred_res$pred_catch2,pred_res$vessel,mean)
##あくまでlog(catch)に対する効果なので、catchに対する効果を取り出すときはバイアス補正をすること（第23回参照）

#係数推定に利用したデータ範囲で95%の信頼区間
CI95 <- predict(res_lm_catch,interval = 'confidence')

#確認
dim(CI95)
head(CI95)

#vessel1について温度を横軸にlog(catch)を縦軸にプロット
CI95v1<-CI95[which(catch_data$vessel=="v1"),]
yrange=c(min(CI95v1[,2]),max(CI95v1[,3]))
plot(catch_data$temp[which(catch_data$vessel=="v1",)],CI95v1[,1],xlab="temp",ylab="log(catch)",ylim=yrange,type="l")
par(new=T)
plot(catch_data$temp[which(catch_data$vessel=="v1",)],CI95v1[,2],type="l",col="red",xlab="",ylab="",ylim=yrange)
par(new=T)
plot(catch_data$temp[which(catch_data$vessel=="v1",)],CI95v1[,3],type="l",col="red",xlab="",ylab="",ylim=yrange)

#95%予測区間の場合
PI95 <- predict(res_lm_catch,interval = 'prediction')

#確認
head(PI95)

#90%予測区間の場合
PI90 <- predict(res_lm_catch,interval = 'prediction',level = 0.90)

#vessel1について温度を横軸にlog(catch)を縦軸にプロット
PI95v1<-PI95[which(catch_data$vessel=="v1"),]
yrange2=c(min(PI95v1[,2]),max(PI95v1[,3]))
plot(catch_data$temp[which(catch_data$vessel=="v1",)],log(catch_data$catch[which(catch_data$vessel=="v1",)]),xlab="temp",ylab="log(catch)",ylim=yrange)
par(new=T)
plot(catch_data$temp[which(catch_data$vessel=="v1",)],PI95v1[,2],type="l",col="blue",xlab="",ylab="",ylim=yrange2)
par(new=T)
plot(catch_data$temp[which(catch_data$vessel=="v1",)],PI95v1[,3],type="l",col="blue",xlab="",ylab="",ylim=yrange2)
#vessel1について目的変数の予測値とその信頼区間も一緒にプロット
par(new=T)
plot(catch_data$temp[which(catch_data$vessel=="v1",)],CI95v1[,1],xlab="",ylab="",ylim=yrange2,type="l")
par(new=T)
plot(catch_data$temp[which(catch_data$vessel=="v1",)],CI95v1[,2],type="l",col="red",xlab="",ylab="",ylim=yrange2)
par(new=T)
plot(catch_data$temp[which(catch_data$vessel=="v1",)],CI95v1[,3],type="l",col="red",xlab="",ylab="",ylim=yrange2)

#90%予測区間も
PI90v1<-PI90[which(catch_data$vessel=="v1"),]
par(new=T)
plot(catch_data$temp[which(catch_data$vessel=="v1",)],PI90v1[,2],type="l",col="green",xlab="",ylab="",ylim=yrange2)
par(new=T)
plot(catch_data$temp[which(catch_data$vessel=="v1",)],PI90v1[,3],type="l",col="green",xlab="",ylab="",ylim=yrange2)


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
