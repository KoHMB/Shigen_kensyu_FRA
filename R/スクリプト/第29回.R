# read data fram csv file
catch_data2 <- read.csv("catch_data2.csv")
barplot(table(catch_data2$area))

# areaを含んだglm
res_glm_catch <- glm(catch~as.factor(vessel)+temp+as.factor(area)-1,family = poisson(link=log),data = catch_data2)

pred_catch_data <- expand.grid(vessel=unique(catch_data2$vessel),temp=seq(min(catch_data2$temp),max(catch_data2$temp),length=20),area=unique(catch_data2$area))
pred_catch_data$pred <- predict(res_glm_catch,newdata = pred_catch_data,type = "response") 

# vessel1について温度を横軸にcatchを縦軸にプロット
pred_catch_datav1<-pred_catch_data[which(pred_catch_data$vessel=="v1"),]
# plotの軸のデータ範囲
yrange=c(min(pred_catch_datav1$pred),max(pred_catch_datav1$pred))
xrange=c(min(pred_catch_datav1$temp),max(pred_catch_datav1$temp))
# 目的変数のプロット
plot(catch_data2$temp[which(catch_data2$vessel=="v1",)],catch_data2$catch[which(catch_data2$vessel=="v1",)],xlab="temp",ylab="catch",ylim=yrange,xlim=xrange)

library(RColorBrewer)
colPal3 <- colorRampPalette(brewer.pal(11, "Spectral"))
col= colPal3(length(unique(catch_data2$area)))
# 60本の線を入れるので時間がかかります
for(i in 1:length(unique(catch_data2$area))){
  par(new=T)
  plot(pred_catch_datav1$temp[which(pred_catch_datav1$area==i)],pred_catch_datav1$pred[which(pred_catch_datav1$area==i)],type="l",col=col[i],lwd=0.5,xlab="",ylab="",ylim=yrange,xlim=xrange)
}


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

