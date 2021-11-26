# 第12回作成データの読み込みと確認
load("catch_data.rda")
head(catch_data)

# lm
res_lm_catch <- lm(log(catch)~vessel+temp-1,data=catch_data)

# lmの結果の保存
save(res_lm_catch,file="./res_lm_catch.rda")

# lm結果オブジェクトの中身確認
names(res_lm_catch)

# lm結果の予測値
(res_lm_catch$fitted.values)

#vessel=v1, temp=25での予測
new.data<-data.frame(vessel="v1",temp=25)
predict(res_lm_catch,newdata=new.data)

#vessel=c(v1,v2) temp=c(15,20,25,30)での予測
pred_data<-expand.grid(vessel=c("v1","v2"), temp=seq(15,30,by=5) )
pred_catch<-predict(res_lm_catch,newdata=pred_data)
pred_res <- cbind(pred_data, as.data.frame(pred_catch))

#係数推定に利用したデータ範囲での予測
pred.fitted <- predict(res_lm_catch)
# fitted.valuesとpredictとの比較
plot(pred.fitted,res_lm_catch$fitted.values) #理論的に1:1だがpredictの丸め誤差のズレは生じる

# 交互作用ありのlm
res_lm2 <- lm(log(catch)~vessel+temp+vessel:temp-1, data=catch_data)
#vessel=c(v1,v2) temp=c(15,20,25,30)での予測
pred_catch2 <-predict(res_lm2,newdata=pred_data)
pred_res <- cbind(pred_res, as.data.frame(pred_catch2))

# 船効果の抽出
tapply(pred_res$pred_catch2,pred_res$vessel,mean)

#係数推定に利用したデータ範囲で95%の信頼区間
CI95 <- predict(res_lm_catch,interval = 'confidence')

#確認
CI95
