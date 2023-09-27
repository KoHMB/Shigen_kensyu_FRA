# install glmmTMB
# install.packages("glmmTMB")

catch_data3 <- read.csv("catch_data3.csv")
# glmmMLの推定結果とglmmTMBの結果の比較
library(glmmML)
res_glmm_catch <- glmmML(catch~vessel+temp,family = poisson(link = "log"), data = catch_data3, cluster = as.factor(area))
summary(res_glmm_catch)

library(glmmTMB)
res_glmm_catch2 <- glmmTMB(catch~vessel+temp+(1|area), data = catch_data3,family = poisson(link = "log"))
summary(res_glmm_catch2)
# 固定効果はおおよそ同じ（ランダム効果の分散推定結果はちょっと違う）

cpue_data3 <- read.csv("cpuestandardization3.csv")
# データの中身確認
head(cpue_data3)
nrow(cpue_data3)
unique(cpue_data3$vessel_id)
# ノミナルCPUEのトレンドチェック
nominal_cpue <- c()
for(y in 1:length(unique(cpue_data3$year))){
  temp_cpue <- cpue_data3[cpue_data3$year==unique(cpue_data3$year)[y],]
  nominal_cpue <- c(nominal_cpue,mean(temp_cpue$cpue))
}
plot(sort(unique(cpue_data3$year)),nominal_cpue)
# apply関数を使ってみる
nominal_cpue2<-tapply(cpue_data3$cpue, cpue_data3$year, mean)
par(new=T)
plot(sort(unique(cpue_data3$year)),nominal_cpue2,col="red")

# 船IDをランダム効果にしてglm
res_glmm_cpue <- glmmTMB(cpue~as.factor(year)+lon+lat+I(lon^2)+I(lat^2)+(1|vessel_id), data = cpue_data3,family = Gamma(link = "log"))
summary(res_glmm_cpue)

# 予測データセットを作成し、stdCPUEを算出してみる
pred_data <- expand.grid(year=sort(unique(cpue_data3$year)),lon=seq(min(cpue_data3$lon),max(cpue_data3$lon),length=20),lat=seq(min(cpue_data3$lat),max(cpue_data3$lat),length=20),vessel_id=sort(unique(cpue_data3$vessel_id)))
head(pred_data)
nrow(pred_data)
pred_data <- cbind(pred_data,cpue=predict(res_glmm_cpue,newdata = pred_data,type = "response"))
head(pred_data)
hist(pred_data$cpue)

std_cpue<-tapply(pred_data$cpue, pred_data$year, mean)

plot(sort(unique(cpue_data3$year)),nominal_cpue,ylim=c(min(nominal_cpue,std_cpue),max(nominal_cpue,std_cpue)),xlab="year",ylab="cpue")
par(new=T)
plot(sort(unique(cpue_data3$year)),std_cpue,col="red",ylim=c(min(nominal_cpue,std_cpue),max(nominal_cpue,std_cpue)),xlab="",ylab="")

# simulate関数で1000個のcatchを作成
res_sim=simulate(res_glmm_cpue,seed=1,nsim=1000)
# listで作成されるので、sim num=1 のcpueを確認（cpue_data3の行数のcpue）
res_sim[[1]]
length(res_sim[[1]])

# vesselの効果を取り出すため、予測データを作成
pred_data_sim <- expand.grid(year=sort(unique(cpue_data3$year)),lon=seq(min(cpue_data3$lon),max(cpue_data3$lon),length=20),lat=seq(min(cpue_data3$lat),max(cpue_data3$lat),length=20),vessel_id=sort(unique(cpue_data3$vessel_id)))
head(pred_data_sim)

# simulateの引数、nsim回数分の標準化CPUEが得られるので、それをsimIDごとにまとめてオブジェクトにする
# simごとに各年のcpueが算出されるので、項目をsimIDとする
std_cpues<-data.frame(std_cpue)
head(std_cpues)
names(std_cpues)<-c("simID_0")
head(std_cpues)
res_glmm_cpue_sim<-list()

load("./res_glmm_cpue_sim.rda")
for(i in 1:100){ # 1000回は時間がかかるのでここでは100でやめておく
  cpue_data_sim<-data.frame(cpue_sim=res_sim[[i]],cpue_data3)

  #res_glmm_cpue_sim[[i]] <- glmmTMB(cpue_sim~as.factor(year)+lon+lat+I(lon^2)+I(lat^2)+(1|vessel_id), data = cpue_data_sim,family = Gamma(link = "log")) 

  res_pred_data_sim <- cbind(pred_data_sim,cpue=predict(res_glmm_cpue_sim[[i]],newdata = pred_data_sim,type = "response"))
  std_cpue_sim <-c()
  std_cpue_sim <- as.data.frame(tapply(res_pred_data_sim$cpue,res_pred_data_sim$year,mean))
  names(std_cpue_sim)<-paste0("simID_",i)
  std_cpues <- cbind(std_cpues,std_cpue_sim)
}
head(std_cpues)
#save(res_glmm_cpue_sim,file="res_glmm_cpue_sim.rda")

# 各年ごとに平均、中央値、標準偏差、95％CIを計算する
mean(as.numeric(std_cpues[1,]))
apply(std_cpues,1,mean)
apply(std_cpues,1,function(x){quantile(x,probs=0.025)})

head(std_cpues)
# bootID_0はbootstrapではないので、summaryの計算からは除く
summary_sim_cpue <- data.frame(mean=apply(std_cpues[,-1],1,mean)
,median=apply(std_cpues[,-1],1,median),CI95lower=apply(std_cpues[,-1],1,function(x){quantile(x,probs=0.025)}),CI95upper=apply(std_cpues[,-1],1,function(x){quantile(x,probs=0.975)}))
summary_sim_cpue$year <-row.names(summary_sim_cpue)
head(summary_sim_cpue)
tail(summary_sim_cpue)

# プロットしてみる
yrange<-c(min(nominal_cpue,std_cpue),max(nominal_cpue,std_cpue))
plot(sort(unique(cpue_data3$year)),nominal_cpue,ylim=yrange,xlab="year",ylab="cpue")
par(new=T)
plot(sort(unique(cpue_data3$year)),std_cpue,col="red",ylim=yrange,xlab="",ylab="")
par(new=T)
plot(summary_sim_cpue$year,summary_sim_cpue$median,col="red",type="l",lty=1,ylim=yrange,xlab="",ylab="")
par(new=T)
plot(summary_sim_cpue$year,summary_sim_cpue$CI95lower,col="red",type="l",lty=2,ylim=yrange,xlab="",ylab="")
par(new=T)
plot(summary_sim_cpue$year,summary_sim_cpue$CI95upper,col="red",type="l",lty=2,ylim=yrange,xlab="",ylab="")

# 時系列の平均を1に規準化する
nominal_cpue_scaled <- nominal_cpue/mean(nominal_cpue)
std_cpue_scaled <- std_cpue/mean(std_cpue)
std_cpues_scaled <- std_cpues/apply(std_cpues,2,mean)
summary_sim_cpue_scaled <- data.frame(mean=apply(std_cpues_scaled[,-1],1,mean)
                               ,median=apply(std_cpues_scaled[,-1],1,median),CI95lower=apply(std_cpues_scaled[,-1],1,function(x){quantile(x,probs=0.025)}),CI95upper=apply(std_cpues_scaled[,-1],1,function(x){quantile(x,probs=0.975)}))
summary_sim_cpue_scaled$year <-row.names(summary_sim_cpue_scaled)

yrange<-c(0,max(nominal_cpue_scaled,std_cpue_scaled))
plot(sort(unique(cpue_data3$year)),nominal_cpue_scaled,ylim=yrange,xlab="year",ylab="cpue")
par(new=T)
plot(sort(unique(cpue_data3$year)),std_cpue_scaled,col="red",ylim=yrange,xlab="",ylab="")
par(new=T)
plot(summary_sim_cpue_scaled$year,summary_sim_cpue_scaled$median,col="red",type="l",lty=1,ylim=yrange,xlab="",ylab="")
par(new=T)
plot(summary_sim_cpue_scaled$year,summary_sim_cpue_scaled$CI95lower,col="red",type="l",lty=2,ylim=yrange,xlab="",ylab="")
par(new=T)
plot(summary_sim_cpue_scaled$year,summary_sim_cpue_scaled$CI95upper,col="red",type="l",lty=2,ylim=yrange,xlab="",ylab="")

# データブートストラップも試してみる
pred_data_boot <- expand.grid(year=sort(unique(cpue_data3$year)),lon=seq(min(cpue_data3$lon),max(cpue_data3$lon),length=20),lat=seq(min(cpue_data3$lat),max(cpue_data3$lat),length=20),vessel_id=sort(unique(cpue_data3$vessel_id)))

std_cpues2<-data.frame(std_cpue)
head(std_cpues2)
names(std_cpues2)<-c("bootID_0")
head(std_cpues2)
res_glmm_cpue_boot<-list()
#load("./res_glmm_cpue_boot.rda")
set.seed(1)
for(i in 1:100){
  print(i)
  resample_id <- sample(c(1:nrow(cpue_data3)), size=nrow(cpue_data3), replace=T)
  resample_data <- cpue_data3[resample_id,]
  res_glmm_cpue_boot[[i]] <- glmmTMB(cpue~as.factor(year)+lon+lat+I(lon^2)+I(lat^2)+(1|vessel_id), data = resample_data,family = Gamma(link = "log"))

  res_pred_data_boot<-c()
  res_pred_data_boot <- cbind(pred_data_boot,cpue=predict(res_glmm_cpue_boot[[i]],newdata = pred_data_boot,type = "response"))
  std_cpue_boot <-c()
  std_cpue_boot <- as.data.frame(tapply(res_pred_data_boot$cpue,res_pred_data_boot$year,mean))
  names(std_cpue_boot)<-paste0("bootID_",i)
  std_cpues2 <- cbind(std_cpues2,std_cpue_boot)
}
head(std_cpues2)
#save(res_glmm_cpue_sim,file="res_glmm_cpue_boot.rda")

summary_boot_cpue <- data.frame(mean=apply(std_cpues2[,-1],1,mean)
                               ,median=apply(std_cpues2[,-1],1,median),CI95lower=apply(std_cpues2[,-1],1,function(x){quantile(x,probs=0.025)}),CI95upper=apply(std_cpues2[,-1],1,function(x){quantile(x,probs=0.975)}))
summary_boot_cpue$year <-row.names(summary_boot_cpue)

# プロットしてみる
yrange<-c(min(nominal_cpue,std_cpue),max(nominal_cpue,std_cpue))
plot(sort(unique(cpue_data3$year)),nominal_cpue,ylim=yrange,xlab="year",ylab="cpue")
par(new=T)
plot(sort(unique(cpue_data3$year)),std_cpue,col="blue",ylim=yrange,xlab="",ylab="")
par(new=T)
plot(summary_boot_cpue$year,summary_boot_cpue$CI95lower,col="blue",type="l",lty=2,ylim=yrange,xlab="",ylab="")
par(new=T)
plot(summary_boot_cpue$year,summary_boot_cpue$CI95upper,col="blue",type="l",lty=2,ylim=yrange,xlab="",ylab="")

# 時系列の平均を1に規準化する
std_cpues2_scaled <- std_cpues2/apply(std_cpues2,2,mean)
summary_boot_cpue_scaled <- data.frame(mean=apply(std_cpues2_scaled[,-1],1,mean)
                                      ,median=apply(std_cpues2_scaled[,-1],1,median),CI95lower=apply(std_cpues2_scaled[,-1],1,function(x){quantile(x,probs=0.025)}),CI95upper=apply(std_cpues2_scaled[,-1],1,function(x){quantile(x,probs=0.975)}))
summary_boot_cpue_scaled$year <-row.names(summary_boot_cpue_scaled)

yrange<-c(0,max(nominal_cpue_scaled,std_cpue_scaled))
plot(sort(unique(cpue_data3$year)),nominal_cpue_scaled,ylim=yrange,xlab="year",ylab="cpue")
par(new=T)
plot(sort(unique(cpue_data3$year)),std_cpue_scaled,col="blue",ylim=yrange,xlab="",ylab="")
par(new=T)
plot(summary_sim_cpue_scaled$year,summary_sim_cpue_scaled$median,col="blue",type="l",lty=1,ylim=yrange,xlab="",ylab="")
par(new=T)
plot(summary_sim_cpue_scaled$year,summary_sim_cpue_scaled$CI95lower,col="blue",type="l",lty=2,ylim=yrange,xlab="",ylab="")
par(new=T)
plot(summary_sim_cpue_scaled$year,summary_sim_cpue_scaled$CI95upper,col="blue",type="l",lty=2,ylim=yrange,xlab="",ylab="")

