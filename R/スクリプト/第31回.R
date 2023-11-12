# データ読み込み
cpue_data_incldzero <- read.csv("./cpuestandardization_zero.csv")
# データの確認
head(cpue_data_incldzero)
dim(cpue_data_incldzero)

# ゼロデータの数を確認
length(which(cpue_data_incldzero$cpue==0))
# ノンゼロデータの数を確認
nrow(cpue_data_incldzero)-length(which(cpue_data_incldzero$cpue==0))

# 有漁確率を求めるため、ゼロデータ/有漁データを0/1で区別したものを目的変数とするロジスティック回帰
cpue_data_incldzero$cpue_01<-ifelse(cpue_data_incldzero$cpue>0, 1, 0)

binorm_cpue_full <- glm(cpue_01 ~ as.factor(year)+lon+lat+I(lon^2)+I(lat^2)+lon:lat-1,family = binomial(link = "logit"), data = cpue_data_incldzero)
summary(binorm_cpue_full)

# モデル選択
library(MuMIn)
options(na.action = "na.fail")

binom_cpue_select <- dredge(binorm_cpue_full, rank = "AIC")
binom_cpue_best <- get.models(binom_cpue_select, subset = 1)[[1]]
summary(binom_cpue_best)

# ２段階目はキャッチがあったデータのみデータを対象にするので抽出
positive_cpue_data <- subset(cpue_data_incldzero, cpue_data_incldzero$cpue> 0); nrow(positive_cpue_data)
# 目的変数をcpueとして誤差構造をガンマ分布、リンク関数をlogでGLM
gamma_cpue_full <- glm(cpue ~ as.factor(year)+lon+lat+I(lon^2)+I(lat^2)+lon:lat-1,family = Gamma(link = "log"), data = positive_cpue_data)
summary(gamma_cpue_full)

# モデル選択
gamma_cpue_select <- dredge(gamma_cpue_full, rank = "AIC")
gamma_cpue_best <- get.models(gamma_cpue_select, subset = 1)[[1]]
summary(gamma_cpue_best)

# 予測値からトレンド抽出 
pred_data <- expand.grid(year=sort(unique(cpue_data_incldzero$year)),lon=seq(min(cpue_data_incldzero$lon),max(cpue_data_incldzero$lon),length=20),lat=seq(min(cpue_data_incldzero$lat),max(cpue_data_incldzero$lat),length=20))

pred_data$prob <- predict(binom_cpue_best,newdata = pred_data,type = "response")
pred_data$positive <- predict(gamma_cpue_best,newdata = pred_data,type="response")
pred_data$cpue_pred <- pred_data$positive*pred_data$prob

# 年ごとで取り出し
prob_cpue_trend <- tapply(pred_data$prob, pred_data$year, mean)
posi_cpue_trend <- tapply(pred_data$positive, pred_data$year, mean)
cpue_pred_trend <- tapply(pred_data$cpue_pred, pred_data$year, mean)

# ノミナルCPUEの計算
nominal_CPUE <- tapply(cpue_data_incldzero$cpue, cpue_data_incldzero$year, mean)
nominal_binom <- tapply(cpue_data_incldzero$cpue_01, cpue_data_incldzero$year, mean)
nominal_2ndstep <- tapply(positive_cpue_data$cpue, positive_cpue_data$year, mean)

# ノミナル、標準化CPUEと有漁確率、有漁CPUEそれぞれをまとめる
delta2step_cpuestd_trends <- rbind(
                             Nominal_CPUE = nominal_CPUE,
                             Nominal_binom = nominal_binom,
                             nominal_2ndstep = nominal_2ndstep,
                             Predict_CPUE = cpue_pred_trend,
                             Predict_binom = prob_cpue_trend,
                             Predict_2ndstep = posi_cpue_trend)
# 時系列の平均を1に規準化
delta2step_cpuestd_trends_scaled <- delta2step_cpuestd_trends/apply(delta2step_cpuestd_trends, 1, mean)

delta2step_cpuestd_trends_scaled

plot(sort(unique(pred_data$year)),delta2step_cpuestd_trends_scaled[1,],ylim=c(0,max(delta2step_cpuestd_trends_scaled)),ylab="cpue",xlab="year")
par(new=T)
plot(sort(unique(pred_data$year)),delta2step_cpuestd_trends_scaled[4,],ylim=c(0,max(delta2step_cpuestd_trends_scaled)),col="red",ylab = "",xlab="")


# CI derivation by bootstrap 以下は自分で回して見てください
# データブートストラップを行なってリサンプルデータを1000個得る
pred_data_boot <- expand.grid(year=sort(unique(cpue_data_incldzero$year)),lon=seq(min(cpue_data_incldzero$lon),max(cpue_data_incldzero$lon),length=20),lat=seq(min(cpue_data_incldzero$lat),max(cpue_data_incldzero$lat),length=20))

res_delta2step_cpue_boot <- as.data.frame(delta2step_cpuestd_trends[4,])
names(res_delta2step_cpue_boot) <- "bootID_0"
res_delta2step_cpue_boot_scaled <- as.data.frame(delta2step_cpuestd_trends_scaled[4,])
names(res_delta2step_cpue_boot_scaled)<- "bootID_0"
set.seed(1)
for(i in 1:100){
  print(i)
  resample_id <- sample(c(1:nrow(cpue_data_incldzero)), size=nrow(cpue_data_incldzero), replace=T)
  resample_data <- cpue_data_incldzero[resample_id,]
  resample_data$cpue_01<-ifelse(resample_data$cpue>0, 1, 0)
  positive_cpue_resample_data <- subset(resample_data, resample_data$cpue>0)
  #summary(binom_cpue_best)
  res_binorm_cpue_boot <- glm(cpue_01~as.factor(year)+ lat + I(lat^2) + 
                      lon + I(lon^2), family = binomial(link = "logit"), data = resample_data)
  #summary(gamma_cpue_best)
  res_gamma_cpue_boot <- glm(cpue~as.factor(year) + lat + I(lat^2) + lon + 
           I(lon^2) + lat:lon, family = Gamma(link = "log"), data = positive_cpue_resample_data)
  
  res_pred_data_boot <- pred_data_boot
  
  res_pred_data_boot$prob <- predict(res_binorm_cpue_boot,newdata = pred_data_boot,type = "response")
  res_pred_data_boot$positive <- predict(res_gamma_cpue_boot,newdata = pred_data_boot,type="response")
  res_pred_data_boot$cpue_pred <- res_pred_data_boot$positive*res_pred_data_boot$prob
  
  # 年ごとで取り出し
  res_pred_cpue_boot_trend <- as.data.frame(tapply(res_pred_data_boot$cpue_pred, res_pred_data_boot$year, mean))
  names(res_pred_cpue_boot_trend) <- paste0("bootID_",i)
  res_delta2step_cpue_boot <- cbind(res_delta2step_cpue_boot, res_pred_cpue_boot_trend)
  res_delta2step_cpue_boot_scaled <- cbind(res_delta2step_cpue_boot_scaled,res_pred_cpue_boot_trend/mean(res_pred_cpue_boot_trend[,1]))
}

# bootID_0はbootstrapではないので、summaryの計算からは除く
summary_delta2step_boot_cpue <- data.frame(mean=apply(res_delta2step_cpue_boot[,-1],1,mean)
                               ,median=apply(res_delta2step_cpue_boot[,-1],1,median),CI95lower=apply(res_delta2step_cpue_boot[,-1],1,function(x){quantile(x,probs=0.025)}),CI95upper=apply(res_delta2step_cpue_boot[,-1],1,function(x){quantile(x,probs=0.975)}))
summary_delta2step_boot_cpue$year <-row.names(summary_boot_cpue)

# プロットしてみる
yrange<-c(min(nominal_cpue,std_cpue),max(nominal_cpue,std_cpue))
plot(sort(unique(cpue_data2$year)),nominal_cpue,ylim=yrange,xlab="year",ylab="cpue")
par(new=T)
plot(sort(unique(cpue_data2$year)),std_cpue,col="red",ylim=yrange,xlab="",ylab="")
par(new=T)
plot(summary_delta2step_boot_cpue$year,summary_delta2step_boot_cpue$median,col="red",type="l",lty=1,ylim=yrange,xlab="",ylab="")
par(new=T)
plot(summary_delta2step_boot_cpue$year,summary_delta2step_boot_cpue$CI95lower,col="red",type="l",lty=2,ylim=yrange,xlab="",ylab="")
par(new=T)
plot(summary_delta2step_boot_cpue$year,summary_delta2step_boot_cpue$CI95upper,col="red",type="l",lty=2,ylim=yrange,xlab="",ylab="")

# 時系列の平均を1に規準化する
nominal_cpue_scaled <- delta2step_cpuestd_trends_scaled[1,]
std_cpue_scaled <- delta2step_cpuestd_trends_scaled[4,]
res_delta2step_cpue_boot_scaled<-res_delta2step_cpue_boot/apply(res_delta2step_cpue_boot,2,mean)
summary_delta2step_boot_cpue_scaled <- data.frame(mean=apply(res_delta2step_cpue_boot_scaled[,-1],1,mean)
                                      ,median=apply(res_delta2step_cpue_boot_scaled[,-1],1,median),CI95lower=apply(res_delta2step_cpue_boot_scaled[,-1],1,function(x){quantile(x,probs=0.025)}),CI95upper=apply(res_delta2step_cpue_boot_scaled[,-1],1,function(x){quantile(x,probs=0.975)}))
summary_delta2step_boot_cpue_scaled$year <-row.names(summary_delta2step_boot_cpue_scaled)

# プロットしてみる
yrange<-c(0,max(nominal_cpue_scaled,std_cpue_scaled))
plot(sort(unique(cpue_data2$year)),nominal_cpue_scaled,ylim=yrange,xlab="year",ylab="cpue")
par(new=T)
plot(sort(unique(cpue_data2$year)),std_cpue_scaled,col="red",ylim=yrange,xlab="",ylab="")
par(new=T)
plot(summary_delta2step_boot_cpue_scaled$year,summary_delta2step_boot_cpue_scaled$median,col="red",type="l",lty=1,ylim=yrange,xlab="",ylab="")
par(new=T)
plot(summary_delta2step_boot_cpue_scaled$year,summary_delta2step_boot_cpue_scaled$CI95lower,col="red",type="l",lty=2,ylim=yrange,xlab="",ylab="")
par(new=T)
plot(summary_delta2step_boot_cpue_scaled$year,summary_delta2step_boot_cpue_scaled$CI95upper,col="red",type="l",lty=2,ylim=yrange,xlab="",ylab="")

