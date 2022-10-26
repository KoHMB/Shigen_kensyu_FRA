
catch_data <- read.csv("catch_data2.csv")

# glm analysis
res_glm_catch <- glm(catch~as.factor(vessel)+temp-1,family = poisson(link=log),data = catch_data)
summary(res_glm_catch)

# check overdispersion
library(performance)
check_overdispersion(res_glm_catch)

# 95% CI
CI_param_glm_catch <- confint(res_glm_catch)

pred_catch_data <- data.frame(vessel=catch_data$vessel,temp=catch_data$temp,obs=catch_data$catch,pred=res_glm_catch$fitted.values)

pred_catch_data$CI95_lwr<-c(exp(CI_param_glm_catch[1,1]+CI_param_glm_catch[4,1]*catch_data$temp[which(catch_data$vessel=="v1")]),exp(CI_param_glm_catch[2,1]+CI_param_glm_catch[4,1]*catch_data$temp[which(catch_data$vessel=="v2")]),exp(CI_param_glm_catch[3,1]+CI_param_glm_catch[4,1]*catch_data$temp[which(catch_data$vessel=="v3")]))
pred_catch_data$CI95_upr<-c(exp(CI_param_glm_catch[1,2]+CI_param_glm_catch[4,2]*catch_data$temp[which(catch_data$vessel=="v1")]),exp(CI_param_glm_catch[2,2]+CI_param_glm_catch[4,2]*catch_data$temp[which(catch_data$vessel=="v2")]),exp(CI_param_glm_catch[3,2]+CI_param_glm_catch[4,2]*catch_data$temp[which(catch_data$vessel=="v3")]))


pred_catch_data$PI95_lwr <- qpois(res_glm_catch$fitted.values,p = 0.025)
pred_catch_data$PI95_upr <- qpois(res_glm_catch$fitted.values,p = 0.975)

head(pred_catch_data)

library(RColorBrewer)
plot_data <- function(data,interval="CI"){
  par(oma=c(1,1,0,0))
  col <- brewer.pal(length(unique(data$vessel)),"Set1")
  i <- 1
  for(v in unique(data$vessel)){
    data_tmp <- data[data$vessel==v,]
    data_tmp <- data_tmp[order(data_tmp$temp),]
    if(i > 1) par(new=T)
    plot(data_tmp$temp, data_tmp$obs, xlim=c(min(data$temp),max(data$temp)), ylim=c(min(data$obs), max(data$obs)), col=col[i], xlab="", ylab="", type="p", pch=21, cex=0.5)
    par(new=T)
    plot(data_tmp$temp, data_tmp$pred, xlim=c(min(data$temp),max(data$temp)), ylim=c(min(data$obs), max(data$obs)), col=col[i], xlab="", ylab="", type="l")
    if(interval=="CI"){
      par(new=T)
      plot(data_tmp$temp, data_tmp$CI95_upr, xlim=c(min(data$temp),max(data$temp)), ylim=c(min(data$obs), max(data$obs)), col=col[i], xlab="", ylab="", type="l",lty ="dashed")
      par(new=T)
      plot(data_tmp$temp, data_tmp$CI95_lwr, xlim=c(min(data$temp),max(data$temp)), ylim=c(min(data$obs), max(data$obs)), col=col[i], xlab="", ylab="", type="l",lty ="dashed")
    }else if(interval=="PI"){
      par(new=T)
      plot(data_tmp$temp, data_tmp$PI95_upr, xlim=c(min(data$temp),max(data$temp)), ylim=c(min(data$obs), max(data$obs)), col=col[i], xlab="", ylab="", type="l",lty ="dashed")
      par(new=T)
      plot(data_tmp$temp, data_tmp$PI95_lwr, xlim=c(min(data$temp),max(data$temp)), ylim=c(min(data$obs), max(data$obs)), col=col[i], xlab="", ylab="", type="l",lty ="dashed")
      
    }
    i <- i+1
  }
  mtext("Temperature", side=1, cex=1.2, outer=T)
  mtext("Catch", side=2, cex=1.2, outer=T)
}

plot_data(pred_catch_data)

plot_data(pred_catch_data,interval = "PI")
