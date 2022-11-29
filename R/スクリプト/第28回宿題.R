catch_data2 <- read.csv("catch_data2.csv")

# glm analysis
res_glm_catch <- glm(catch~as.factor(vessel)+temp+as.factor(area)-1,family = poisson(link=log),data = catch_data2)
summary(res_glm_catch)

res_glm_catch2 <- glm(catch~as.factor(vessel)+temp-1,family = poisson(link=log),data = catch_data2)
summary(res_glm_catch2)

# check overdispersion
library(performance)
check_overdispersion(res_glm_catch)
check_overdispersion(res_glm_catch2)

pred_catch_data <- data.frame(vessel=catch_data2$vessel,temp=catch_data2$temp,catch=catch_data2$catch,pred=res_glm_catch$fitted.values)

pred_catch_data2 <- data.frame(vessel=catch_data2$vessel,temp=catch_data2$temp,catch=catch_data2$catch,pred=res_glm_catch2$fitted.values)

# 95% PI
pred_catch_data$PI95lwr <- qpois(res_glm_catch$fitted.values,p = 0.025)
pred_catch_data$PI95upr <- qpois(res_glm_catch$fitted.values,p = 0.975)
head(pred_catch_data)
pred_catch_data2$PI95lwr <- qpois(res_glm_catch2$fitted.values,p = 0.025)
pred_catch_data2$PI95upr <- qpois(res_glm_catch2$fitted.values,p = 0.975)
head(pred_catch_data2)


library(RColorBrewer)
plot_data <- function(data){
  par(oma=c(1,1,0,0))
  col <- brewer.pal(length(unique(data$vessel)),"Set1")
  i <- 1
  for(v in unique(data$vessel)){
    data_tmp <- data[data$vessel==v,]
    data_tmp <- data_tmp[order(data_tmp$temp),]
    if(i > 1) par(new=T)
    plot(data_tmp$temp, data_tmp$catch, xlim=c(min(data$temp),max(data$temp)), ylim=c(min(data$catch), max(data$catch)), col=col[i], xlab="", ylab="", type="p", pch=21, cex=0.5)
    par(new=T)
    plot(data_tmp$temp, data_tmp$pred, xlim=c(min(data$temp),max(data$temp)), ylim=c(min(data$catch), max(data$catch)), col=col[i], xlab="", ylab="", type="l")

      par(new=T)
      plot(data_tmp$temp, data_tmp$PI95upr, xlim=c(min(data$temp),max(data$temp)), ylim=c(min(data$catch), max(data$catch)), col=col[i], xlab="", ylab="", type="l",lty ="dashed")
      par(new=T)
      plot(data_tmp$temp, data_tmp$PI95lwr, xlim=c(min(data$temp),max(data$temp)), ylim=c(min(data$catch), max(data$catch)), col=col[i], xlab="", ylab="", type="l",lty ="dashed")

    i <- i+1
  }
  mtext("Temperature", side=1, cex=1.2, outer=T)
  mtext("Catch", side=2, cex=1.2, outer=T)
}

plot_data(pred_catch_data2)
