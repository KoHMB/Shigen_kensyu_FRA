calc_std_cpue <- function(data)
{
  res <- glm(log(cpue)~factor(year)+area+factor(year)*area+factor(month), data=data)
  summary(res)
  newdata <- expand.grid(unique(data$year), unique(data$area), unique(data$month))
  names(newdata) <- c("year", "area", "month")
  pred <- predict(res, newdata=newdata)
  newdata$pred <- pred
  mean_a <- c()
  mean_b <- c()
  for(y in unique(data$year))
  {
    temp <- newdata[newdata$year==y,]
    temp_a <- temp[temp$area=="a",]
    temp_b <- temp[temp$area=="b",]
    mean_a <- c(mean_a, mean(exp(temp_a$pred + var(res$residuals)/2)))
    mean_b <- c(mean_b, mean(exp(temp_b$pred + var(res$residuals)/2)))
  }
  index <- mean_a + 2*mean_b
}

data <- read.csv("/Users/shin-ichironakayama/Dropbox/r_practice/projectR/cpuestandardization2.csv")
cpue_mean <- calc_std_cpue(data)
res <- glm(log(cpue)~factor(year)+area+factor(year)*area+factor(month), data=data)

cpue_boot <- c()
for(i in 1:1000)
{
  cpue <- exp(simulate(res))
  names(cpue) <- "cpue"
  data_boot <- cbind(data[,1:3], cpue)
  cpue_boot <- rbind(cpue_boot, calc_std_cpue(data_boot))
}
cpue_boot <- cpue_boot/mean(cpue_mean)
cpue_mean <- cpue_mean/mean(cpue_mean)
conf_int <- data.frame()
for(i in  1:ncol(cpue_boot))
{
  temp <- quantile(cpue_boot[,i], c(0.025, 0.975))
  conf_int <- rbind(conf_int, temp)
}
names(conf_int) <- c("lower", "upper")

year <- unique(data$year)
plot(year, cpue_mean, pch=20, cex=2, xlab="", ylab="", ylim=c(0, 2))
par(new=T)
plot(year, conf_int$lower, type="l", xlab="", ylab="", ylim=c(0, 2), col="red")
par(new=T)
plot(year, conf_int$upper, type="l", xlab="", ylab="", ylim=c(0, 2), col="red")