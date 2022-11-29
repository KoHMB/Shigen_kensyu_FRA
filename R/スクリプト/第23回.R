data_num <- 10
year_list <- 2010:2020
month_effect <- c(1.5, 1.3, 1.1, 1, 1, 1, 1, 1, 1, 1, 1, 1.1, 1.3)
mean_total <- seq(1, 0.5, length.out=length(year_list))
ratio_a <- seq(0.2, 0.8, length.out=length(year_list))
mean_a <- mean_total*ratio_a
mean_b <- mean_total * (1 - ratio_a)
year <- c()
area <- c()
month <- c()
cpue <- c()
for(i in 1:length(year_list))
{
  for(j in 1:12)
  {
    year <- c(year, rep(year_list[i], data_num*2))
    month <- c(month, rep(j, data_num*2))
    area <- c(area, rep("a", data_num), rep("b", data_num))
    cpue_a <- exp(rnorm(data_num, mean_a[i]*month_effect[j], 0.5))
    cpue_b <- exp(rnorm(data_num, mean_b[i]*month_effect[j], 0.5))
    cpue <- c(cpue, cpue_a, cpue_b)
  }
}
data <- data.frame(year=year, area=area, month=month, cpue=cpue)
dim(data)
head(data)
tail(data)
index_truth <- exp(mean_a+0.5^2/2) + exp(mean_b+0.5^2/2) *2 
index_truth <- index_truth/mean(index_truth)
#write.csv(data, file="/Users/shin-ichironakayama/Dropbox/r_practice/projectR/cpuestandardization2.csv", row.names=F)



data <- read.csv("/Users/shin-ichironakayama/Dropbox/r_practice/projectR/cpuestandardization2.csv")
data_a <- data[data$area=="a",]
data_b <- data[data$area=="b",]
plot(data_a$year, log(data_a$cpue))
plot(data_b$year, log(data_b$cpue))

res <- glm(log(cpue)~factor(year)+area+factor(year)*area+factor(month), data=data)
summary(res)
newdata <- expand.grid(unique(data$year), unique(data$area), unique(data$month))
names(newdata) <- c("year", "area", "month")
pred <- predict(res, newdata=newdata)
newdata$pred <- pred
mean_a <- c()
mean_b <- c()
for(y in year_list)
{
  temp <- newdata[newdata$year==y,]
  temp_a <- temp[temp$area=="a",]
  temp_b <- temp[temp$area=="b",]
  mean_a <- c(mean_a, mean(exp(temp_a$pred + var(res$residuals)/2)))
  mean_b <- c(mean_b, mean(exp(temp_b$pred + var(res$residuals)/2)))
}
index <- mean_a + 2*mean_b
index <- index/mean(index)
index_a <- mean_a/mean(mean_a)
index_b <- mean_b/mean(mean_b)

year <- unique(data$year)
plot(year, index, pch=20, cex=2, xlab="", ylab="", ylim=c(0, 2))
par(new=T)
plot(year, index_a, pch=20, cex=2, xlab="", ylab="", ylim=c(0, 2), col="red")
par(new=T)
plot(year, index_b, pch=20, cex=2, xlab="", ylab="", ylim=c(0, 2), col="blue")
par(new=T)
plot(year, index_truth, pch=21, cex=3, xlab="", ylab="", ylim=c(0, 2))

