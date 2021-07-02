x <- load("/Users/shin-ichironakayama/Dropbox/r_practice/projectR/catch_data.rda")

res1 <- lm(log(catch) ~ vessel + temp - 1, data=catch_data)
res2 <- lm(log(catch) ~ vessel + temp + vessel:temp - 1, data=catch_data)

summary(res1)
summary(res2)


plot_res <- function(res, catch_data)
{
  xlim <- c(min(catch_data$temp), max(catch_data$temp))
  ylim <- c(min(log(catch_data$catch)), max(log(catch_data$catch)))
  v1 <- which(catch_data$vessel=="v1")
  v2 <- which(catch_data$vessel=="v2")
  v3 <- which(catch_data$vessel=="v3")
  par(mfrow=c(1,1), oma=c(3,3,3,3))
  plot(catch_data$temp[v1], log(catch_data$catch[v1]), xlim=xlim, ylim=ylim, xlab="", ylab="", col="red")
  par(new=T)
  plot(catch_data$temp[v1], res$fitted.values[v1], xlim=xlim, ylim=ylim, xlab="", ylab="", col="red", type="l", lwd=2)
  par(new=T)
  plot(catch_data$temp[v2], log(catch_data$catch[v2]), xlim=xlim, ylim=ylim, xlab="", ylab="", col="blue")
  par(new=T)
  plot(catch_data$temp[v2], res$fitted.values[v2], xlim=xlim, ylim=ylim, xlab="", ylab="", col="blue", type="l", lwd=2)
  par(new=T)
  plot(catch_data$temp[v3], log(catch_data$catch[v3]), xlim=xlim, ylim=ylim, xlab="", ylab="", col="green")
  par(new=T)
  plot(catch_data$temp[v3], res$fitted.values[v3], xlim=xlim, ylim=ylim, xlab="", ylab="", col="green", type="l", lwd=2)
  mtext("Temperature", side=1, line=2)
  mtext("log(catch)", side=2, line=2)
}

plot_res(res1, catch_data)
par(new=T)
plot_res(res2, catch_data)

logLik(res1)
logLik(res2)
