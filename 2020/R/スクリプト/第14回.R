x <- load("/Users/shin-ichironakayama/Dropbox/r_practice/projectR/res_lm.rda")
x <- load("/Users/shin-ichironakayama/Dropbox/r_practice/projectR/catch_data.rda")

names(res)
res$residuals

hist(res$residuals)

plot(res$fitted.values, res$residuals)
plot(catch_data$temp, res$residuals)
plot(catch_data$vessel, res$residuals)
