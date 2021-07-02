x <- load("/Users/shin-ichironakayama/Dropbox/r_practice/projectR/catch_data.rda")

lm(log(catch)~vessel+temp, data=catch_data)

lm(log(catch)~vessel+temp-1, data=catch_data)

res <- lm(log(catch)~vessel+temp-1, data=catch_data)
res
summary(res)

save(res, file="/Users/shin-ichironakayama/Dropbox/r_practice/projectR/res_lm.rda")
