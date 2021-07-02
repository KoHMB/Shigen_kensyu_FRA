set.seed(1)
x <- load("/Users/shin-ichironakayama/Dropbox/r_practice/projectR/catch_data.rda")
null <- lm(log(catch) ~  1, data=catch_data)
full <- lm(log(catch) ~ vessel + temp + vessel:temp - 1, data=catch_data)

model_forward <- step(null, scope = list(lower = null, upper = full), direction="forward")
model_backward <- step(full, scope = list(lower = null, upper = full), direction="backward")
model_both1 <- step(null, scope = list(lower = null, upper = full), direction="both")
model_both2 <- step(full, scope = list(lower = null, upper = full), direction="both")

model_forward
model_backward
model_both1
model_both2


install.packages("MuMIn")
library(MuMIn)
options(na.action = "na.fail")
res_dredge <- dredge(full, rank=AIC)
res_dredge
model_best <- get.models(res_dredge, subset=1)
model_best
