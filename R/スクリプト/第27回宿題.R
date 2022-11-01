# glm catch_data2
# read catch data
catch_data <- read.csv("catch_data2.csv")

# glm; catch ~ vessel + temp + area
res_glm_catch <- glm(catch~as.factor(vessel)+temp+as.factor(area)-1,family = poisson(link="log"),data=catch_data)
summary(res_glm_catch)
res_glm_catch$aic

library(MuMIn)
options(na.action="na.fail")
dredge_glm_catch<-dredge(res_glm_catch,rank=AIC)
res_dredge_glm_catch<-get.models(dredge_glm_catch,subset=1)
summary(res_dredge_glm_catch$'8')

# count data# by each area
barplot(table(catch_data$area))

# glm; catch ~ vessel + temp
res_glm_catch2 <- glm(catch~as.factor(vessel)+temp-1,family = poisson(link="log"),data=catch_data)
res_glm_catch2$aic
