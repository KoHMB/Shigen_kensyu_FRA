# glm catch_data2
# read catch data
catch_data2 <- read.csv("catch_data2.csv")
head(catch_data2)
# glm; catch ~ vessel + temp + area
res_glm_catch <- glm(catch~as.factor(vessel)+temp+as.factor(area)-1,family = poisson(link="log"),data=catch_data2)
summary(res_glm_catch)
res_glm_catch$aic

# count data# by each area
barplot(table(catch_data2$area))

# glm; catch ~ vessel + temp
res_glm_catch2 <- glm(catch~as.factor(vessel)+temp-1,family = poisson(link="log"),data=catch_data2)
res_glm_catch2$aic
