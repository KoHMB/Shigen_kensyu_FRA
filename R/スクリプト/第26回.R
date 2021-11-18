
# check df
summary(res_lm_catch) #lm case
res_glm_gala #glm case

# chi squire test
RPS <- c(40,20,40)
names(RPS_obs)<-c("Rock","Paper","Scissors")
chisq.test(RPS)

# read csv data
catch_data2 <- read.csv("xx")

# glm
res_glm_catch <- glm(catch~vessel+temp-1,family = poisson(link = "log"), data = catch_data2)

plot(catch_data2$temp,catch_data2$catch)


# check over dispersion
library(performance)
over_dispersion <- check_overdispersion(res_glm_catch)
