
# check df
summary(res_lm_catch) #lm case
res_glm_gala #glm case

# chi squire test
RPS <- c(40,20,40)
names(RPS)<-c("Rock","Paper","Scissors")
chisq.test(RPS)

# read csv data
catch_data <- read.csv("catch_data2.csv")

# glm analysis
res_glm_catch <- glm(catch~as.factor(vessel)+temp-1,family = poisson(link=log),data = catch_data)

# check overdispersion
library(performance)
check_overdispersion(res_glm_catch)
