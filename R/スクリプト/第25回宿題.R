data(gala,package = "faraway")
names(gala)

# glm
res_glm_gala <- glm(Species~. - Endemics, family=poisson(link = "log"),data=gala)

# 95% CI parameters
library(MASS)
CI95_param_glm_gala <- confint(res_glm_gala)
CI95_param_glm_gala[1,1]
CI95_param_glm_gala[2,1]

# 95% CI (lower)
CI95_lwr_gala <- exp(CI95_param_glm_gala[1,1]+CI95_param_glm_gala[2,1]*gala$Area+CI95_param_glm_gala[3,1]*gala$Elevation+CI95_param_glm_gala[4,1]*gala$Nearest+CI95_param_glm_gala[5,1]*gala$Scruz+CI95_param_glm_gala[6,1]*gala$Adjacent)

# 95% CI (upper)
CI95_upr_gala <- exp(CI95_param_glm_gala[1,2]+CI95_param_glm_gala[2,2]*gala$Area+CI95_param_glm_gala[3,2]*gala$Elevation+CI95_param_glm_gala[4,2]*gala$Nearest+CI95_param_glm_gala[5,2]*gala$Scruz+CI95_param_glm_gala[6,2]*gala$Adjacent)

# plot
plot(res_glm_gala$fitted.values,ylim=c(0,max(CI95_upr_gala)),ylab="")
par(new=T)
plot(CI95_lwr_gala,col="red",ylim=c(0,max(CI95_upr_gala)),ylab="")
par(new=T)
plot(CI95_upr_gala,col="blue",ylim=c(0,max(CI95_upr_gala)),ylab="")

# plot against log(Area)
plot(log(gala$Area),res_glm_gala$fitted.values,ylim=c(0,max(CI95_upr_gala)),ylab="")
par(new=T)
plot(log(gala$Area),CI95_lwr_gala,col="red",ylim=c(0,max(CI95_upr_gala)),ylab="")
par(new=T)
plot(log(gala$Area),CI95_upr_gala,col="blue",ylim=c(0,max(CI95_upr_gala)),ylab="")


# 95% PI
PI95_lwr_gala <- qpois(res_glm_gala$fitted.values,p = 0.025)
PI95_upr_gala <- qpois(res_glm_gala$fitted.values,p = 0.975)

par(new=T)
plot(log(gala$Area),PI95_lwr_gala,col="green",ylim=c(0,max(CI95_upr_gala)),ylab="")
par(new=T)
plot(log(gala$Area),PI95_upr_gala,col="green",ylim=c(0,max(CI95_upr_gala)),ylab="")

#予測区間よりも信頼区間の方が広い？なぜ？第28回（過分散）へ
