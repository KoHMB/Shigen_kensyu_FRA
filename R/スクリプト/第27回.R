data(gala,package = "faraway")
names(gala)

# glm
res_glm_gala <- glm(Species~. - Endemics, family=poisson(link = "log"),data=gala)

# check glm result and deviance
summary(res_glm_gala)

resid_glm_gala <- residuals(res_glm_gala,type = "deviance")
plot(resid_glm_gala)
