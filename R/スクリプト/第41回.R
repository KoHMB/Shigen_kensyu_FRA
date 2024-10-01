#install.packages("mgcv")
library("mgcv")
library("tidyverse")
library("GGally")
library("MuMIn")
options(na.action="na.fail")

data <- read_csv("/Users/shin-ichironakayama/Dropbox/r_practice/projectR/cpuestandardization.csv")
head(data)
tail(data)
ggpairs(data)
boxplot(data$cpue)
min(data$cpue)

full <- gam(cpue~factor(year)+s(lon)+s(lat)+s(lon,lat), family=Gamma(link="log"), data=data)
summary(full)
selected <- dredge(full, rank="AIC")
selected

best <- gam(cpue~factor(year)+s(lon)+s(lon,lat), family=Gamma(link="log"), data=data)
summary(best)
plot(best, se=T)
vis.gam(best, view=c("lon", "lat"), theta=-50)

plot(best$coefficients[2:27])
