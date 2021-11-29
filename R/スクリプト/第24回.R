
# read data
meteo_data <- read.csv("yokohama.csv")
# edit data
meteo_data <- meteo_data[-1,]
plot(meteo_data$T)
head(meteo_data)
dim(meteo_data)
names(meteo_data)
class(meteo_data$year)

# plotしてチェック
# plot(meteo_data$T)
# 型がfactorになってしまっている場合は一旦characterに変換
#for(i in 1:20){
#  meteo_data[,i]<-as.character(meteo_data[,i])
#}
#class(meteo_data$year)

# 解析に使う列のデータの型変換
for(i in 1:20){
  meteo_data[,i]<-as.numeric(meteo_data[,i])
}

meteo_data_forlm <- subset(meteo_data,(meteo_data$year>2009))
dim(meteo_data_forlm)
meteo_data_forlm <- meteo_data_forlm[,4:20]
names(meteo_data_forlm)

# lm
res_lm_meteor <- lm(Tw0~.,data=meteo_data_forlm)
summary(res_lm_meteor)

# library performance
library(performance)
check_collinearity(res_lm_meteor)
