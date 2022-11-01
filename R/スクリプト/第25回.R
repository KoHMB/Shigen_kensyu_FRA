# read data
meteo_data <- read.csv("yokohama.csv")
# check & edit data
head(meteo_data)
meteo_data <- meteo_data[-1,]
plot(meteo_data$T)
head(meteo_data)
dim(meteo_data)
names(meteo_data)
class(meteo_data$year)

# plotしてチェック
# plot(meteo_data$T)
# 型がfactorになってしまっている場合は一旦characterに変換
#for(i in 1:ncol(meteo_data)){
#  meteo_data[,i]<-as.character(meteo_data[,i])
#}
#class(meteo_data$year)

# 解析に使う列のデータの型変換
for(i in 1:ncol(meteo_data)){
  meteo_data[,i]<-as.numeric(meteo_data[,i])
}

# 解析対象となる年のみ取り出す
meteo_data_forlm <- subset(meteo_data,meteo_data$year>2009)

# 解析対象となる変数のみ取り出す c("Tw0","T","Tmax","Tmin","Pr","N")
meteo_data_forlm <- data.frame(T=meteo_data_forlm$T,Tmax=meteo_data_forlm$Tmax,Tmin=meteo_data_forlm$Tmin,Pr=meteo_data_forlm$Pr,N=meteo_data_forlm$N,Tw0=meteo_data_forlm$Tw0)

dim(meteo_data_forlm)
names(meteo_data_forlm)

# 対象となる変数の抽出はライブラリdplyrのfilter関数やselect関数をパイプ演算子 %>% でつないでいっぺんに取り出せる
# library(dplyr)
# meteo_data_forlm <- meteo_data %>% filter(year > 2009) %>% select(Tw0,T,Tmax,Tmin,Pr,N)
# head(meteo_data_forlm)

#欠測値がこのデータベースだと-99999が入っているので、解析の対象となる変数について欠測値があるレコードを除去。
for(i in 1:nrow(meteo_data_forlm)){
  for(j in 1:ncol(meteo_data_forlm)){
    if(meteo_data_forlm[i,j]==-99999) meteo_data_forlm[i,j]<-NA
  }
}
meteo_data_forlm <- na.omit(meteo_data_forlm)
head(meteo_data_forlm)

plot(meteo_data_forlm)

# lm
res_lm_meteor <- lm(Tw0 ~ T + Tmax + Tmin + Pr + N, data=meteo_data_forlm)
summary(res_lm_meteor)
plot(res_lm_meteor)

# library performance
# install.packages("performance")
library(performance)
check_collinearity(res_lm_meteor)

res_lm_meteor2 <- lm(Tw0 ~ Tmax + Tmin + Pr + N,data=meteo_data_forlm)
summary(res_lm_meteor2)
check_collinearity(res_lm_meteor2)

res_lm_meteor3 <- lm(Tw0 ~ Tmin + Pr + N,data=meteo_data_forlm)
summary(res_lm_meteor3)
check_collinearity(res_lm_meteor3)

plot(meteo_data_forlm$Tw0,meteo_data_forlm$Tmin)

