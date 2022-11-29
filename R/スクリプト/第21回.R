# シミュレーションで生成したCPUEデータの読み込み
cpue_data <- read.csv("https://raw.githubusercontent.com/KoHMB/Shigen_kensyu_FRA/main/R/cpuestandardization.csv")

# うまくいかない場合は以下を試してみてください
#install.packages("RCurl")
#library("RCurl")
#url <- getURL("https://raw.githubusercontent.com/KoHMB/Shigen_kensyu_FRA/main/R/cpuestandardization.csv")
#cpue_data <- read.csv(text = url, header = TRUE)

# 中身の確認
head(cpue_data)
cpue_data <- cpue_data[,-1]
head(cpue_data)

# データの俯瞰
plot(cpue_data)

# 年ごとに場所ごとのCPUEをプロットしてファイル保存
xrange<-c(min(cpue_data$lon),max(cpue_data$lon))
yrange<-c(min(cpue_data$lat),max(cpue_data$lat))

for(y in unique(cpue_data$year)){
  cpue_year <- subset(cpue_data, cpue_data$year==y)
  fname <- paste0("cpue_",y,".png")
  png(filename=fname)
  plot(cpue_year$lon,cpue_year$lat,cex=log(cpue_year$cpue),xlim=xrange,ylim=yrange,xlab="Longitude",ylab="Latitude",title(main=y))
  dev.off()
}


# ノミナルCPUE計算の関数化
calc_nominal <- function(data){
  year <- unique(data$year) #データオブジェクトのyear列から重複なくデータを抽出
  nominal_cpue <-c()
  for( i in year){
    data_temp <- data[data$year==i,] # yearがfor文のiの年に相当するものを抽出
    nominal_cpue <- c(nominal_cpue, mean(data_temp$cpue))
  }
  nominal_cpue/mean(nominal_cpue)
}

# ノミナルCPUEの計算
calc_nominal(cpue_data)

# ノミナルCPUEのトレンドをプロット
plot(unique(cpue_data$year),calc_nominal(cpue_data),xlab="year",ylab="nominal cpue")
