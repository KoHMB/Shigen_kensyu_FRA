# シミュレーションで生成したCPUEデータの読み込み、整形、ノミナルCPUE関数定義
# cpue_data <- read.csv("https://raw.githubusercontent.com/KoHMB/Shigen_kensyu_FRA/main/2021/cpuestandardization.csv")
# cpue_data <- cpue_data[,-1]
# ノミナルCPUE計算の関数化
# calc_nominal <- function(data){
#  year <- unique(data$year) #データオブジェクトのyear列から重複なくデータを抽出
#  nominal_cpue <-c()
#  for( i in year){
#    data_temp <- data[data$year==i,] # yearがfor文のiの年に相当するものを抽出
#    nominal_cpue <- c(nominal_cpue, mean(data_temp$cpue))
#  }
#  nominal_cpue/mean(nominal_cpue)
#}

# 第21回の上記作業が終わっているものとして

# glmでカテゴリカル変数としてのyear、lon latを説明変数として解析、誤差構造はガンマ分布、リンク関数は対数リンク
res_glm_lonlat <- glm(cpue~as.factor(year)+(lon)+(lat)-1,family=Gamma(log),data=cpue_data) #切片の推定なし

# stepで確認
glm_lonlat_backward <- step(res_glm_lonlat, direction="backward")

# 年効果の取り出しと平均で割って規準化してCPUE標準化
year_trend <- exp(res_glm_lonlat$coefficients[1:27])
year_trend <- year_trend/mean(year_trend)
# プロットでトレンドの確認
plot(year_trend )

# lon latの二乗項までを説明変数に含めて解析
res_glm_lon2lat2 <- glm(cpue~as.factor(year)+lon+lat+I(lon^2)+I(lat^2)-1,family=Gamma(link="log"),data=cpue_data)

# dredgeでモデルセレクト
library(MuMIn)
options(na.action = "na.fail")
res_lon2lat2_dredge <- dredge(res_glm_lon2lat2, rank=AIC)
res_glm_lon2lat2_best <- get.models(res_lon2lat2_dredge, subset=1)
res_glm_lon2lat2_best$'32'

# 年効果の取り出しと平均で割って規準化してCPUE標準化
year_trend2 <- exp(res_glm_lon2lat2_best$'32'$coefficients[1:27])
year_trend2 <- year_trend2/mean(year_trend2)
# プロットでトレンドの確認
plot(year_trend2)

# ノミナルCPUEと標準化CPUEを一つのオブジェクトにまとめる
trend_cpue <- data.frame(nominal=calc_nominal(cpue_data),model1=year_trend,model2=year_trend2,year=unique(cpue_data$year))
head(trend_cpue)

# 比較してプロット
# 比較のために縦軸の範囲を３つのCPUEの最小・最大に合わせる（凡例を図の上部に記載するときに余白が欲しければ、最大側の範囲を1.5倍にするなど）
yrange <-c(min(min(treat_cpue$nominal),min(treat_cpue$std_cpue_lonlat),min(treat_cpue$std_cpue_lonlat2)),max(max(treat_cpue$nominal),max(treat_cpue$std_cpue_lonlat),max(treat_cpue$std_cpue_lonlat2)))

# ３つの結果をプロット
plot(treat_cpue$year,treat_cpue$nominal,xlab="year",col="black",ylim=yrange,ylab="")
par(new=T)
plot(treat_cpue$year,treat_cpue$std_cpue_lonlat,xlab="year",col="blue",ylim=yrange,ylab="")
par(new=T)
plot(treat_cpue$year,treat_cpue$std_cpue_lonlat2,xlab="year",col="red",ylim=yrange,ylab="")
labels <- c("nominal","model1","model2")
cols <-c("black","blue","red")
legend("topright",legend = labels, col = cols,pch = 1,ncol = 2) # 右上に凡例。プロットにかぶるので２列表示
