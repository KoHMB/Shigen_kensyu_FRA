# package "faraway"のインストール
install.packages("faraway")

# package "faraway" からgalaデータのみ読み込み
data(gala,package = "faraway")

# galaデータの中身チェック
names(gala)

# SpeciesがArea/Elevationでどう変化するのかプロット
plot(gala$Area, gala$Species)
plot(log(gala$Area), gala$Species) # logを取り直して
plot(gala$Elevation, gala$Species)

# glm実行とglm結果オブジェクトの確認
res_glm_gala <- glm(Species~Area+Elevation+Nearest+Scruz+Adjacent, family=poisson(link = "log"),data=gala)
summary(res_glm_gala)

# step関数（direction="backward"）でモデルセレクト
glm_gala_backward <- step(res_glm_gala, direction="backward")

# dredge関数（rank=AIC）でモデルセレクト
#install.packages("MuMIn")
library(MuMIn)
options(na.action = "na.fail")
res_glm_gala_dredge <- dredge(res_glm_gala, rank=AIC)
glm_gala_model_best <- get.models(res_glm_gala_dredge, subset=1)

# glm結果オブジェクトから残差を取り出してプロット
plot(res_glm_gala$residuals)
abline(a=0,b=0)

# glm結果オブジェクトから予測値y^を取り出してプロット、観測された目的変数yと比較
plot(log(gala$Area),res_glm_gala$fitted.values,col="red",ylim=c(0,max(gala$Species)))
par(new=T)
plot(log(gala$Area),gala$Species,col="blue",ylim=c(0,max(gala$Species)))
