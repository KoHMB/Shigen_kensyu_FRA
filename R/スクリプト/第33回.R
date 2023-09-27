# データ読み込み
FishTrait <- read.csv("FishBodyTraitData.csv")

# データを眺める
View(FishTrait) # 種、科、目、生息場所、尾ビレの形以外の数値は全長に対する比

# 尾ビレの形が円形のものは尾叉長/全長の比率はNAになっているので、解析のため1とする
FishTrait$ratio.Fork.length<-ifelse(is.na(FishTrait$ratio.Fork.length),1,FishTrait$ratio.Fork.length)

# 目的変数のHabitatは浮魚を１、底魚を０とする
FishTrait$Habitat<- ifelse(FishTrait$Habitat=="浮魚",1,0)

# 説明変数を尾叉長+眼径のみ
res_logistic_glm <- glm(Habitat~ratio.Fork.length+ratio.Eye.diameter,data = FishTrait,family = binomial(link = "logit"))

summary(res_logistic_glm)
predict.glm(res_logistic_glm,type = "response")

# install.packages("pROC")
library(pROC)
# roc関数にてROC解析を実行
ROC <- roc(response=res_logistic_glm$y, predictor=res_logistic_glm$fitted.values) 
# plotしてみる
plot(1-ROC$specificities, ROC$sensitivities, xlab="False Positive Rate", ylab="True Positive Rate", type="l", lwd=2) # plot ROC curve
abline(a=0,b=1,lty=2)
# AUC
ROC$auc
CI <- ci.auc(ROC, conf.level=0.95) 

# 説明変数を尾叉長+眼径+尾ひれの形
res_logistic_glm2 <- glm(Habitat~ratio.Fork.length+ratio.Eye.diameter+Shape.caudal.fin,data = FishTrait,family = binomial(link = "logit"))

summary(res_logistic_glm2)
predict.glm(res_logistic_glm2,type = "response")

# roc関数にてROC解析を実行
ROC2 <- roc(response=res_logistic_glm2$y, predictor=res_logistic_glm2$fitted.values) 
# plotしてみる
plot(1-ROC2$specificities, ROC2$sensitivities, xlab="False Positive Rate", ylab="True Positive Rate", type="l", lwd=2) # plot ROC curve
abline(a=0,b=1,lty=2)
# AUC
ROC2$auc
CI2 <- ci.auc(ROC2, conf.level=0.95) 

# 尾びれの形を説明変数に入れたことでAUCに有意な差が生じたか検定
roc.test(ROC, ROC2, method="delong", alternative="two.sided")

# 全長に対する各部位の長さ＋尾ビレの形で浮魚・底魚に対するロジスティック回帰
res_logistic_glm_full <- glm(Habitat~ratio.Fork.length+ratio.Eye.diameter+ratio.Body.height+ratio.First.dorsal.fin+ratio.Second.dorsal.fin+Shape.caudal.fin,data = FishTrait,family = binomial(link = "logit"))

summary(res_logistic_glm_full)
predict.glm(res_logistic_glm_full,type = "response") # ガッツリ予測

# roc関数にてROC解析を実行
ROC.full <- roc(response=res_logistic_glm_full$y, predictor=res_logistic_glm_full$fitted.values)
# plotしてみる
plot(1-ROC.full$specificities, ROC.full$sensitivities, xlab="False Positive Rate", ylab="True Positive Rate", type="l", lwd=2) # plot ROC curve
abline(a=0,b=1,lty=2)

roc.test(ROC, ROC.full, method="delong", alternative="two.sided")
# ROC曲線の形状はかなり違うが、サンプルサイズが小さいので有意差は出なかった