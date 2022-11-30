#########################################################
#########################################################
#
# 令和４年度資源管理研修会
#   SPiCTを用いた状態空間プロダクションモデルによる
#   資源解析の演習
#   1. spictの実践
#
#   Author: Kohei Hamabe & Akira Hirao
#
#########################################################
#########################################################


# 必要なパッケージの呼び出し----
library(tidyverse)


#########################################################
# SPiCTによる資源解析----

## spictの呼び出し
library(spict)

## データの読み込みと作成 ----
df_data <- read.csv("pm_dataset.csv", header = TRUE)

data_test <- list(timeC = df_data$Year,
                  obsC  = df_data$catch,
                  timeI = df_data$Year,
                  obsI  = df_data$cpue
                  )

### データのプロット
plotspict.ci(data_test)

### 解析のための諸設定を自動生成
input <- check.inp(data_test)
names(input)
input$dteuler <- 1


## 事前分布の確認 ----
names(input$priors)
input$priors$logn

input$priors$logr
input$priors$logK
input$priors$logq
input$priors$logsdb
input$priors$logsdi
### 最初の設定は無情報事前分布である


## とりあえずspictで推定
res <- fit.spict(input)
summary(res)
### 最初の設定は無情報事前分布である

## モデル診断
res_diag <- calc.osa.resid(res)
plotspict.diagnostic(res_diag)


## 結果のプロット ----

## 資源動態の図
plotspict.biomass(res)
plotspict.bbmsy(res)

## 真の資源動態と比較
Bt_est <- exp(res$value[names(res$value)=="logBBmsy"])*
  exp(res$value[names(res$value)=="logBmsy"])
df_Best <- df_true %>% mutate(estB1 = Bt_est[1:50])
ggplot(df_Best)+
  geom_line(aes(x=Year, y=estB1), col = "red")+
  geom_line(aes(x=Year, y=Biomass), linetype = 2)+
  ylab("資源量")
  



#########################################################
# 設定を変更して再解析----

## 事前分布の設定 ----
names(input$priors)
input$priors$logn <- c(log(2), 0.5, 1)
input$priors$logr <- c(log(0.5), 0.5, 1)


## spictで推定
res <- fit.spict(input)
summary(res)
### 最初の設定は無情報事前分布である

## モデル診断
res_diag <- calc.osa.resid(res)
plotspict.diagnostic(res_diag)


## 結果のプロット ----

## 資源動態の図
plotspict.biomass(res)
plotspict.bbmsy(res)

## 真の資源動態と比較
Bt_est <- exp(res$value[names(res$value)=="logBBmsy"])*
  exp(res$value[names(res$value)=="logBmsy"])
df_Best <- df_Best %>% select(Year, Biomass, estB1) %>%
  mutate(estB2 = Bt_est[1:50]) 
ggplot(df_Best%>% 
         pivot_longer(cols = -"Year", values_to = "Biomass"),
       aes(x=Year, y=Biomass, color = name))+
  geom_line(size = 2)+
  ylab("資源量")







