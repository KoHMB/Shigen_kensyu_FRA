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


## とりあえずspictで推定 ==================================================
res <- fit.spict(input)



### 1. 解析結果の確認 =====================================================

# 結果を要約する
summary(res)

# 推定された初期資源量の割合がデフォルトでは出ないので，出す
get.par("logbkfrac", res, exp = TRUE) #オプションexp=TRUEによって、log推定値を非対数に戻す

#入力データの初期値を確認する：入力データが正しく設定されているかを事後のチェックをしておきましょう
res$inp$ini  # res$inp (spict解析に用いた入力データのオブジェクト)

##------------------------------------------
## 推定が上手くいっているかの確認事項・その１
##
## その１−１: 収束しているかどうかを判定
res$opt$convergence  #これが0だったら，収束しているのでOK; もし1だったら、収束していないので結果は信頼できない
##
##
## その１−２: 推定パラメータの分散が有限かどうかを判定
all(is.finite(res$sd))  # TRUEだったらパラメータの分散が全て有限であるということでOK
##
##
## その１−３: B/BmsyやF/Fmsyの信用区間が一桁以上に広がっていないかどうかを確認
calc.om(res) #戻り値のmagnitudeが1 以下ならばOK
##
##
## その１−４: 初期値によってパラメータの推定値が変わらないかどうかを確認
## check.ini(res)で初期値を変えたときの影響をみることができる．
## そしてfit<-check.ini(res)としてfit$check.ini$resmatとすると10回分の推定パラの値の一覧が出てくる．
options(max.print = 1e+05)
fit <- check.ini(res, ntrials = 10)  #ntrials = 20に増やしてもよい？
##
fit$check.ini$inimat  #trial毎に与えた初期値を確認しておく
##
fit$check.ini$resmat  #初期値を変えたtrialによって推定された値。初期値によってはNAとなる場合も。。。



### ２．結果のプロット =====================================================

plot(res) #全体的な結果のプロット
##-------------------------------------------
## 推定が上手くいっているかの確認事項・その２
## 余剰生産曲線の形が現実的であるかどうか
calc.bmsyk(res)　
##この値が0.1—0.9の範囲外にある場合は、余剰生産曲線の形が偏っている
##-------------------------------------------


### ３．推定パラメーターの事前分布と事後分布のプロット  ========================

plotspict.priors(res)  #事前分布と事後分布


### ４．残差診断（バイアス、自己相関、正規性の診断） ==========================

res_resi <- calc.osa.resid(res)
plotspict.diagnostic(res_resi)

##------------------------------------------- 
##　推定が上手くいっているか確認事項・その３
## p値が0.05より大きい(有意に差がない．有意に差があると，
## 図の上のp値の文字が赤色になる．緑色ならOK)
##-------------------------------------------



### 5．結果のプロット ==========================

## 資源動態の図 ------------
plotspict.biomass(res)
plotspict.bbmsy(res)

## 真の資源動態と比較 -----------------
Bt_est <- exp(res$value[names(res$value)=="logBBmsy"])*
  exp(res$value[names(res$value)=="logBmsy"])
df_true <- read.csv("pm_true.csv", header = TRUE)
df_Best <- df_true %>% select("Year", "Biomass") %>%
  mutate(est = Bt_est[1:50]) %>% 
  rename("true" = Biomass) %>% 
  pivot_longer(cols = -"Year", values_to = "Biomass")
ggplot(df_Best,
       aes(x = Year, y = Biomass, linetype = name, color = name))+
  geom_line(size = 2)+
  ylab("資源量")+ylim(0,NA)
  



#########################################################
####　５．レトロスペクティブ解析

res_retro <- retro(res, nretroyear = 5)　#レトロスペクティブ解析を実行
plotspict.retro(res_retro)  #レトロ解析プロット

plotspict.retro.fixed(res_retro)  #推定パラメータに関するレトロプロット

mohns_rho(res_retro, what = c("FFmsy", "BBmsy"))  #モーンズローの値を表示

##------------------------------------------- 
##　推定が上手くいっているか確認事項・その4
## レトロ解析パターンに一貫性があるかどうか：
## チェックポイント１：F/FmsyやB/Bmsyが連続に一貫して過小評価あるいは過大評価されていないか
## チェックポイント2：ベースケースの信用区間内にあるかどうか
#-------------------------------------------



#########################################################
####　６．資源変動の要因分解プロット

source("function.R")
res_plot <- plot_barbiomass(res = res)

##------------------------------------------- 
##　推定が上手くいっているか確認事項・その5
## 図の灰色は資源量の推定値を示し、赤、緑、青の矢印がそれぞれの資源量の変動に対する
## 余剰生産、漁獲、プロセス誤差の影響の大きさを示す
## チェックポイント：資源量の変動の大部分がプロセス誤差で説明されている場合はよい推定ではない
#-------------------------------------------



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







