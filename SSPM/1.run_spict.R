#########################################################
#########################################################
#
# 令和４年度資源管理研修会
#   SPiCTを用いた状態空間プロダクションモデルによる
#   資源解析の演習
#
#   Author: Kohei Hamabe & Akira Hirao
#
#########################################################
#########################################################


# 必要なパッケージの呼び出し----
library(tidyverse)


#########################################################
# プロダクションモデルの疑似データの作成----

PM_func <- function(Bt, r, K, n, Ft, pe){
  B_next <- (Bt + r/(n-1)*Bt*(1-(Bt/K)^(n-1)) - Ft*Bt)*exp(pe)
  return(max(B_next, 100))
}
set.seed(1)

## 設定----
Time      <- 50
r_true    <- 0.5
K_true    <- 10000
n_true    <- 2
q_true    <- 0.001
sigi_true <- 0.1
sigb_true <- 0.001

Ft_true <- c(seq(0.25,0.5,length=20), rep(0.05,15), rep(0.25, 15))
df_true <- data.frame(Year = 1:Time, F = Ft_true)
ggplot(data = df_true)+
  geom_line(aes(x = Year, y = F))

oe_true <- rnorm(Time, -0.5*sigi_true^2, sigi_true)
pe_true <- rnorm(Time, -0.5*sigb_true^2, sigb_true)

## 真の資源動態の生成----
Bt_true <- numeric(Time)
Bt_true[1] <- K_true
for(t in 2:Time){
  Bt_true[t] <- PM_func(Bt = Bt_true[t-1],
                        r  = r_true,
                        K  = K_true,
                        n  = n_true,
                        Ft = Ft_true[t-1],
                        pe = pe_true[t-1])
}
df_true <- df_true %>% mutate(Biomass = Bt_true)
ggplot(data = df_true)+
  geom_line(aes(x = Year, y = Bt_true))+
  ylim(0,NA)

## 漁獲量とCPUEデータの生成----
cpue <- q_true*Bt_true*exp(oe_true)
df_true <- df_true %>% mutate(cpue = cpue) %>% 
  mutate(catch = F*Biomass)

write.csv(df_true, "pm_dataset.csv", row.names = FALSE)



#########################################################
# SPiCTによる資源解析----

## spictの呼び出し
library(spict)

## データの作成 ----
data_test <- list(timeC = df_true$Year,
                  obsC  = df_true$catch,
                  timeI = df_true$Year,
                  obsI  = df_true$cpue
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







