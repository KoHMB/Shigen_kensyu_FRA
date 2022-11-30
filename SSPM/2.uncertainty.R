#########################################################
#########################################################
#
# 令和４年度資源管理研修会
#   SPiCTを用いた状態空間プロダクションモデルによる
#   資源解析の演習
#   2. 推定の不確実性
#
#   Author: Kohei Hamabe & Akira Hirao
#
#########################################################
#########################################################


# 必要なパッケージの呼び出し----
library(tidyverse)
library(spict)


#########################################################
# プロダクションモデルの疑似データの作成----

## "1.run_spict.R"が実行済みであること


## 以下のseedにご自身の生年月日を8桁の数字にして入力してください
set.seed(19001205)


## 設定----
sigi_true <- 0.4
oe_true <- rnorm(Time, -0.5*sigi_true^2, sigi_true)
set.seed(1)
sigb_true <- 0.2
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
df_true <- df_true %>% mutate(Bt_true = Bt_true)
ggplot(data = df_true)+
  geom_line(aes(x = Year, y = Bt_true))+
  ylim(0,NA)

## 漁獲量とCPUEデータの生成----
cpue <- q_true*Bt_true*exp(oe_true)
df_true <- df_true %>% mutate(cpue = cpue) %>% 
  mutate(catch = F*Bt_true)

#write.csv(df_true, "pm_dataset.csv", row.names = FALSE)



#########################################################
# SPiCTによる資源解析----

## データの作成 ----
data_test <- list(timeC = df_true$Year,
                  obsC  = df_true$catch,
                  timeI = df_true$Year,
                  obsI  = df_true$cpue
                  )

## データのプロット
plotspict.ci(data_test)
### あなたのCPUEはどんなトレンドでしょうか？


## 解析のための諸設定を自動生成 ----
input <- check.inp(data_test)
names(input)
input$dteuler <- 1

## 事前分布の設定 ----
names(input$priors)
input$priors$logn <- c(log(2), 0.5, 1)
input$priors$logr <- c(log(0.5), 0.5, 1)


## とりあえずspictで推定
res <- fit.spict(input)
summary(res)

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
ggplot(df_Best %>% select("Year", "Bt_true","estB1") %>% 
         pivot_longer(cols = -"Year", values_to = "Biomass"),
       aes(x=Year, y=Biomass, color = name))+
  geom_line(size = 2) + ylab("資源量")+
  ggtitle("資源動態")

ggplot(df_Best %>% mutate(re = (Bt_true-estB1)/Bt_true) %>% 
         select("Year","re"),
       aes(x=Year, y=re))+
  geom_hline(yintercept = 0, linetype = 2)+
  geom_line(size = 1.5) + geom_point(size = 2) +
  ylab("相対誤差")+ylim(-2,2)
  ggtitle("相対誤差の時系列動態")




