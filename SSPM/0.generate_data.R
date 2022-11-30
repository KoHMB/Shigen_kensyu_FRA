#########################################################
#########################################################
#
# 令和４年度資源管理研修会
#   SPiCTを用いた状態空間プロダクションモデルによる
#   資源解析の演習
#   0. 疑似データの生成
#
#   Author: Kohei Hamabe & Akira Hirao
#
#########################################################
#########################################################


# 必要なパッケージの呼び出し----
library(tidyverse)

#########################################################
# プロダクションモデルの疑似データの作成----

## ここは実行されなくても大丈夫です。

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
sigi_true <- 0.2
sigb_true <- 0.1

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

write.csv(df_true, "SSPM/pm_true.csv", row.names = FALSE)
write.csv(df_true %>% select("Year", "cpue", "catch"),
          "SSPM/pm_dataset.csv", row.names = FALSE)


