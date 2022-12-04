# 2022　資源管理研修 Type2-02

# MSEの諸関数を含んだソースファイルをsourceして読み込み
source("scr/23kei_simulation_mse.txt")

# MSEのOM（Operation Model）設定
#- 9 scenario x delta x 3 r x 2 si x 2 sr x n = 10800 calculation --
n <- 100
r.tmp <- c(0.3,0.5,0.7)
si.tmp <- c(0.2,0.4)
sr.tmp <- c(0.2,0.4)
k.tmp <- c(1)
# set delta
delta <- rbind(c(0.5,0.4,0.4)) 
colnames(delta) <- c("high","mid","low")
# 2系基本ルールMSEの実行 ----
res_base_rule <- do.scenario(delta,n=n,r.tmp=r.tmp,si.tmp=si.tmp,sr.tmp=sr.tmp,k.tmp=k.tmp,label="base",man.option="ABC2",n.cpue=3, n.catch=5,Bref=0.8,PL=0.7,PB=0.0)

# スコア(Bscore,Cscore,AAVscore)を出す ----
score_base_rule<-score.func(res=res_base_rule,first2 = F,score.ratio = T)


