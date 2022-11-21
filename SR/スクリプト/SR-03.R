# 2022 資源管理研修 SR-03

# SR-02のスクリプトを回した推定結果オブジェクトがRに格納されていることを前提にします

# AIC,AICcやBICを比較してフィットの良いモデルは何かを探る ----
SRarglist <- expand.grid(SR=c("HS","BH","RI"),AR=c(0,1),out.AR=c(FALSE,TRUE))
SRarglist <- SRarglist[-which(SRarglist$AR==0 & SRarglist$out.AR==TRUE),]
resfitSRlist<-list()
AIC <- AICc <- BIC <- c()
for(i in 1:nrow(SRarglist)){
  resfitSRlist[[i]]<-fit.SR(SRdata_ex,SR=SRarglist$SR[i],AR=SRarglist$AR[i],out.AR=SRarglist$out.AR[i])

  AIC <- c(AIC,resfitSRlist[[i]]$AIC)
  AICc <- c(AICc,resfitSRlist[[i]]$AICc)
  BIC <- c(BIC,resfitSRlist[[i]]$BIC)
}
SRlist <- cbind(SRarglist,AIC,AICc,BIC)

# SR関係推定結果オブジェクトをプロットして、推定の予測区間も表示 ----
SRplot_gg(resL1HS,plot_CI=T)

# 推定したパラメータの収束や残差について確認----
# 収束判定 ※③を除く----
checkL1HS<-check.SRfit(resL1HS)
# L1HSは解が複数でるので、複数の推定値が互いに近傍であれば中央値を使う
# 中央値はoptimumに格納される
names(checkL1HS$optimum)
resL1HSchecked<-checkL1HS$optimum

# 残差分布 ----
check.SRdist(resL1HS)
# プロットの描画設定が３つ横並びになっているので一旦解除
dev.off()

# プロファイル尤度 ----
prof.likSR(resL1HS)
prof.likSR(resL1HSchecked)
# 図を保存する場合
prof.likSR(resL1HS, output = TRUE, filename = "ResidDistCheck_L1HS")

prof.likSR(resL2RIARout1)
prof.likSR(resL2BHARout0)

# 残差の自己相関のチェック ----
outer1HS = calc.residAR(resL1HS, output = TRUE, filename = "L1HSresidARouter")
outer1HS

# 残差の自己相関をプロット
autocor.plot(resL1HS) #デフォルトはuse.resid = 1
autocor.plot(resL1HS, output = TRUE, filename = "L1HSdevianceAR")

autocor.plot(resL1HS,use.resid = 2, output = TRUE, filename = "L1HSresidAR")

# ブートストラップ解析(parametric) ----
boot.resL1HS = boot.SR(resL1HS, n = 1000, method = "p")
# 結果のプロット
bootSR.plot(boot.resL1HS,output = T,filename = "bootresL1HS")

# ジャックナイフ解析 ----
jackknife.SR(resL1HS)
# 図を保存する場合
jack.resL1HS = jackknife.SR(resL1HS, output = TRUE, is.plot = TRUE)

# 再生産関係の密度効果について確認----
# パラメータ間相関 ----
corSR(resL1HS)

corSR(resL2RIARout1)
corSR(resL2BHARout0)

# スティープネス ----
resL1HS$steepness


