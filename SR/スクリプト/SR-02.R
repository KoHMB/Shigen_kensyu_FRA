# 2022 資源管理研修 SR-02

# frasyrのインストールとライブラリーの読み込み
devtools::install_github("ichimomo/frasyr@dev")
library(frasyr)

# 例データの呼び出し
data("res_vpa_example")

# vpaオブジェクトからSRdataへの整形
SRdata_ex <- get.SRdata(vpares = res_vpa_example,weight.year = 0)

# 確認
names(SRdata_ex)
head(SRdata_ex)
plot_SRdata(SRdata_ex)

# 手動でSSB,Rを集計してプロット、SRdataと比較して確認
# R <- res_vpa_example$naa[1,]
# SSB <- apply(res_vpa_example$ssb,MARGIN = 2,sum)
# plot(SRdata_ex$SSB,SRdata_ex$R,xlab="SSB",ylab="Recruitment")
# par(new=T)
# plot(SSB,R,xlab="",ylab="",col="red",pch=2,cex=0.5)

# vpaオブジェクトで時系列データの最後の3年は再生産関係の推定に使わない場合
#SRdata_ex2 <- get.SRdata(vpares = res_vpa_example,weight.year=c(min(as.numeric(colnames(res_vpa_example$ssb))):max(as.numeric(colnames(res_vpa_example$ssb)))-3))


# bio_paroオブジェクトの作成(vpaへ入力したデータの最終年から前5年平均)
bio_par <- derive_biopar(res_obj = res_vpa_example,derive_year = c((max(SRdata_ex$year)-4):max(SRdata_ex$year)),stat=mean)
# 確認
bio_par


# 再生産関係にHockey-Stick型を指定、推定方法を最小絶対値法とし（method="L1"）、自己相関を仮定しない（AR=0）場合
resL1HS = fit.SR(SRdata = SRdata_ex,
                 SR = "HS",
                 method = "L1",
                 out.AR = FALSE,
                 AR = 0,
                 bio_par = bio_par)

# 結果オブジェクトの中身
names(resL1HS)
resL1HS
# 結果をテキストでファイル出力
out.SR(resL1HS,filename = "L1HS")
# 推定結果をプロット
plot_SR(SR_result = resL1HS,
        refs = NULL,
        xscale = 1000,
        xlabel = "千トン",
        yscale = 1,
        ylabel = "尾",
        labeling.year = NULL,
        add.info = TRUE,
        recruit_intercept = 0,
        plot_CI = FALSE,
        CI = 0.9,
        shape_custom = c(21,3),
        box.padding = 0,
        add_graph = NULL)

SRplot_gg(resL1HS,
          plot_CI=T)
# プロットオブジェクトを格納
SRplotHS<-SRplot_gg(resL1HS)
# プロットをpngファイルで出力
ggsave_SH(SRplotHS,file="L1HS.png")


# 再生産関係にHockey-Stick型を指定、推定方法を最小絶対値法とし（method="L1"）、2005年にレジームが切り替わり、a,b,sdがレジームで変更される場合
resR1_L1HS <- fit.SRregime(SRdata_ex, SR = "HS",
                           method = "L1",
                           regime.year = c(2005),
                           regime.par = c("a","b", "sd")[1:3],
                           use.fit.SR = TRUE,
                           regime.key = c(0, 1),
                           bio_par=bio_par)
# 結果をテキストでファイル出力
out.SR(resR1_L1HS,filename = "R1L1HS")
# 推定結果をプロット
plot_SRregime(resR1_L1HS)
# オブジェクトとして書き出す
p_R1_L1HS<-SRregime_plot(resR1_L1HS,
                         regime.name = c("A","B"))
# ggsaveで図として保存
ggsave(p_R1_L1HS,
       file = "plotR1L1HS.png",
       unit = "mm", width = 240,
       height = 120,
       dpi = 600)

# 再生産関係にHockey-Stick型を指定、推定方法を最小絶対値法とし（method="L1"））、2005年、2010年にレジームが切り替わるが、2004年以前2010年以後は同じレジームとみなせて、a,b,sdがレジームで変更される場合
resR11_L1HS <- fit.SRregime(SRdata, SR = "HS", method = "L1", regime.year = c(2005,2010), regime.par = c("a","b","sd")[1:3], use.fit.SR = TRUE, regime.key = c(0, 1, 0), bio_par=bio_par)
# 結果をテキストでファイル出力
out.SR(resR11_L1HS,filename = "R11L1HS")
# 推定結果をプロット
plot_SRregime(resR11_L1HS)
SRregime_plot(resR11_L1HS, regime.name = c("A","B"),themeSH = T)

# 再生産関係にべバートンホルト型を指定、推定方法を最小二乗法とし（method="L2"））、2005年にレジームが切り替わり、b,sdがレジームで変更される場合
resR1_L2BH <- fit.SRregime(SRdata, SR = "BH", method = "L2", regime.year = c(2005), regime.par = c("a","b","sd")[2:3], use.fit.SR = TRUE, regime.key = c(0, 1), bio_par=bio_par)
# 結果をテキストでファイル出力
out.SR(resR1_L2BH,filename = "R1L2BH")
# 推定結果をプロット
plot_SRregime(resR1_L2BH)
SRregime_plot(resR1_L2BH, regime.name = c("A","B"))

# 再生産関係にHockey-Stick型を指定、推定方法を最小絶対値法とし（method="L1"）、2005年、2010年にレジームが切り替わる場合
resR2_L1HS <- fit.SRregime(SRdata, SR = "HS", method = "L1", regime.year = c(2005,2010), regime.par = c("a","b", "sd")[1:3], use.fit.SR = TRUE, regime.key = c(0, 1, 2), bio_par=bio_par)
# 結果をテキストでファイル出力
out.SR(resR2_L1HS,filename = "R2L1HS")
# 推定結果をプロット
plot_SRregime(resR2_L1HS)
SRregime_plot(resR2_L1HS, regime.name = c("A","B","C"))
