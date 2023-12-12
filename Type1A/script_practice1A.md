Rを用いた1A系資源の解析演習
===========================

Rスクリプトを使って1A系資源の解析を実感しましょう。個々の演習課題を実施してください。下についているRコードは「ヒント」です。**\[難\]**のマークがついているものは、スクリプトの変更や、考察が必要な難しい問題です。

事前準備
--------

-   devtoolsや必要パッケージ(frasyr, tidyverse)をインストールします
-   以下の作業は最初の１回だけ実施すれば大丈夫です

<!-- -->

    install.packages("devtools") # Windowsの人はdevtoolsをインストールするさいにはRtools（https://cran.r-project.org/bin/windows/Rtools/）のインストールが必要です
    install.packages("tidyverse")
    #devtools::install_github("ichimomo/frasyr")

B1. 基礎知識編
--------------

### 演習① Rに慣れよう

Rを使って以下の計算をしたり、グラフを書いたりしましょう

1.  1から10の足し算をしましょう(sum)
2.  正規乱数を100個発生させましょう(rnorm)
3.  上で発生させた正規乱数をヒストグラムにしてみましょう(hist)
4.  histの引数を使ってみましょう

### 演習② ワーキングディレクトリを使いこなそう

1.  自分のワーキングディレクトリを設定しましょう(setwd, getwd)
2.  さっき書いたスクリプトをワーキングディレクトリ下に保存しましょう
3.  配布した疑似データ・スクリプトをそのワーキングディレクトリに保存しましょう

<!-- -->

    #setwd("c:/Users/00006909/kenshu2023")
    # ワーキングディレクトリが設定できたか確認
    getwd()
    # ファイルがちゃんとおいてあるか確認
    dir()

D. データを用いた演習
---------------------

### 演習③ データを読み込んでVPAを一回実行しよう

-   配布データされたcsvファイルを読み込んでVPAを一回実行してみてください

<!-- -->

    # ワーキングディレクトリを設定
    #setwd("c:/Users/00006909/kenshu2023")
    library(frasyr)
    library(tidyverse)
    library(patchwork)

    # データの読み込み ----
    # 年齢別漁獲尾数
    caa   <- read.csv("ex1_caa.csv",  row.names=1)
    # 年齢別体重
    waa   <- read.csv("ex1_waa.csv",  row.names=1)
    # 年齢別成熟率
    maa   <- read.csv("ex1_maa.csv",  row.names=1)
    # 資源量指数
    index   <- read.csv("ex1_index.csv",  row.names=1)
    dat_vpa <- data.handler(caa=caa, waa=waa, maa=maa, M=0.5, index=index)
    res_vpa <- vpa(dat_vpa) # 引数をつけないで実行＝デフォルト引数が使われる

### 演習④ いろいろな設定のVPAを試そう

-   配布された“dat\_vpa1.rds”ファイルをreadRDSコマンドで読み込もう
-   どんなデータか確認しよう
-   いろいろなVPAの設定を試して、結果を比較しよう

<!-- -->

    dat_vpa <- readRDS("dat_vpa1.rds") 
    #dat_vpa <- readRDS("dat_vpa2.rds") # 以下、演習問題で使う別のデータです
    #dat_vpa <- readRDS("dat_vpa3.rds")
    #dat_vpa <- readRDS("dat_vpa4.rds")

    # データの確認
    par(mfrow=c(2,2))
    matplot(t(dat_vpa$caa), type="b")
    matplot(t(dat_vpa$index[1,]), type="b")
    matplot(t(dat_vpa$index[2,]), type="b")

    # tuningなし
    res_vpa_no <- vpa(dat_vpa, tf.year=45:49, tune=FALSE,
                      p.init=0.4,Pope=FALSE,fc.year=48:50)
    plot_vpa(res_vpa_no, what.plot=c("SSB","Recruitment","fishing_mortality"))

    # 選択率更新法
    res_vpa_su <- vpa(dat_vpa, tf.year=45:49, sel.f=NULL,
                    tune=TRUE, sel.update=TRUE,
                    abund=c("N","SSB"),min.age=c(0,0), max.age=c(0,5),
                    p.init=0.4,Pope=FALSE,fc.year=40:45)
    plot_vpa(res_vpa_su, what.plot=c("SSB","Recruitment","fishing_mortality"))

    # 全F推定
    res_vpa_fa <- vpa(dat_vpa, tf.year=NULL, sel.f=NULL,
                    tune=TRUE, sel.update=FALSE, term.F="all",
                    abund=c("N","SSB"),min.age=c(0,0), max.age=c(0,5),
                    p.init=0.4,Pope=FALSE,fc.year=40:45)
    plot_vpa(res_vpa_fa, what.plot=c("SSB","Recruitment","fishing_mortality"))

    # 3つの結果の比較
    plot_vpa(lst(res_vpa_no, res_vpa_su, res_vpa_fa),
             what.plot=c("SSB","Recruitment","fishing_mortality"))

### 演習⑤ モデル診断しよう

-   残差プロット・レトロスペクティブ解析・ブートストラップ解析を行って、３つのモデルを比較しよう

<!-- -->

    # 残差のプロット
    gg <- plot_residual_vpa(res_vpa_su)
    wrap_plots(gg[1:3], ncol=1)

    gg <- plot_residual_vpa(res_vpa_fa)
    wrap_plots(gg[1:3], ncol=1)

    # レトロスペクティブ解析
    res_retro_no <- do_retrospective_vpa(res_vpa_no)
    res_retro_su <- do_retrospective_vpa(res_vpa_su)
    res_retro_fa <- do_retrospective_vpa(res_vpa_fa)
    # Mohn's rho: 正なら過大傾向気味、負なら過小推定気味
    bind_rows(res_retro_no$mohn_rho,
              res_retro_su$mohn_rho,
              res_retro_fa$mohn_rho)

    # ブートストラップ
    res_boot_su <- plot_resboot_vpa(res_vpa_su, B_ite=30)
    res_boot_fa <- plot_resboot_vpa(res_vpa_fa, B_ite=30)

    wrap_plots(res_boot_su[c("plot_ssb","plot_rec","plot_biomass")],ncol=2)
    wrap_plots(res_boot_fa[c("plot_ssb","plot_rec","plot_biomass")],ncol=2)

### 演習⑥ 資源の状態を把握しよう

-   SPR,YPRなどの管理基準値を計算し、現状の漁獲圧がどのくらいなのか（高いのか・低いのか）判断してみよう

<!-- -->

    # 生物学的管理基準値の計算
    F4850 <- rowMeans(res_vpa_su$faa[, as.character(48:50)])
    res_ref4850 <- ref.F(res_vpa_su, Fcurrent=F4850)
    F4045 <- rowMeans(res_vpa_su$faa[, as.character(40:45)])
    res_ref4045 <- ref.F(res_vpa_su, Fcurrent=F4045)

    res_ref4850$summary
    res_ref4045$summary

    aa <- get.SPR(res_vpa_su)
    plot(aa$ysdata$perSPR)

### 演習⑦ どのチューニング手法がおすすめか、判断しよう **\[難\]**

-   dat1.rdsから推定される3つのVPA結果のうち、どの結果が一番「良い」だろうか？その根拠は？**\[難\]**
-   別データ（dat2.rds）でも同様の解析を行って、dat2.rdsの場合にはどの手法が一番良いか考えよう**\[難\]**

C. 疑似データを使った1系資源の解析（再生産関係の推定）
------------------------------------------------------

### 演習⑧ 再生産関係のあてはめと選択

-   dat1.rdsの選択率更新法の結果を用いて再生産関係の推定を行ってみよう
-   再生産ガイドラインに沿うと、HS, RI,
    BHのうちどの再生産関係を選ぶのが適切だろうか？**\[難\]**
-   再生産関係の選択によって、過去の資源のトレンドや資源状態がどのように説明できるようになるだろうか？**\[難\]**
-   dat3.rdaデータについても同様の解析を行い、dat1.rdsの結果と比較してみよう**\[難\]**

<!-- -->

    # hの計算に必要な生物パラメータの取り出し

    biopars <- derive_biopar(res_vpa_su, derive_year=45:50)

    data_SR_su <- get.SRdata(res_vpa_su)
    res_allSR_su <- tryall_SR(data_SR_su,bio_par=biopars)
    res_allSR_su %>% arrange(AICc) %>%
        mutate(deltaAICc=AICc-min(AICc)) %>% View()

    res_SR_BH  <- fit.SR(data_SR_su, SR="BH", method="L2", AR=0, bio_par=biopars, max.ssb.pred=10)
    res_SR_RI  <- fit.SR(data_SR_su, SR="RI", method="L2", AR=0, bio_par=biopars, max.ssb.pred=10)
    res_SR_HS  <- fit.SR(data_SR_su, SR="HS", method="L2", AR=0, bio_par=biopars, max.ssb.pred=10)

    # B0まで考慮した場合の再生産関係：どれがそれらしいか？頑健か？
    (gg <- compare_SRfit(SRlist=lst(res_SR_BH, res_SR_RI, res_SR_HS)) +
         geom_vline(xintercept=res_SR_BH$steepness$SB0/1000, col="red") +
         geom_vline(xintercept=res_SR_RI$steepness$SB0/1000, col="blue") +
         geom_vline(xintercept=res_SR_HS$steepness$SB0/1000, col="green") +
         xlim(0, res_SR_BH$steepness$SB0/1000*1.2)) 

C. 疑似データを使った1系資源の解析（将来予測）
----------------------------------------------

### 演習⑨ 将来予測をやってみよう

-   以下のコマンドを実行し、将来予測〜結果のプロット〜神戸プロットの描画までを体験してみよう
-   dat3.rds、dat4.rdsについても将来予測をやってみよう。それぞれどのような特徴があるだろうか？**\[難\]**

<!-- -->

    currentF <- as.numeric(unlist(res_vpa_su$faa))

    data_future <- make_future_data(res_vpa_su, nsim=100, nyear=30,
                     future_initial_year_name=50, start_F_year_name=51,
                     start_biopar_year_name=51, start_random_rec_year_name=51,
                     currentF=F4850,
                     futureF=F4850,
                     waa_year=49:50, waa_catch_year=49:50, maa_year=49:50,
                     M_year=49:50,
                     faa_year=NULL, start_ABC_year_name=52,res_SR=res_SR_HS)
    res_MSY <- est_MSYRP(data_future)
    res_MSY$summary

    SPRmsy   <- res_MSY$summary$perSPR[1]
    Fmsy     <- res_MSY$Fvector[1,] %>% as.numeric()
    SBlimit  <- res_MSY$summary$SSB[4]
    SBban    <- res_MSY$summary$SSB[3]
    SBtarget <- res_MSY$summary$SSB[1]

    # make_data_futureのHCR関係の引数をMSYベースのものに変更する
    data_future_msy <- redo_future(data_future,
                                   input_data_list=list(futureF=Fmsy, HCR_Bban=SBban,
                                                        HCR_beta=0.8, HCR_Blimit=SBlimit),
                                   only_data=TRUE)
    res_future_msy <- future_vpa(data_future_msy$data, multi_init=1,
                                 calc_SPR_year_name=as.character(1:100),
                                 SPRtarget=SPR_msy*100)

    plot_futures(res_vpa_su,list(res_future_msy), Btarget=res_MSY$summary$SSB[1],
                 what.plot=c("Recruitment", "SSB", "biomass","catch", "U", "Fratio"))

    # 神戸プロット
    kobe_ratio <- get.SPR(res_vpa_su, target.SPR=res_MSY$summary$perSPR[1]*100)$ysdata %>%
        mutate(Bratio=unlist(colSums(res_vpa_su$ssb))/res_MSY$summary$SSB[1])%>%
        mutate(Fratio=get("F/Ftarget")) %>%
        mutate(year=as.numeric(colnames(res_vpa_su$faa)))

    plot_kobe_gg(FBdata=kobe_ratio,
                 refs_base=res_MSYRP$summary)

### 演習10 将来予測でABCの不確実性を考慮してみよう**\[難\]**

-   通常の将来予測res\_future\_msyの結果とどこが違うだろうか？
-   dat1.rdsとdat4.rdsでどのように違うだろうか？その理由は？

<!-- -->

    # 引数を変えてdata_futureを作り直す（時間がかかるため、計算回数を減らす）
    data_future_mse <- redo_future(data_future_msy, input_data_list=list(nsim=50, nyear=10),
                                   only_data=TRUE)
    # do_MSEのオプションをTRUEにしてMSEモードにする
    res_future_mse <- future_vpa(data_future_mse$data,
                                 multi_init=res_MSYRP$summary$Fref2Fcurrent[1],
                                 do_MSE=TRUE,MSE_input_data=data_future_mse,
                                 calc_SPR_year_name=as.character(1:100), SPRtarget=SPR_msy*100)
    plot_futures(res_vpa_su, lst(res_future_mse, res_future_msy),
                 what.plot=c("Recruitment", "SSB", "biomass","catch", "U", "Fratio"))
