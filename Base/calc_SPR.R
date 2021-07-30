#STEP1: SPR計算のための関数を読み込む----
calssb <- function(age,waa,M,F,maa){
  res<- data.frame(age=age,waa=waa,M=M,F=F,maa=maa)
  res$remain.n <- res$remain.n0 <- 0 
  for(i in 1:nrow(res)){
    res$remain.n[i] <- exp(-sum(M[1:i])-sum(F[1:i]))
    res$remain.n0[i] <- exp(-sum(M[1:i]))
  }
  res$ssb.w <-  res$waa * res$maa * res$remain.n
  res$ssb0.w <- res$waa * res$maa * res$remain.n0
  return(res)
}

calSPR <- function(waa,M,F,Fmulti,age,maa){
  spr <- spr0 <- x0<-rep(0,length(Fmulti))
  for(i in 1:length(Fmulti)){
    res <- calssb(age=bpara$Age,waa=bpara$waa,M=bpara$M,F=bpara$Faa*Fmulti[i],maa=bpara$maa)
    spr[i] <- sum(res$ssb.w,na.rm=T)
    spr0[i] <-sum(res$ssb0.w,na.rm=T)
    x0[i] <-spr[i]/spr0[i]
  }
  return(list(spr=spr,spr0=spr0,x0=x0,Fmulti=Fmulti))
}

#STEP2: bparaに自分のデータを代入する----
bpara <- data.frame(waa=c(0.04,0.106,0.224,0.404,0.404,0.404),#年齢別体重
                    M=c(rep(0.5,6)),#年齢別自然死亡係数
                    Faa=c(0.52,0.75,1,1,1,1),#年齢別選択率
                    maa=c(0,0.5,1,1,1,1), #年齢別成熟率
                    Age=1:6)

#STEP3: YPR計算を実行させる----
res<-calSPR(waa=bpara$waa,M=bpara$M,F=bpara$Faa,Fmulti=seq(from=0,to=1,by=0.01),age=bpara$Age,maa=bpara$maa)

#STEP4: FとSPR, %SPRの関係を可視化----
plot(res$Fmulti,res$spr,type="l",xlab="F",ylab="SPR") 
plot(res$Fmulti,res$x0,type="l",xlab="F",ylab="%SPR") 

