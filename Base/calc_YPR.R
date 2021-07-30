#STEP1: YPR計算のための関数を読み込む----
calfish <- function(age,waa,M,F){
  res<- data.frame(age=age,waa=waa,M=M,F=F)
  res$fished.n <- 0 
  for(i in 2:nrow(res)){
    res$fished.n[i] <- 
      F[i]/(F[i]+M[i]) * (1-exp(-F[i]-M[i])) * exp(-sum(M[1:(i-1)])-sum(F[1:(i-1)]))
  }
  res$fished.w <-  res$waa * res$fished.n
  return(res)
}

calYPR <- function(waa,M,F,Fmulti,age){
  ypr <- rep(0,length(Fmulti))
  for(i in 1:length(Fmulti)){
    res <- calfish(age=bpara$Age,waa=bpara$waa,M=bpara$M,F=bpara$Faa*Fmulti[i])
    ypr[i] <- sum(res$fished.w,na.rm=T)
  }
  f.max<-Fmulti[ypr==max(ypr)]
  return(list(ypr=ypr,Fmulti=Fmulti,f.max=f.max))
}


#STEP2: bparaに自分のデータを代入する----
bpara <- data.frame(waa=c(0.04,0.106,0.224,0.404,0.404,0.404),#年齢別体重
                    M=c(rep(0.5,6)),#年齢別自然死亡係数
                    Faa=c(0.52,0.75,1,1,1,1),#年齢別選択率
                    Age=1:6)


#STEP3: YPR計算を実行させる----
res<-calYPR(waa=bpara$waa,M=bpara$M,F=bpara$Faa,Fmulti=seq(from=0,to=1,by=0.01),age=bpara$Age)

#STEP4: FとYPRの関係を可視化----
plot(res$Fmulti,res$ypr,type="l",xlab="F",ylab="YPR") 
points(res$f.max,max(res$ypr),pch=1)
legend("bottomright",pch=1,legend=paste("Fmax=",res$f.max))