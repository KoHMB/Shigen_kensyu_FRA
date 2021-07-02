#作業場所確認
getwd()
#ホームディレクトリからデスクトップ上のcsvファイルを取り込み
yokohama_meteor <- read.csv("./Desktop/yokohama.csv",fileEncoding = "UTF-8")
#格納したデータの確認
dim(yokohama_meteor)
class(yokohama_meteor)
names(yokohama_meteor)
head(yokohama_meteor)
#プロットしてデータ範囲の確認
plot(yokohama_meteor$T)
#データの型の確認
class(yokohama_meteor$T)
#１行目の除去
yokohama_meteor <- yokohama_meteor[-1,]
#データの型変換
yokohama_meteor$T <- as.numeric(yokohama_meteor$T)

yokohama_meteor$P <- as.numeric(yokohama_meteor$P)
plot(yokohama_meteor$P)
#気圧が正の値のみ抽出して表示
subset(yokohama_meteor, yokohama_meteor$P>0)
#気圧が負になってしまう行番号の表示
which(yokohama_meteor$P<0)
#気圧が負になってしまう行番号を除去してオブジェクトyoko_posPに格納
yoko_posP <- yokohama_meteor[-which(yokohama_meteor$P<0),]
plot(yoko_posP$P)

#項目名のクリーニング
#項目名を文字列のベクトルとして取り出す
meteor_names <- names(yokohama_meteor) 
#項目名の１つ目をyearに上書き
meteor_names[1] <-c("year")
#修正し得た項目名をあらたに項目名にして、確認
names(yokohama_meteor) <- meteor_names
names(yokohama_meteor)

#複数の条件を満た降水量データを抽出
yoko_199012_pr <- subset(yokohama_meteor$Pr, (yokohama_meteor$year==1990 & yokohama_meteor$month==12))
#型の確認と数値型への変換
class(yoko_199012_pr)
yoko_199012_pr <- as.numeric(yoko_199012_pr)
#mean関数が使えることを確認
mean(yoko_199012_pr)
#NAを挿入する日を３箇所ランダムに決定
naday <- as.integer(runif(3,1,length(yoko_199012_pr)))
#NA挿入箇所の確認
naday
#NA挿入とその確認
yoko_199012_pr[naday] <- NA
yoko_199012_pr
#mean関数が使えないことを確認
mean(yoko_199012_pr)
#NA除去
yoko_199012_pr_naomit <- na.omit(yoko_199012_pr)
#mean関数が使えることを確認
mean(yoko_199012_pr_naomit)
#mean関数のna.rmオプションが使えることを確認
mean(yoko_199012_pr,na.rm = TRUE)

#他のデータフレーム型のデータとリスト型で結合
kanagawa_meteor <- list()
kanagawa_meteor[[1]] <- yokohama_meteor

#他のデータの読み込み
ebina_meteor <- read.csv("./Desktop/ebina.csv",fileEncoding = "UTF-8")
tsujido_meteor <- read.csv("./Desktop/tsujido.csv",fileEncoding = "UTF-8")

#リストで結合
kanagawa_meteor[[2]] <- ebina_meteor
kanagawa_meteor[[3]] <- tsujido_meteor

#リストデータの確認
View(kanagawa_meteor)

#リスト内のデータに項目をつける
names(kanagawa_meteor) <- c("yokohama","ebina","tsujido")

#リストに他の方のデータを加える
kanagawa_meteor[[4]] <- yoko_199012_pr_naomit
