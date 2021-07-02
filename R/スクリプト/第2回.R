#代入
year <- 2019
catch_at_age <- c(10000, 30000)
#確認
year
catch_at_age

gantan <- "2020-1-1"
class(gantan)
gantan <- as.Date("2020-1-1")
class(gantan)
gantan + 1
gantan + "Christmas" #エラー

a <- 7
b <- 2
a^b
a/b
a%/%b
a%%b
a <- c(1:10)
a + b
b <- c(10:1)
a + b
b <- c(1,2)
a + b
