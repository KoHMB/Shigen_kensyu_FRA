heikin <- function(a, b)
{
  mean <- (a+b)/2
  mean
}

heikin(1,3)

heikin2 <- function(a, b)
{
  (a+b)/2
}

heikin2(1,3)


heikin3 <- function(a, b, souka=T)
{
  if(souka==T)
  {
    mean <- (a+b)/2
  }else
  {
    mean <- (a*b)^(1/2)
  }
  mean
}

heikin3(1,3)
heikin3(1,3, souka=F)




fibonacci <- function(n)
{
  a <- -1
  b <- 1
  c <- 0
  for(i in 1:n)
  {
    a <- b
    b <- c
    c <- a + b
  }
  c
}

fibonacci(8)


fibonacci_list <- function(n)
{
  suretsu <- c()
  for(i in 1:n)
  {
    suretsu <- c(suretsu, fibonacci(i))
  }
  suretsu
}

fibonacci_list(10)










