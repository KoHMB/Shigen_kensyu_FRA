n <- 5
a <- 1
b <- 1
c <- a + b
for(i in 1:(n-3))
{
  print(i)
  a <- b
  b <- c
  c <- a + b
}
c


n <- 3
a <- -1
b <- 1
c <- 0
for(i in 1:n)
{
  #print(i)
  a <- b
  b <- c
  c <- a + b
}
c