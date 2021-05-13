number <- 1000
sum1 <- 0
sum2 <- 0
for(i in 1:number)
{
  if(i%%2 == 0)
  {
    sum1 <- sum1 + i
  }else
  {
    sum2 <- sum2 + i
  }
}
sum1
sum2



eigo <- 80
sugaku <- 25
reference <- 70
if(eigo > reference)
{
  if(sugaku > reference)
  {
    print("You are rank A")
  }else
  {
    print("You are rank B")
  }
}else
{
  if(sugaku > reference)
  {
    print("You are rank B")
  }else
  {
    print("You are rank C")
  }
}




eigo <- 20
sugaku <- 25
reference <- 70

if((eigo > reference) && (sugaku > reference))
{
  print("You are rank A")
}else if((eigo > reference) || (sugaku > reference))
{
  print("You are rank B")
}else
{
  print("You are rank C")
}