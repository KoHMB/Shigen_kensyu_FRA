calc_bmi <- function(taiju, shinchou)
{
  taiju/shinchou^2
}

calc_bmi(60, 1.7)


hantei <- function(taiju, shinchou, kagen=18.5, jougen=25)
{
  bmi <- calc_bmi(taiju, shinchou)
  if(bmi<18.5)
  {
    print("teitaiju")
  }else if(bmi>=25)
  {
    print("himan")
  }else
  {
    print("futsuu")
  }
}

hantei(40, 1.7)
