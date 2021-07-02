x <- load("/Users/shin-ichironakayama/Dropbox/r_practice/projectR/catch_data.rda")


nllh <- function(initials)
{
  b0_v1 <- initials[1]
  b0_v2 <- initials[2]
  b0_v3 <- initials[3]
  b1 <- initials[4]
  s <- initials[5]
  llh <- 0
  for(i in 1:nrow(catch_data))
  {
    if(catch_data[i,]$vessel=="v1")
    {
      pred <- b0_v1 + b1*catch_data[i,]$temp
    }else if(catch_data[i,]$vessel=="v2")
    {
      pred <- b0_v2 + b1*catch_data[i,]$temp
    }else
    {
      pred <- b0_v3 + b1*catch_data[i,]$temp
    }
    prob <- dnorm(log(catch_data[i,]$catch), pred, s)
    llh <- llh + log(prob)
  }
  nllh <- -llh
  nllh
}


initials <- c(0, 0, 0, 0, 1)
opt <- nlminb(initials, nllh)
opt
res <- lm(log(catch) ~ vessel + temp - 1, data=catch_data)
res
logLik(res)








