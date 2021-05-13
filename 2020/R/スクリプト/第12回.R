set.seed(1) # fix random seed

#generate catch_vs_temp_data
vessel_list <- c("v1", "v2", "v3") # names of the vessels
vessel_effect <- c(2.5, 3.5, 4.5) # effects of the vessels
beta <- 0.1 #coefficient of temperature
mean_temp <- 25 # mean of temperature
sigma_temp <- 2 # sd of temperature
sigma_err <- 0.3 # sd of the error term
data_num <- 1000 # number of data for each vessel

catch_data <- c() # create empty data
for(i in 1:length(vessel_list))
{
  vessel <- rep(vessel_list[i], data_num)
  temp <- rnorm(data_num, mean_temp, sigma_temp)
  catch <- exp(vessel_effect[i] + beta*temp + rnorm(data_num, 0, sigma_err))
  data_temp <- data.frame(vessel, temp, catch)
  catch_data <- rbind(catch_data, data_temp)
}


plot_data <- function(catch_data, vessel_effect, beta)
{
  par(oma=c(5,5,5,5))
  col <- c("red", "blue", "green")
  i <- 1
  for(v in vessel_list)
  {
    data_temp <- catch_data[catch_data$vessel==v,]
    if(i > 1)
    {
      par(new=T)
    }
    plot(data_temp$temp, log(data_temp$catch), xlim=c(18,33), ylim=c(0, 10), col=col[i], xlab="", ylab="", type="p", pch=21)
    abline(a=vessel_effect[i], b=beta, col=col[i], lwd=2)
    i <- i + 1
  }
  mtext("Temperature", side=1, cex=1.5, outer=T)
  mtext("log(Catch)", side=2, cex=1.5, outer=T)
}

plot_data(catch_data, vessel_effect, beta)
head(catch_data)
tail(catch_data)
save(catch_data, file="/Users/shin-ichironakayama/Dropbox/r_practice/projectR/catch_data.rda")


