
data("res_vpa_example")

SRdata<-get.SRdata(res_vpa_example,weight.year = 0)

SRdata2<-SRdata
SRdata2$R<-ifelse(SRdata2$R>1800,SRdata2$R*2,SRdata2$R)
plot_SRdata(SRdata2)

resL1HS_data2 <- fit.SR(SRdata2,SR="HS",method = "L1",AR=0,out.AR = F)
resL2HS_data2 <- fit.SR(SRdata2,SR="HS",method = "L2",AR=0,out.AR = F)


drlaplace <- function(x, mu, sigma){
  f <- 1/(2*sigma)*exp(-(abs(x-mu))/sigma)
  return(f)
}

curve(dnorm(x,mean=0,sd=1),-5,5,xlab="",ylab = "",ylim=c(0,0.5),col="red")
par(new=T)
curve(drlaplace(x, 0, 1/sqrt(2)), xlab = "", ylab = "probability density", xlim = c(-5,5), ylim=c(0,0.5),col="blue")
