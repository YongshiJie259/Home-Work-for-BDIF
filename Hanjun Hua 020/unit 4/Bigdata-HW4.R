#HW4-1#
#download data#
install.packages("rjson",repos = "http://cran.us.r-project.org")
library("rjson")
json_file="http://crix.hu-berlin.de/data/crix.json"
json_data=fromJSON(file=json_file)
crix_data_frame=as.data.frame(json_data)
#polt of time series #
par(mfrow=c(3,2))
crix_data_frame_t<-t(crix_data_frame)
time<-crix_data_frame_t[seq(1,2350,by=2)]
price<-as.numeric(crix_data_frame_t[seq(2,2350,by=2)])
crix_data_frame<-cbind(time,price)
time_series<-ts(data=price,start =c(2014,7,31),frequency = 365)
plot(time_series,main="The time series of price")
#plot of  return  #
ret<-diff(log(price))
plot(ts(ret,start= c(2014,7,31),frequency = 365),main="The time series of ret")
#histogram of price and ret#
hist(price,breaks=50,col = "red",freq = FALSE,xlab="price",main = "Histogram of price")
lines(density(price),col="black",lwd=2)

hist(ret,breaks = 40,col = "blue",freq = FALSE,main = "Histogram of price")
lines(density(ret),col="black",lwd=2)

x=seq(-4,4,length=100)
curve(dnorm(x,mean=mean(ret),sd=sd(ret)),add=TRUE,col="red",lwd=2)
#normal qq plot#
qqnorm(ret)
qqline(ret,col="blue",lwd=3)

#  HW4-2 and HW4_3  #
library(forecast)
library(tseries)
#stationary test#
ndiffs(price)
par(mfrow=c(1,1))
d_price<-diff(price,lag=2)
plot(ts(d_price,start =c(2014,7,31),frequency = 365))
adf.test(d_price)
#ACF and PACF#
par(mfrow=c(2,2))
Acf(price)
Pacf(price)
Acf(d_price)
Pacf(d_price)
#fit model#
fit<-arima(time_series,order = c(0,2,3))
fit
accuracy(fit)
#model test#
par(mfrow=c(1,1))
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals,type = "Ljung-Box")
#model forecast#
forecast(fit,5)
plot(forecast(fit,5),xlab = "time",ylab = "price")