############# HW3 ##############
#1
library(digest)
digest("I learn a lot from this class when I am proper listening to the professor","sha256")
digest("I do not learn a lot from this class when I am absent and playing on my Iphone","sha256")


#3
library(rjson)
json_file = "http://crix.hu-berlin.de/data/crix.json"

json_data = fromJSON(file=json_file)

x = as.data.frame(json_data)

date1=c(json_data[[1]]$date)

for (i in 1:1177){
  
  date1[i]=c(json_data[[i]]$date)
}

price1=c(json_data[[1]]$price)
for (i in 1:1177){
  
  price1[i]=c(json_data[[i]]$price)
}

date=date1
price=price1
crix=data.frame(date,price)

plot(crix$price~as.Date(crix$date))
plot(crix$price~crix$date,type="b")
plot(ts(crix$price,freq=1),type='l',xlab='Day',ylab='Price')


#4

library(caschrono)
library(TTR)
library(fGarch)
library(rugarch)
library(forecast)
library(TSA)

#*******************ARIMA model*****************
xy.acfb(crix$price,numer=FALSE)
adf.test(crix$price)
#Augmented Dickey-Fuller Test:not stationary

##*****1)return
r=diff(log(crix$price))*100
plot(r,type="b")
abline(h = 0)
plot(r,type="l")
xy.acfb(r,numer=FALSE)

#*****2)Parameter Estimation
#estimation of p and q

a.fin2=arima(r,order=c(2,0,2))
summary(a.fin2)
f=forecast(a.fin2,h=3,level=c(99.5))
acf(f$residuals,lag.max = 20)
Box.test(f$residuals,lag=20,type='Ljung-Box')
#the residuals follow Gaussian distribution
plot.ts(f$residuals)

#****3)some evidence to GARCH model
#get ACF and PACF of the residuals
xy.acfb(residuals(a.fin2),numer=FALSE)
xy.acfb((residuals(a.fin2))^2,numer=FALSE)+
xy.acfb(abs(residuals(a.fin2)),numer=FALSE)

#get the Conditional heteroskedasticity test
McLeod.Li.test(y=residuals(a.fin2))
#p-values are all included in the test, it formally shows strong evidence for ARCH in this data.

#**Normality of the Residuals
qqnorm(residuals(a.fin2))
qqline(residuals(a.fin2))
shapiro.test(residuals(a.fin2))
#The QQ plot suggest that the distribution of returns may have a tail thicker that of a 
#normal distribution and maybe somewhat skewed to the right
#p-value<0.05 reject the normality hypothesis

g1=garchFit(~garch(1,1),data=residuals(a.fin2),trace=FALSE,include.mean=TRUE, na.action=na.pass)
summary(g1)
g2=garchFit(~garch(1,2),data=residuals(a.fin2),trace=FALSE,include.mean=TRUE, na.action=na.pass)
summary(g2) 
g3=garchFit(~garch(2,1),data=residuals(a.fin2),trace=FALSE,include.mean=TRUE, na.action=na.pass)
summary(g3)
g4=garchFit(~garch(2,2),data=residuals(a.fin2),trace=FALSE,include.mean=TRUE, na.action=na.pass)
summary(g4)
#The best one is Garch(1,1) model which has the smallest AIC.






