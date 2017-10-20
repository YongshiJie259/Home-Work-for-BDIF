############# HW4 ##############
#1&2
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

##figure 3:crix&ecrix%efcrix
plot(ts(crix$price,freq=1),type='l',xlab='Day',ylab='Price')

library(caschrono)
library(TTR)
library(fGarch)
library(rugarch)
library(forecast)
library(TSA)

#*******************ARIMA medel*****************
xy.acfb(crix$price,numer=FALSE)
adf.test(crix$price)
#Augmented Dickey-Fuller Test:not stationary

##*****1)return
r=diff(log(crix$price))
plot(r,type="b")
abline(h = 0)
plot(r,type="l")

#figure4
ts.plot(r)

#figure5
par(mfrow=c(1,2))
mean(r)
var(r)
sd(r)
hist(r, col = "grey", breaks = 20, freq = FALSE, ylim = c(0, 25), xlab = "ret")
lines(density(r), lwd = 2)

mu = mean(r)
sigma = sd(r)
x = seq(-4, 4, length = 100)
curve(dnorm(x, mean = mean(r), sd = sd(r)), add = TRUE, col = "darkblue", 
      lwd = 2)
qqnorm(r)
qqline(r, col = "blue", lwd = 3)

#figure6
libraries = c("zoo", "tseries")
autocorr = acf(r, lag.max = 20, ylab = "Sample Autocorrelation", main = "sample ACF of CRIX returns from 2014/07/31 to 2017/10/19" , 
               lwd = 2, ylim = c(-0.3, 1))
autopcorr = pacf(r, lag.max = 20, ylab = "Sample Partial Autocorrelation", 
                 main = "sample PACF of CRIX returns from 2014/07/31 to 2017/10/19" , ylim = c(-0.3, 0.3), lwd =2)


#figure7
par(mfrow = c(1, 1))
auto.arima(r)
fit202 = arima(r, order = c(2, 0, 2))
tsdiag(fit202)
fit202 = arima(r, order = c(2, 0, 2))
crpre = predict(fit202, n.ahead = 30)

dates = seq(as.Date("31/07/2014", format = "%d/%m/%Y"), by = "days", length = length(ret))

plot(ret, type = "l", ylab = "log return", xlab = "days", 
     lwd = 1.5, main = "CRIX returns and predicted values")
lines(crpre$pred, col = "red", lwd = 3)
lines(crpre$pred + 2 * crpre$se, col = "red", lty = 3, lwd = 3)
lines(crpre$pred - 2 * crpre$se, col = "red", lty = 3, lwd = 3)

#*****2)Model Specification ARIMA(p,d,q)
a.fin1=arima(r,order=c(2,0,0))
summary(a.fin1)
a.fin2=arima(r,order=c(2,0,2))
summary(a.fin2)
a.fin3=arima(r,order=c(2,0,3))
summary(a.fin3)
a.fin4=arima(r,order=c(4,0,2))
summary(a.fin4)
a.fin5=arima(r,order=c(2,1,1))
summary(a.fin5)
a.fin6=arima(r,order=c(2,1,3))
summary(a.fin6)

g1=garchFit(~garch(1,1),data=residuals(a.fin2),trace=FALSE,include.mean=TRUE, na.action=na.pass)
summary(g1)
g2=garchFit(~garch(1,2),data=residuals(a.fin2),trace=FALSE,include.mean=TRUE, na.action=na.pass)
summary(g2) 
g3=garchFit(~garch(2,1),data=residuals(a.fin2),trace=FALSE,include.mean=TRUE, na.action=na.pass)
summary(g3)
g4=garchFit(~garch(2,2),data=residuals(a.fin2),trace=FALSE,include.mean=TRUE, na.action=na.pass)
summary(g4)
#The best one is Garch(1,1) model which has the smallest AIC.


