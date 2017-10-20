#figure 3:crix&ecrix&efcrix
setwd("c:/Users/dell/Desktop")
load("ecrix.RData")
load("efcrix.RData")
plot(ecrix, type = "l", col = "blue", xaxt = "n", lwd = 3, main = "performance of three indices",  xlab = "Date", 
     ylab = "daily value of indices")
lines(efcrix, col = "red")
library(rjson)
json_file = "http://crix.hu-berlin.de/data/crix.json"
json_data = fromJSON(file=json_file)
crix_data_frame=as.data.frame(json_data)
x=crix_data_frame
dim(x)
n=dim(x)
a=seq(1,n[2],2)
b=seq(2,n[2],2)
data=t(x[1,a])
price=t(x[1,b])
ts.plot(price)
plot(price)
lines(price, col = "green")
s=seq(1,n[2],n[2]/20)
axis(1, at = s, label = names(ecrix)[s])
mtext("blue:ecrix, red:efcrix£¬green:crix")

#figure4
library(rjson)
json_file = "http://crix.hu-berlin.de/data/crix.json"
json_data = fromJSON(file=json_file)
x = as.data.frame(json_data)


date1=c(json_data[[1]]$date)

for (i in 1:2348){
  
  date1[i]=c(json_data[[i]]$date)
}

price1=c(json_data[[1]]$price)
for (i in 1:2348){
  
  price1[i]=c(json_data[[i]]$price)
}

date=date1
price=price1
crix=data.frame(date,price)
date2=date[-1]
ret=diff(log(price))
plot(ret~as.Date(date2),type="l",col="red",xlab="date",ylab="ret", main="Log return of crix index")

#figure5
mean(ret)
var(ret)
sd(ret)
hist(ret, col = "grey", breaks = 20, freq = FALSE, ylim = c(0, 25), xlab = "ret")
lines(density(ret), lwd = 2)
mu = mean(ret)
sigma = sd(ret)
x = seq(-4, 4, length = 100)
curve(dnorm(x, mean = mean(ret), sd = sd(ret)), add = TRUE, col = "darkblue", 
      lwd = 2)

qqnorm(ret)
qqline(ret, col = "blue", lwd = 3)

#figure6
libraries = c("zoo", "tseries")
autocorr = acf(ret, lag.max = 20, ylab = "Sample Autocorrelation", main = "sample ACF of CRIX returns from 2014/07/31 to 2017/10/19" , 
               lwd = 2, ylim = c(-0.3, 1))
autopcorr = pacf(ret, lag.max = 20, ylab = "Sample Partial Autocorrelation", 
                 main = "sample PACF of CRIX returns from 2014/07/31 to 2017/10/19" , ylim = c(-0.3, 0.3), lwd = 2)
#figure7
# arima model
library(caschrono)
library(TTR)
library(forecast)
library(TSA)

par(mfrow = c(1, 1))
auto.arima(ret)
fit202 = arima(ret, order = c(2, 0, 2))
tsdiag(fit202)
fit202 = arima(ret, order = c(2, 0, 2))
crpre = predict(fit202, n.ahead = 30)

dates = seq(as.Date("31/07/2014", format = "%d/%m/%Y"), by = "days", length = length(ret))

plot(ret, type = "l", ylab = "log return", xlab = "days", 
     lwd = 1.5, main = "CRIX returns and predicted values")
lines(crpre$pred, col = "red", lwd = 3)
lines(crpre$pred + 2 * crpre$se, col = "red", lty = 3, lwd = 3)
lines(crpre$pred - 2 * crpre$se, col = "red", lty = 3, lwd = 3)
