#HW3_1#
install.packages("digest",repos ='http://cran.us.r-project.org' )
library("digest")
digest("I learn a lot from this class when I am proper listening to the professor","sha256")
digest("I do not learn a lot from this class when I am absent and playing on my Iphone","sha256")

#HW3-3#
library("RJSONIO")
number<-c(1010,1011,1012,1013,1014,1015,1016)
name<-c("Alice","Bob","Cindy","David","Ella","Frank","Gres")
da<-as.matrix(data.frame(number,name))
s<-cat(toJSON(da))
writeLines(da,"name.json")

#HW3-4#
#download data#
install.packages("rjson",repos = "http://cran.us.r-project.org")
library("rjson")
json_file="http://crix.hu-berlin.de/data/crix.json"
json_data=fromJSON(file=json_file)
crix_data_frame=as.data.frame(json_data)

crix_data_frame_t<-t(crix_data_frame)
time<-crix_data_frame_t[seq(1,2350,by=2)]
price<-crix_data_frame_t[seq(2,2350,by=2)]
crix_data_frame<-cbind(time,price)
time_series<-ts(data=price,start =c(2014,7,31),frequency = 365)
plot(time_series)
#ARMA or ARIMA?#
library(tseries)
adf.test(time_series)
#Since p-value greater than printed p-value,we can't reject the hypothesis#
#the time series(time_series) is not stationary#