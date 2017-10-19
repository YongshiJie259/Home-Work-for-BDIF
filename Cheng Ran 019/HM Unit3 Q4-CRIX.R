install.packages("rjson", repos="http://cran.us.r-project.org")

library("rjson")

json_file = "http://crix.hu-berlin.de/data/crix.json"

json_data = fromJSON(file=json_file)

crix_data_frame = as.data.frame(json_data)

crix_data_frame1 = as.vector(json_data)

for(i in 1:1174){
  date[i] = crix_data_frame1[[i]][1]
  price[i] = crix_data_frame1[[i]][2]
  
}

date 
price
date1 = as.vector(date)
price1 = as.numeric(price)
data1 = data.frame(date1,price1)
data2 = data.frame(t(as.vector(data1[1:1174])),t(as.vector(data1[-1:-1174])))
names(data2) <- c("date","price")

plot(x = data2$date, y = data2$price)

arima(data2$price, order = c(0,0,0))

arima(data2$price, order = c(1,1,0))

arima(data2$price, order = c(0,1,1))



