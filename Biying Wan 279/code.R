#1
install.packages("digest")
library("digest")
digest("I learn a lot from this class when I am proper listening to the professor","sha256")
digest("I do not learn a lot from this class when I am absent and playing on my Iphone","sha256")

#3
install.packages("rjson")
library("rjson")
json_file3="D:/研二/研二上/大数据与互联网金融/HW3/test.json"
json_data3<- fromJSON(paste(readLines(json_file3), collapse=""))
json_data3<- as.data.frame(json_data3)
print(json_data3)

#4
install.packages("ggplot2")
install.packages("scales")
library("rjson")
json_file="D:/研二/研二上/大数据与互联网金融/HW3/crix.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))
json_df <- as.data.frame(c(json_data[[1]][1],json_data[[1]][2]))
for (i in 2:length(json_data)){
  json_df <- rbind(json_df,as.data.frame(c(json_data[[i]][1],json_data[[i]][2])))
  }
json_df$date <- as.POSIXct(json_df$date)
library(ggplot2)
library(scales)
ggplot(json_df)+
  geom_line(aes(x=date,y=price))+  
  scale_x_datetime(breaks=date_breaks("6 month"),labels=date_format("%Y/%m"))
x<-json_df[,2]
return<-log(x[2:nrow(json_df)])-log(x[1:nrow(json_df)-1])
return<-c(NA,return)
json_return<-as.data.frame(cbind(json_df,return))
json_return<-json_return[,-2]
plot(json_return,type="l")
hist(json_return$return,freq=FALSE,breaks=20,col="red",xlab="return",main="Density Histogram")
qqnorm(json_return$return)