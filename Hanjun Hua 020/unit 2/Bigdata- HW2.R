year<-c(1971,1972,1974,1976,1978,1979,1982,1985,1988,1989,1990,1991,1992,1993,
        1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)
number<-c(1,1,1,1,1,1,2,1,1,1,1,1,4,1,3,3,2,5,12,21,22,27,44,31,27,16,29,44,59)
#HW1#
plot(year,number,type = "b",
     col="black",main = "The history of computer memory",
     sub = "This is the change in the number of types of computer memory",
     xlab = "year",ylab = "The number of memory")
barplot(number,
        xlab = "year",ylab ="The number of memory ")
#HW2#
#HW3#
lambda=2
x=seq(0:6)
P<-data.frame(dpois(x,lambda))
sum<-(P[1,]+P[7,]+P[2,]+P[6,]+P[3,]+P[5,]+P[4,]+P[4,])
sum
lambda=5
x=0
dpois(x,lambda)