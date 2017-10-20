################HW2###################

# EX1&EX2
cpu.df = read.csv("byte.csv",header = TRUE)
plot(cpu.df$Byte~cpu.df$year)


splines.reg.l1 = smooth.spline(x = cpu.df$year, y = cpu.df$Byte, spar = 0.2)
splines.reg.l2 = smooth.spline(x = cpu.df$year, y = cpu.df$Byte, spar = 1)
splines.reg.l3= smooth.spline(x = cpu.df$year, y = cpu.df$Byte, spar = 2)
lines(splines.reg.l1, col = "red", lwd = 2)  # regression line with lambda = 0.2
lines(splines.reg.l2, col = "green", lwd = 2)  # regression line with lambda = 1
lines(splines.reg.l3, col = "blue", lwd = 2)  # regression line with lambda = 2

# EX3
lambda=2
x=3
probex1=exp(-lambda)*lambda^x/factorial(x)
probex1

lambda=5
x=0
probex2=exp(-lambda)*lambda^x/factorial(x)
probex2


