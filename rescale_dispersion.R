# 1%
A1 <- 1
B1 <- 2
t <- sqrt(4^2+4^2)
minCD <- 0
scale_min1 <- A1*10^(-B1*t)
scale_max1 <- A1*10^(-B1*minCD)
dist <- 1
prob1 <- (A1*10^(-B1*dist)-scale_min1)/(scale_max1-scale_min1)
prob1

# 5%
A5 <- 1
B5 <- 1.301
t <- sqrt(4^2+4^2)
minCD <- 0
scale_min5 <- A5*10^(-B5*t)
scale_max5 <- A5*10^(-B5*minCD)
dist <- 1
prob5 <- (A5*10^(-B5*dist)-scale_min5)/(scale_max5-scale_min5)
prob5

# 25%
A25 <- 1
B25 <- 0.6015
t <- sqrt(4^2+4^2)
minCD <- 0
scale_min25 <- A25*10^(-B25*t)
scale_max25 <- A25*10^(-B25*minCD)
dist <- 1
prob25 <- (A25*10^(-B25*dist)-scale_min25)/(scale_max25-scale_min25)
prob25

############################################################################################################
x <-seq(0,sqrt(4^2+4^2),0.0001)
y_resc1 <- (A1*10^(-B1*x)-scale_min1)/(scale_max1-scale_min1)
plot(x, y_resc1, xlim = c(0, 4*sqrt(2)), type="l", col="black")

y_resc5 <- (A5*10^(-B5*x)-scale_min5)/(scale_max5-scale_min5)
lines(x, y_resc5, xlim=c(0, 4*sqrt(2)), type="l", col="black")

y_resc25 <- (A25*10^(-B25*x)-scale_min25)/(scale_max25-scale_min25)
lines(x, y_resc25, xlim=c(0, 4*sqrt(2)), type="l", col="black")

abline(v=1, type="d")

# cdcode <- log( (1 * (t - minCD) + minCD) /A ) / (B * log(10))
# cdcode
############################################################################################################
