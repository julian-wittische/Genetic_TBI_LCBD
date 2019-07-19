x <-seq(0,,0.01)
y <- 10^(-2*x)
plot(x,y,xlim=c(0,8), type="l", col="red")

y_resc <- (A*10^(-B*x)-scale_min)/(scale_max-scale_min)
lines(x,y_resc, type="l", col="black", add=TRUE)

x <-seq(0,36,0.01)
y <- 10^(-1.301*x)
lines(x,y,xlim=c(0,5), type="l", col="green", add=TRUE)

x <-seq(0,36,0.01)
y <- 10^(-0.60205*x)
lines(x,y,xlim=c(0,5), type="l", col="blue", add=TRUE)

abline(v=1)
abline(v=2)

cdcode <- log( (1 * (t - minCD) + minCD) /A ) / (B * log(10))
cdcode
############################################################################################################

# 1%
A <- 1
B <- 2
#10%   t=1.169661
#20%   t=1.416
t <- sqrt(4^2+4^2)
minCD <- 0
scale_min<-A*10^(-B*t)
scale_max<-A*10^(-B*minCD)
dist <- 1
prob <- (A*10^(-B*dist)-scale_min)/(scale_max-scale_min)
prob

# 5%
A <- 1
B <- 1.301
#10%   t=1.169661
#20%   t=1.416
t <- sqrt(4^2+4^2)
minCD <- 0
scale_min<-A*10^(-B*t)
scale_max<-A*10^(-B*minCD)
dist <- 1
prob <- (A*10^(-B*dist)-scale_min)/(scale_max-scale_min)
prob

# 25%
A <- 1
B <- 0.6015
#10%   t=1.169661
#20%   t=1.416
t <- sqrt(4^2+4^2)
minCD <- 0
scale_min<-A*10^(-B*t)
scale_max<-A*10^(-B*minCD)
dist <- 1
prob <- (A*10^(-B*dist)-scale_min)/(scale_max-scale_min)
prob*4/4