A <- 100
B <- 0.3
#10%   t=1.169661
#20%   t=1.416
t <- 1.416    
minCD <- 0
scale_min<-A*10^(-B*t)
scale_max<-A*10^(-B*minCD)
dist <- 1
prob <- (A*10^(-B*dist)-scale_min)/(scale_max-scale_min)
prob
prob*4/4