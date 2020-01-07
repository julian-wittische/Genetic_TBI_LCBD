# Figure 1: rescaled kernels

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
# x <-seq(0,sqrt(4^2+4^2),0.0001)
# y_resc1 <- (A1*10^(-B1*x)-scale_min1)/(scale_max1-scale_min1)
# plot(x, y_resc1, xlim = c(0, 4*sqrt(2)), type="l", col="black")
# 
# y_resc5 <- (A5*10^(-B5*x)-scale_min5)/(scale_max5-scale_min5)
# lines(x, y_resc5, xlim=c(0, 4*sqrt(2)), type="l", col="black")
# 
# y_resc25 <- (A25*10^(-B25*x)-scale_min25)/(scale_max25-scale_min25)
# lines(x, y_resc25, xlim=c(0, 4*sqrt(2)), type="l", col="black")
# 
# abline(v=1, type="d")
# 
# # cdcode <- log( (1 * (t - minCD) + minCD) /A ) / (B * log(10))
# # cdcode
############################################################################################################
p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
fun.1 <- function(x) (A1*10^(-B1*x)-scale_min1)/(scale_max1-scale_min1)
fun.2 <- function(x) (A5*10^(-B5*x)-scale_min5)/(scale_max5-scale_min5)
fun.3 <- function(x) (A25*10^(-B25*x)-scale_min25)/(scale_max25-scale_min25)

p +
  stat_function(fun = fun.1, mapping = aes(color = "fun.1"), size=1.5) +
  stat_function(fun = fun.2, mapping = aes(color = "fun.2"), size=1.5) +
  stat_function(fun = fun.3, mapping = aes(color = "fun.3"), size=1.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = NA)) +
    scale_x_continuous(limits = c(0, sqrt(4^2+4^2)), breaks=c(0,1,2,3,4,5)) +
  scale_color_manual(name = "Dispersal",
                     values = c("fun.1" = "gray85", "fun.2" = "gray50", "fun.3" = "black"),
                     labels = c("Low", "Intermediate", "High")) +
  #geom_vline(xintercept = 1, linetype = "dashed") +
  ylab("Probability of dispersal") +
  xlab("Distance from home population") +
  theme(text=element_text(size=18,  family="serif"))


