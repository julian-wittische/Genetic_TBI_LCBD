# Threshold

library(ggplot2)
library(reshape2)

setwd("C:/Users/jwitt/OneDrive/Desktop/Git_Projects/Genetic_TBI_LCBD")

#B_L1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_L1/output1563381261", scenario = 0)
bl1_1 <- lapply(B_L1, function(x) confusion_mat(x[1:30], 1))
bl1_2 <- lapply(B_L1, function(x) confusion_mat(x[31:60], 2))
bl1_3 <- lapply(B_L1, function(x) confusion_mat(x[61:90], 3))
bl1_7 <- lapply(B_L1, function(x) confusion_mat(x[91:120], 7))
bl1_8 <- lapply(B_L1, function(x) confusion_mat(x[121:150], 8))
bl1_13 <- lapply(B_L1, function(x) confusion_mat(x[151:180], 13))

combine_positions <- mapply(rbind, bl1_1, bl1_2, bl1_3, bl1_7, bl1_8, bl1_13 , SIMPLIFY=FALSE)

meansfpr <- sapply(combine_positions, function(x) mean(x$FPR))
sdfpr <- sapply(combine_positions, function(x) sd(x$FPR))
meansfpr
sdfpr

meansfnr <- sapply(combine_positions, function(x) mean(x$FNR))
sdfnr <- sapply(combine_positions, function(x) sd(x$FNR))
meansfnr
sdfnr

#ADD control FPR to make choice

comb_pos_m <- melt(combine_positions)
comb_pos_m$value <- as.numeric(as.character(comb_pos_m$value))

# ggplot(data = comb_pos_m[comb_pos_m$variable=="FP",], aes(x=factor(L1), y=value)) +
#   geom_boxplot(aes(fill=variable), scale="width") + 
#   ylim(0, 5) +
#   xlab("p.TBI threshold") +
#   scale_x_discrete(labels=as.character(alpha)) + 
#   ylab("FP") + 
#   theme(legend.position = "none")

ggplot(meansfpr)
#   ylim(0, 5) +
#   xlab("p.TBI threshold") +
#   scale_x_discrete(labels=as.character(alpha)) + 
#   ylab("FP") + 
#   theme(legend.position = "none")

plot(log10(alpha), 1-meansfnr)
plot(alpha, 1-meansfnr)
plot(log10(alpha), meansfpr)
plot(alpha, meansfpr)

plot(alpha, meansfnr, type="l")
lines(alpha, meansfpr, add=TRUE, col="red")












