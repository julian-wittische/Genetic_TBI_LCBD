# Position

library(ggplot2)
library(reshape2)

setwd("C:/Users/jwitt/OneDrive/Desktop/Git_Projects/Genetic_TBI_LCBD")

B_L1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_L1/output1563381261", scenario = 0)
bl1_1 <- lapply(B_L1, function(x) confusion_mat(x[1:30], 1))
bl1_2 <- lapply(B_L1, function(x) confusion_mat(x[31:60], 2))
bl1_3 <- lapply(B_L1, function(x) confusion_mat(x[61:90], 3))
bl1_7 <- lapply(B_L1, function(x) confusion_mat(x[91:120], 7))
bl1_8 <- lapply(B_L1, function(x) confusion_mat(x[121:150], 8))
bl1_13 <- lapply(B_L1, function(x) confusion_mat(x[151:180], 13))

# Influence of position in the landscape
bl1_conf_list <- list(bl1_1, bl1_2, bl1_3, bl1_7, bl1_8, bl1_13)
means1 <- sapply(bl1_conf_list[[1]], function(x) mean(x$FPR))
means13 <- sapply(bl1_conf_list[[6]], function(x) mean(x$FPR))
ks.test(means1, means13) #???

par(mfrow=c(2,3))

bl1_1_m <- melt(bl1_1)
bl1_1_m$value <- as.numeric(as.character(bl1_1_m $value))
ggplot(data = bl1_1_m[bl1_1_m$variable=="FPR",], aes(x=factor(L1), y=value)) +
  geom_violin(aes(fill=variable)) + 
  ylim(0, 0.15) +
  xlab("p.TBI threshold") +
  scale_x_discrete(labels=as.character(alpha)) + 
  ylab("FPR") + 
  theme(legend.position = "none")
  

bl1_2_m <- melt(bl1_2)
bl1_2_m$value <- as.numeric(as.character(bl1_2_m $value))
ggplot(data = bl1_2_m[bl1_2_m$variable=="FPR",], aes(x=factor(L1), y=value)) +
  geom_violin(aes(fill=variable)) + 
  ylim(0, 0.15)+
  xlab("p.TBI threshold") +
  scale_x_discrete(labels=as.character(alpha)) + 
  ylab("FPR") + 
  theme(legend.position = "none")

bl1_7_m <- melt(bl1_7)
bl1_7_m$value <- as.numeric(as.character(bl1_7_m $value))
ggplot(data = bl1_7_m[bl1_7_m$variable=="FPR",], aes(x=factor(L1), y=value)) +
  geom_violin(aes(fill=variable)) + 
  ylim(0, 0.15)+
  xlab("p.TBI threshold") +
  scale_x_discrete(labels=as.character(alpha)) + 
  ylab("FPR") + 
  theme(legend.position = "none")


bl1_8_m <- melt(bl1_8)
bl1_8_m$value <- as.numeric(as.character(bl1_8_m $value))
ggplot(data = bl1_8_m[bl1_8_m$variable=="FPR",], aes(x=factor(L1), y=value)) +
  geom_violin(aes(fill=variable)) + 
  ylim(0, 0.15)+
  xlab("p.TBI threshold") +
  scale_x_discrete(labels=as.character(alpha)) + 
  ylab("FPR") + 
  theme(legend.position = "none")


bl1_13_m <- melt(bl1_13)
bl1_13_m$value <- as.numeric(as.character(bl1_13_m $value))
ggplot(data = bl1_13_m[bl1_13_m$variable=="FPR",], aes(x=factor(L1), y=value)) +
  geom_violin(aes(fill=variable)) + 
  ylim(0, 0.15)+
  xlab("p.TBI threshold") +
  scale_x_discrete(labels=as.character(alpha)) + 
  ylab("FPR") + 
  theme(legend.position = "none")



boxplot(bl1_1[[1]]$FNR)

for (i in 1:length(bl1_1)){
  boxplot(bl1_1[[i]]$FNR)
}



B_M1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_M1/output1563805196", scenario = 0)
lapply(B_M1, function(x) confusion_mat(x[1:30], 1))
lapply(B_M1, function(x) confusion_mat(x[31:60], 2))
lapply(B_M1, function(x) confusion_mat(x[61:90], 3))
lapply(B_M1, function(x) confusion_mat(x[91:120], 7))
lapply(B_M1, function(x) confusion_mat(x[121:150], 8))
lapply(B_M1, function(x) confusion_mat(x[151:180], 13))

B_H1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_H1/", scenario = 0)
lapply(B_H1, function(x) confusion_mat(x[1:30], 1))
lapply(B_H1, function(x) confusion_mat(x[31:60], 2))
lapply(B_H1, function(x) confusion_mat(x[61:90], 3))
lapply(B_H1, function(x) confusion_mat(x[91:120], 7))
lapply(B_H1, function(x) confusion_mat(x[121:150], 8))
lapply(B_H1, function(x) confusion_mat(x[151:180], 13))

I_L1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/I_L1/", scenario = 0)
lapply(I_L1, function(x) confusion_mat(x[1:30], 1))
lapply(I_L1, function(x) confusion_mat(x[31:60], 2))
lapply(I_L1, function(x) confusion_mat(x[61:90], 3))
lapply(I_L1, function(x) confusion_mat(x[91:120], 7))
lapply(I_L1, function(x) confusion_mat(x[121:150], 8))
lapply(I_L1, function(x) confusion_mat(x[151:180], 13))

I_M1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/I_M1/", scenario = 0)
lapply(I_M1, function(x) confusion_mat(x[1:30], 1))
lapply(I_M1, function(x) confusion_mat(x[31:60], 2))
lapply(I_M1, function(x) confusion_mat(x[61:90], 3))
lapply(I_M1, function(x) confusion_mat(x[91:120], 7))
lapply(I_M1, function(x) confusion_mat(x[121:150], 8))
lapply(I_M1, function(x) confusion_mat(x[151:180], 13))

I_H1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/I_H1/", scenario = 0)
lapply(I_H1, function(x) confusion_mat(x[1:30], 1))
lapply(I_H1, function(x) confusion_mat(x[31:60], 2))
lapply(I_H1, function(x) confusion_mat(x[61:90], 3))
lapply(I_H1, function(x) confusion_mat(x[91:120], 7))
lapply(I_H1, function(x) confusion_mat(x[121:150], 8))
lapply(I_H1, function(x) confusion_mat(x[151:180], 13))
