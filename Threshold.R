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

comb_pos_m <- melt(combine_positions)
comb_pos_m$value <- as.numeric(as.character(comb_pos_m$value))

#ADD control FPR to make choice
#C_L <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/C_L/output1563923314", scenario = 0)
cl <- lapply(C_L, function(x) FPR_control(x))
meansfpr_control <- sapply(cl, function(x) mean(x))
sdfpr_control <- sapply(cl, function(x) sd(x))
meansfpr_control
sdfpr_control

# plot(log10(alpha), 1-meansfnr)
# plot(alpha, 1-meansfnr)
# plot(log10(alpha), meansfpr)
# plot(alpha, meansfpr)
# 
# plot(alpha, meansfnr, type="b", lwd=2)
# lines(alpha, meansfpr, type="b", col="red", lwd=2)
# lines(alpha, meansfpr_control, type="b", col="blue", lwd=2)
# abline(h=0.1, lty=3)
# 
allbl <- data.frame(alpha, meansfpr_control, meansfpr, meansfnr)
allbl_m <- melt(allbl, id.vars="alpha")
allbl_m

ggplot(data=allbl_m, aes(x=alpha, y=value, col=variable)) +
  geom_line() +
  geom_point() +
  xlab("Threshold (p.TBI)") +
  ylab("Rate") +
  ggtitle("B_L1") +
  scale_colour_manual(values = c("orange","blue","black"),
                    labels = c("FPR (control)", "FPR", "FNR"),
                    name = "Performance") +
  geom_hline(yintercept = 0.1, linetype = "dashed")

############################################################################################################


meansfpr_bm1 <- sapply(bm1_combine_positions, function(x) mean(x$FPR))
sdfpr_bm1 <- sapply(bm1_combine_positions, function(x) sd(x$FPR))
meansfpr_bm1
sdfpr_bm1

meansfnr_bm1 <- sapply(bm1_combine_positions, function(x) mean(x$FNR))
sdfnr_bm1 <- sapply(bm1_combine_positions, function(x) sd(x$FNR))
meansfnr_bm1
sdfnr_bm1

comb_pos_m <- melt(combine_positions)
comb_pos_m$value <- as.numeric(as.character(comb_pos_m$value))

#ADD control FPR to make choice
#C_L <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/C_L/output1563923314", scenario = 0)
cl <- lapply(C_L, function(x) FPR_control(x))
meansfpr_control <- sapply(cl, function(x) mean(x))
sdfpr_control <- sapply(cl, function(x) sd(x))
meansfpr_control
sdfpr_control

allbl_bm1 <- data.frame(alpha, meansfpr_control, meansfpr_bm1, meansfnr)
allbl_bm1_m <- melt(allbl, id.vars="alpha")
allbl_bm1_m

ggplot(data=allbl_m, aes(x=alpha, y=value, col=variable)) +
  geom_line() +
  geom_point() +
  xlab("Threshold (p.TBI)") +
  ylab("Rate") +
  ggtitle("B_L1") +
  scale_colour_manual(values = c("orange","blue","black"),
                      labels = c("FPR (control)", "FPR", "FNR"),
                      name = "Performance") +
  geom_hline(yintercept = 0.1, linetype = "dashed")







