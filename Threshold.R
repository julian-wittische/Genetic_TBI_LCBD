# Threshold

setwd("C:/Users/jwitt/OneDrive/Desktop/Git_Projects/Genetic_TBI_LCBD")

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

cl <- lapply(C_L, function(x) FPR_control(x))
meansfpr_control <- sapply(cl, function(x) mean(x))
sdfpr_control <- sapply(cl, function(x) sd(x))
meansfpr_control
sdfpr_control

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
C_M <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/C_M/output1563923273", scenario = 0)
cm <- lapply(C_M, function(x) FPR_control(x))
meansfpr_control_cm <- sapply(cm, function(x) mean(x))
sdfpr_control_cm <- sapply(cm, function(x) sd(x))
meansfpr_control_cm
sdfpr_control_cm

allbl_bm1 <- data.frame(alpha, meansfpr_control_cm, meansfpr_bm1, meansfnr_bm1)
allbl_bm1_m <- melt(allbl_bm1, id.vars="alpha")
allbl_bm1_m

ggplot(data=allbl_bm1_m, aes(x=alpha, y=value, col=variable)) +
  geom_line() +
  geom_point() +
  xlab("Threshold (p.TBI)") +
  ylab("Rate") +
  ggtitle("B_M1") +
  scale_colour_manual(values = c("orange","blue","black"),
                      labels = c("FPR (control)", "FPR", "FNR"),
                      name = "Performance") +
  geom_hline(yintercept = 0.1, linetype = "dashed")







