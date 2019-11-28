source("Sourcing_necessary_functions.R")
setwd("E:/Globus")
load("B_L1_m.RData")
bl1_1 <- lapply(B_L1_m, function(x) confusion_mat(x[1:30], 1))
bl1_2 <- lapply(B_L1_m, function(x) confusion_mat(x[31:60], 2))
bl1_3 <- lapply(B_L1_m, function(x) confusion_mat(x[61:90], 3))
bl1_7 <- lapply(B_L1_m, function(x) confusion_mat(x[91:120], 7))
bl1_8 <- lapply(B_L1_m, function(x) confusion_mat(x[121:150], 8))
bl1_13 <- lapply(B_L1_m, function(x) confusion_mat(x[151:180], 13))
bl1_combine_positions <- mapply(rbind, bl1_1, bl1_2, bl1_3, bl1_7, bl1_8, bl1_13, SIMPLIFY=FALSE)

meansfpr <- sapply(bl1_combine_positions, function(x) mean(x$FPR))
sdfpr <- sapply(bl1_combine_positions, function(x) sd(x$FPR))

meansfnr <- sapply(bl1_combine_positions, function(x) mean(x$FNR))
sdfnr <- sapply(bl1_combine_positions, function(x) sd(x$FNR))

#alpha <- c(0.0001, 0.00025, 0.0005, 0.00075, 0.001, 0.0025, 0.005, 0.0075, 0.01, 0.025, 0.05, 0.075, 0.1)
alpha <- c(0.001, 0.0025, 0.005, 0.0075, 0.01, 0.025, 0.05, 0.075, 0.1)

allbl <- data.frame(alpha, meansfpr, meansfnr)
allbl_m <- melt(allbl, id.vars="alpha")

pl <- ggplot(data=allbl_m, aes(x=alpha, y=value, col=variable)) +
  geom_line() +
  geom_point() +
  scale_colour_manual(values = c("blue","black"),
                      labels = c("FPR", "FNR"),
                      name = "Performance") +
  geom_hline(yintercept = 0.1, linetype = "dashed") +
  theme(text=element_text(size=12,  family="serif")) +
  coord_fixed(ratio=0.25)

pl
