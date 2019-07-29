# Position

library(ggplot2)
library(reshape2)

setwd("C:/Users/jwitt/OneDrive/Desktop/Git_Projects/Genetic_TBI_LCBD")

#DONE
#B_L1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_L1/output1563381261", scenario = 0)
bl1_1 <- lapply(B_L1, function(x) confusion_mat(x[1:30], 1))
bl1_2 <- lapply(B_L1, function(x) confusion_mat(x[31:60], 2))
bl1_3 <- lapply(B_L1, function(x) confusion_mat(x[61:90], 3))
bl1_7 <- lapply(B_L1, function(x) confusion_mat(x[91:120], 7))
bl1_8 <- lapply(B_L1, function(x) confusion_mat(x[121:150], 8))
bl1_13 <- lapply(B_L1, function(x) confusion_mat(x[151:180], 13))
bl1_combine_positions <- mapply(rbind, bl1_1, bl1_2, bl1_3, bl1_7, bl1_8, bl1_13 , SIMPLIFY=FALSE)

#DONE
B_M1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_M1/output1563805196", scenario = 0)
bm1_1 <- lapply(B_M1, function(x) confusion_mat(x[1:30], 1))
bm1_2 <- lapply(B_M1, function(x) confusion_mat(x[31:60], 2))
bm1_3 <- lapply(B_M1, function(x) confusion_mat(x[61:90], 3))
bm1_7 <- lapply(B_M1, function(x) confusion_mat(x[91:120], 7))
bm1_8 <- lapply(B_M1, function(x) confusion_mat(x[121:150], 8))
bm1_13 <- lapply(B_M1, function(x) confusion_mat(x[151:180], 13))
bm1_combine_positions <- mapply(rbind, bm1_1, bm1_2, bm1_3, bm1_7, bm1_8, bm1_13 , SIMPLIFY=FALSE)

B_H1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_H1/", scenario = 0)
bh1_1 <- lapply(B_H1, function(x) confusion_mat(x[1:30], 1))
bh1_2 <- lapply(B_H1, function(x) confusion_mat(x[31:60], 2))
bh1_3 <- lapply(B_H1, function(x) confusion_mat(x[61:90], 3))
bh1_7 <- lapply(B_H1, function(x) confusion_mat(x[91:120], 7))
bh1_8 <- lapply(B_H1, function(x) confusion_mat(x[121:150], 8))
bh1_13 <- lapply(B_H1, function(x) confusion_mat(x[151:180], 13))

I_L1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/I_L1/", scenario = 0)
il1_1 <- lapply(I_L1, function(x) confusion_mat(x[1:30], 1))
il1_2 <- lapply(I_L1, function(x) confusion_mat(x[31:60], 2))
il1_3 <- lapply(I_L1, function(x) confusion_mat(x[61:90], 3))
il1_7 <- lapply(I_L1, function(x) confusion_mat(x[91:120], 7))
il1_8 <- lapply(I_L1, function(x) confusion_mat(x[121:150], 8))
il1_13 <- lapply(I_L1, function(x) confusion_mat(x[151:180], 13))

I_M1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/I_M1/", scenario = 0)
im1_1 <- lapply(I_M1, function(x) confusion_mat(x[1:30], 1))
im1_2 <- lapply(I_M1, function(x) confusion_mat(x[31:60], 2))
im1_3 <- lapply(I_M1, function(x) confusion_mat(x[61:90], 3))
im1_7 <- lapply(I_M1, function(x) confusion_mat(x[91:120], 7))
im1_8 <- lapply(I_M1, function(x) confusion_mat(x[121:150], 8))
im1_13 <- lapply(I_M1, function(x) confusion_mat(x[151:180], 13))

I_H1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/I_H1/", scenario = 0)
lapply(I_H1, function(x) confusion_mat(x[1:30], 1))
lapply(I_H1, function(x) confusion_mat(x[31:60], 2))
lapply(I_H1, function(x) confusion_mat(x[61:90], 3))
lapply(I_H1, function(x) confusion_mat(x[91:120], 7))
lapply(I_H1, function(x) confusion_mat(x[121:150], 8))
lapply(I_H1, function(x) confusion_mat(x[151:180], 13))
