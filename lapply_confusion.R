setwd("C:/Users/jwitt/OneDrive/Desktop/Git_Projects/Genetic_TBI_LCBD")

# Test the influence of position
B_L1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_L1/output1563381261", scenario = 0)
lapply(B_L1, function(x) confusion_mat(x[1:30], 1))
lapply(B_L1, function(x) confusion_mat(x[31:60], 2))
lapply(B_L1, function(x) confusion_mat(x[61:90], 3))
lapply(B_L1, function(x) confusion_mat(x[91:120], 7))
lapply(B_L1, function(x) confusion_mat(x[121:150], 8))
lapply(B_L1, function(x) confusion_mat(x[151:180], 13))

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

############################################################################################################

B_L2 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_L2/output1563381263", scenario = 0)
lapply(B_L2, function(x) confusion_mat(x[1:30], 1))
lapply(B_L2, function(x) confusion_mat(x[31:60], 2))
lapply(B_L2, function(x) confusion_mat(x[61:90], 3))
lapply(B_L2, function(x) confusion_mat(x[91:120], 7))
lapply(B_L2, function(x) confusion_mat(x[121:150], 8))
lapply(B_L2, function(x) confusion_mat(x[151:180], 13))

B_L3 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_L3/output1563381266", scenario = 0)
lapply(B_L3, function(x) confusion_mat(x[1:30], 1))
lapply(B_L3, function(x) confusion_mat(x[31:60], 2))
lapply(B_L3, function(x) confusion_mat(x[61:90], 3))
lapply(B_L3, function(x) confusion_mat(x[91:120], 7))
lapply(B_L3, function(x) confusion_mat(x[121:150], 8))
lapply(B_L3, function(x) confusion_mat(x[151:180], 13))