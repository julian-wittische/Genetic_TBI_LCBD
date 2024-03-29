B_L1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_L1/output1563381261", scenario = 0)
bl1_1 <- lapply(B_L1, function(x) confusion_mat(x[1:30], 1))
bl1_2 <- lapply(B_L1, function(x) confusion_mat(x[31:60], 2))
bl1_3 <- lapply(B_L1, function(x) confusion_mat(x[61:90], 3))
bl1_7 <- lapply(B_L1, function(x) confusion_mat(x[91:120], 7))
bl1_8 <- lapply(B_L1, function(x) confusion_mat(x[121:150], 8))
bl1_13 <- lapply(B_L1, function(x) confusion_mat(x[151:180], 13))

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