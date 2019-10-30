start <- Sys.time()
B_L1_m <- TBI_test_auto_allp_TBIgenJW_test(rep = 180, path = "E:/Julian_simulations/B_L1/output1563381261", scenario = 0,
                                      alpha = c(0.001, 0.0025, 0.005, 0.0075,
                                                0.01, 0.025, 0.05, 0.075,
                                                0.1),
                                      nloci = 100,
                                      nalleles = 2,
                                      nperm = 999)
end <- Sys.time()
end-start

bl1_1 <- lapply(B_L1, function(x) confusion_mat(x[1:30], 1))
bl1_2 <- lapply(B_L1, function(x) confusion_mat(x[31:60], 2))
bl1_3 <- lapply(B_L1, function(x) confusion_mat(x[61:90], 3))
bl1_7 <- lapply(B_L1, function(x) confusion_mat(x[91:120], 7))
bl1_8 <- lapply(B_L1, function(x) confusion_mat(x[121:150], 8))
bl1_13 <- lapply(B_L1, function(x) confusion_mat(x[151:180], 13))
bl1_combine_positions <- mapply(rbind, bl1_1, bl1_2, bl1_3, bl1_7, bl1_8, bl1_13, SIMPLIFY=FALSE)