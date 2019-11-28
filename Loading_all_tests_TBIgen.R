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

mbl1_1 <- lapply(B_L1_m, function(x) confusion_mat(x[1:30], 1))
mbl1_2 <- lapply(B_L1_m, function(x) confusion_mat(x[31:60], 2))
mbl1_3 <- lapply(B_L1_m, function(x) confusion_mat(x[61:90], 3))
mbl1_7 <- lapply(B_L1_m, function(x) confusion_mat(x[91:120], 7))
mbl1_8 <- lapply(B_L1_m, function(x) confusion_mat(x[121:150], 8))
mbl1_13 <- lapply(B_L1_m, function(x) confusion_mat(x[151:180], 13))
mbl1_combine_positions <- mapply(rbind, mbl1_1, mbl1_2, mbl1_3, mbl1_7, mbl1_8, mbl1_13, SIMPLIFY=FALSE)

start <- Sys.time()
B_M1 <- TBI_test_auto_allp_TBIgenJW_test(rep = 180, path = "E:/Julian_simulations/B_M1/output", scenario = 0,
                                        alpha = c(0.001, 0.0025, 0.005, 0.0075, 0.01, 0.025, 0.05, 0.075, 0.1),
                                        nloci = 100,
                                        nalleles = 2,
                                        nperm = 999)
bm1_1 <- lapply(B_M1, function(x) confusion_mat(x[1:30], 1))
bm1_2 <- lapply(B_M1, function(x) confusion_mat(x[31:60], 2))
bm1_3 <- lapply(B_M1, function(x) confusion_mat(x[61:90], 3))
bm1_7 <- lapply(B_M1, function(x) confusion_mat(x[91:120], 7))
bm1_8 <- lapply(B_M1, function(x) confusion_mat(x[121:150], 8))
bm1_13 <- lapply(B_M1, function(x) confusion_mat(x[151:180], 13))
bm1_combine_positions <- mapply(rbind, bm1_1, bm1_2, bm1_3, bm1_7, bm1_8, bm1_13, SIMPLIFY=FALSE)
end <- Sys.time()
end-start