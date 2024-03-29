# Loading all the necessary tests for the analysis
# Julian Wittische 
# August 2019

# I saved the TBI results as .RData so no need to rerun the commented lines

setwd("E:/Julian_simulations")
load("data.RData")

# B_L1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_L1/output1563381261", scenario = 0)
bl1_1 <- lapply(B_L1, function(x) confusion_mat(x[1:30], 1))
bl1_2 <- lapply(B_L1, function(x) confusion_mat(x[31:60], 2))
bl1_3 <- lapply(B_L1, function(x) confusion_mat(x[61:90], 3))
bl1_7 <- lapply(B_L1, function(x) confusion_mat(x[91:120], 7))
bl1_8 <- lapply(B_L1, function(x) confusion_mat(x[121:150], 8))
bl1_13 <- lapply(B_L1, function(x) confusion_mat(x[151:180], 13))
bl1_combine_positions <- mapply(rbind, bl1_1, bl1_2, bl1_3, bl1_7, bl1_8, bl1_13, SIMPLIFY=FALSE)

# B_L2 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_L2/output1563381263", scenario = 0)
bl2_2and8 <- lapply(B_L2, function(x) confusion_mat(x[1:30], c(2, 8)))
bl2_3and8 <- lapply(B_L2, function(x) confusion_mat(x[31:60], c(3, 8)))
bl2_3and13 <- lapply(B_L2, function(x) confusion_mat(x[61:90], c(3, 13)))
bl2_7and8 <- lapply(B_L2, function(x) confusion_mat(x[91:120], c(7, 8)))
bl2_7and13 <- lapply(B_L2, function(x) confusion_mat(x[121:150], c(7, 13)))
bl2_8and13 <- lapply(B_L2, function(x) confusion_mat(x[151:180], c(8, 13)))
bl2_combine_positions <- mapply(rbind, bl2_2and8, bl2_3and8, bl2_3and13, bl2_7and8, bl2_7and13, bl2_8and13,
                                SIMPLIFY=FALSE)

# B_L3 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_L3/output1563381266", scenario = 0)
bl3_2and3and8 <- lapply(B_L3, function(x) confusion_mat(x[1:30], c(2, 3, 8)))
bl3_2and3and8 <- lapply(B_L3, function(x) confusion_mat(x[31:60], c(2, 7, 13)))
bl3_2and3and8 <- lapply(B_L3, function(x) confusion_mat(x[61:90], c(2, 8, 13)))
bl3_3and7and8 <- lapply(B_L3, function(x) confusion_mat(x[91:120], c(3, 7, 8)))
bl3_3and7and13 <- lapply(B_L3, function(x) confusion_mat(x[121:150], c(3, 7, 13)))
bl3_7and8and13 <- lapply(B_L3, function(x) confusion_mat(x[151:180], c(7, 8, 13)))
bl3_combine_positions <- mapply(rbind, bl3_2and3and8, bl3_2and3and8, bl3_2and3and8, bl3_3and7and8,
                                bl3_3and7and13, bl3_7and8and13, SIMPLIFY=FALSE)

# B_M1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_M1/output1563805196", scenario = 0)
bm1_1 <- lapply(B_M1, function(x) confusion_mat(x[1:30], 1))
bm1_2 <- lapply(B_M1, function(x) confusion_mat(x[31:60], 2))
bm1_3 <- lapply(B_M1, function(x) confusion_mat(x[61:90], 3))
bm1_7 <- lapply(B_M1, function(x) confusion_mat(x[91:120], 7))
bm1_8 <- lapply(B_M1, function(x) confusion_mat(x[121:150], 8))
bm1_13 <- lapply(B_M1, function(x) confusion_mat(x[151:180], 13))
bm1_combine_positions <- mapply(rbind, bm1_1, bm1_2, bm1_3, bm1_7, bm1_8, bm1_13, SIMPLIFY=FALSE)

# B_M2 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_M2/output1563805278", scenario = 0)
bm2_1 <- lapply(B_M2, function(x) confusion_mat(x[1:30], c(2, 8)))
bm2_2 <- lapply(B_M2, function(x) confusion_mat(x[31:60], c(3, 8)))
bm2_3 <- lapply(B_M2, function(x) confusion_mat(x[61:90], c(3, 13)))
bm2_7 <- lapply(B_M2, function(x) confusion_mat(x[91:120], c(7, 8)))
bm2_8 <- lapply(B_M2, function(x) confusion_mat(x[121:150], c(7, 13)))
bm2_13 <- lapply(B_M2, function(x) confusion_mat(x[151:180], c(8, 13)))
bm2_combine_positions <- mapply(rbind, bm2_1, bm2_2, bm2_3, bm2_7, bm2_8, bm2_13, SIMPLIFY=FALSE)

# B_M3 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_M3/output1563805180", scenario = 0)
bm3_1 <- lapply(B_M3, function(x) confusion_mat(x[1:30], c(2, 3, 8)))
bm3_2 <- lapply(B_M3, function(x) confusion_mat(x[31:60], c(2, 7, 13)))
bm3_3 <- lapply(B_M3, function(x) confusion_mat(x[61:90], c(2, 8, 13)))
bm3_7 <- lapply(B_M3, function(x) confusion_mat(x[91:120], c(3, 7, 8)))
bm3_8 <- lapply(B_M3, function(x) confusion_mat(x[121:150], c(3, 7, 13)))
bm3_13 <- lapply(B_M3, function(x) confusion_mat(x[151:180], c(7, 8, 13)))
bm3_combine_positions <- mapply(rbind, bm3_1, bm3_2, bm3_3, bm3_7, bm3_8, bm3_13, SIMPLIFY=FALSE)

# B_H1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_H1/output1564188836", scenario = 0)
bh1_1 <- lapply(B_H1, function(x) confusion_mat(x[1:30], 1))
bh1_2 <- lapply(B_H1, function(x) confusion_mat(x[31:60], 2))
bh1_3 <- lapply(B_H1, function(x) confusion_mat(x[61:90], 3))
bh1_7 <- lapply(B_H1, function(x) confusion_mat(x[91:120], 7))
bh1_8 <- lapply(B_H1, function(x) confusion_mat(x[121:150], 8))
bh1_13 <- lapply(B_H1, function(x) confusion_mat(x[151:180], 13))
bh1_combine_positions <- mapply(rbind, bh1_1, bh1_2, bh1_3, bh1_7, bh1_8, bh1_13, SIMPLIFY=FALSE)

# B_H2 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_H2/output1564188878", scenario = 0)
bh2_1 <- lapply(B_H2, function(x) confusion_mat(x[1:30], c(2, 8)))
bh2_2 <- lapply(B_H2, function(x) confusion_mat(x[31:60], c(3, 8)))
bh2_3 <- lapply(B_H2, function(x) confusion_mat(x[61:90], c(3, 13)))
bh2_7 <- lapply(B_H2, function(x) confusion_mat(x[91:120], c(7, 8)))
bh2_8 <- lapply(B_H2, function(x) confusion_mat(x[121:150], c(7, 13)))
bh2_13 <- lapply(B_H2, function(x) confusion_mat(x[151:180], c(8, 13)))
bh2_combine_positions <- mapply(rbind, bh2_1, bh2_2, bh2_3, bh2_7, bh2_8, bh2_13, SIMPLIFY=FALSE)

# B_H3 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/B_H3/output1564188916", scenario = 0)
bh3_1 <- lapply(B_H3, function(x) confusion_mat(x[1:30], c(2, 3, 8)))
bh3_2 <- lapply(B_H3, function(x) confusion_mat(x[31:60], c(2, 7, 13)))
bh3_3 <- lapply(B_H3, function(x) confusion_mat(x[61:90], c(2, 8, 13)))
bh3_7 <- lapply(B_H3, function(x) confusion_mat(x[91:120], c(3, 7, 8)))
bh3_8 <- lapply(B_H3, function(x) confusion_mat(x[121:150], c(3, 7, 13)))
bh3_13 <- lapply(B_H3, function(x) confusion_mat(x[151:180], c(7, 8, 13)))
bh3_combine_positions <- mapply(rbind, bh3_1, bh3_2, bh3_3, bh3_7, bh3_8, bh3_13, SIMPLIFY=FALSE)

#I_L1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/I_L1/output1563928846", scenario = 1)
il1_1 <- lapply(I_L1, function(x) confusion_mat(x[1:30], 1))
il1_2 <- lapply(I_L1, function(x) confusion_mat(x[31:60], 2))
il1_3 <- lapply(I_L1, function(x) confusion_mat(x[61:90], 3))
il1_7 <- lapply(I_L1, function(x) confusion_mat(x[91:120], 7))
il1_8 <- lapply(I_L1, function(x) confusion_mat(x[121:150], 8))
il1_13 <- lapply(I_L1, function(x) confusion_mat(x[151:180], 13))
il1_combine_positions <- mapply(rbind, il1_1, il1_2, il1_3, il1_7, il1_8, il1_13 , SIMPLIFY=FALSE)

#I_L2 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/I_L2/output1564868483", scenario = 1)
il2_2and8 <- lapply(I_L2, function(x) confusion_mat(x[1:30], c(2, 8)))
il2_3and8 <- lapply(I_L2, function(x) confusion_mat(x[31:60], c(3, 8)))
il2_3and13 <- lapply(I_L2, function(x) confusion_mat(x[61:90], c(3, 13)))
il2_7and8 <- lapply(I_L2, function(x) confusion_mat(x[91:120], c(7, 8)))
il2_7and13 <- lapply(I_L2, function(x) confusion_mat(x[121:150], c(7, 13)))
il2_8and13 <- lapply(I_L2, function(x) confusion_mat(x[151:180], c(8, 13)))
il2_combine_positions <- mapply(rbind, il2_2and8, il2_3and8, il2_3and13, il2_7and8, il2_7and13, il2_8and13,
                                SIMPLIFY=FALSE)

#I_L3 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/I_L3/output1566002099", scenario = 1)
il3_2and3and8 <- lapply(I_L3, function(x) confusion_mat(x[1:30], c(2, 3, 8)))
il3_2and3and8 <- lapply(I_L3, function(x) confusion_mat(x[31:60], c(2, 7, 13)))
il3_2and3and8 <- lapply(I_L3, function(x) confusion_mat(x[61:90], c(2, 8, 13)))
il3_3and7and8 <- lapply(I_L3, function(x) confusion_mat(x[91:120], c(3, 7, 8)))
il3_3and7and13 <- lapply(I_L3, function(x) confusion_mat(x[121:150], c(3, 7, 13)))
il3_7and8and13 <- lapply(I_L3, function(x) confusion_mat(x[151:180], c(7, 8, 13)))
il3_combine_positions <- mapply(rbind, il3_2and3and8, il3_2and3and8, il3_2and3and8, il3_3and7and8,
                                il3_3and7and13, il3_7and8and13, SIMPLIFY=FALSE)

#I_M1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/I_M1/output1566011552", scenario = 1)
im1_1 <- lapply(I_M1, function(x) confusion_mat(x[1:30], 1))
im1_2 <- lapply(I_M1, function(x) confusion_mat(x[31:60], 2))
im1_3 <- lapply(I_M1, function(x) confusion_mat(x[61:90], 3))
im1_7 <- lapply(I_M1, function(x) confusion_mat(x[91:120], 7))
im1_8 <- lapply(I_M1, function(x) confusion_mat(x[121:150], 8))
im1_13 <- lapply(I_M1, function(x) confusion_mat(x[151:180], 13))
im1_combine_positions <- mapply(rbind, im1_1, im1_2, im1_3, im1_7, im1_8, im1_13 , SIMPLIFY=FALSE)

#I_M2 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/I_M2/output1564868688", scenario = 1)
im2_2and8 <- lapply(I_M2, function(x) confusion_mat(x[1:30], c(2, 8)))
im2_3and8 <- lapply(I_M2, function(x) confusion_mat(x[31:60], c(3, 8)))
im2_3and13 <- lapply(I_M2, function(x) confusion_mat(x[61:90], c(3, 13)))
im2_7and8 <- lapply(I_M2, function(x) confusion_mat(x[91:120], c(7, 8)))
im2_7and13 <- lapply(I_M2, function(x) confusion_mat(x[121:150], c(7, 13)))
im2_8and13 <- lapply(I_M2, function(x) confusion_mat(x[151:180], c(8, 13)))
im2_combine_positions <- mapply(rbind, im2_2and8, im2_3and8, im2_3and13, im2_7and8, im2_7and13, im2_8and13,
                                SIMPLIFY=FALSE)

#I_M3 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/I_M3/output1566002287", scenario = 1)
im3_2and3and8 <- lapply(I_M3, function(x) confusion_mat(x[1:30], c(2, 3, 8)))
im3_2and3and8 <- lapply(I_M3, function(x) confusion_mat(x[31:60], c(2, 7, 13)))
im3_2and3and8 <- lapply(I_M3, function(x) confusion_mat(x[61:90], c(2, 8, 13)))
im3_3and7and8 <- lapply(I_M3, function(x) confusion_mat(x[91:120], c(3, 7, 8)))
im3_3and7and13 <- lapply(I_M3, function(x) confusion_mat(x[121:150], c(3, 7, 13)))
im3_7and8and13 <- lapply(I_M3, function(x) confusion_mat(x[151:180], c(7, 8, 13)))
im3_combine_positions <- mapply(rbind, im3_2and3and8, im3_2and3and8, im3_2and3and8, im3_3and7and8,
                                im3_3and7and13, im3_7and8and13, SIMPLIFY=FALSE)

#I_H1 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/I_H1/output1566412750", scenario = 1)
ih1_1 <- lapply(I_H1, function(x) confusion_mat(x[1:30], 1))
ih1_2 <- lapply(I_H1, function(x) confusion_mat(x[31:60], 2))
ih1_3 <- lapply(I_H1, function(x) confusion_mat(x[61:90], 3))
ih1_7 <- lapply(I_H1, function(x) confusion_mat(x[91:120], 7))
ih1_8 <- lapply(I_H1, function(x) confusion_mat(x[121:150], 8))
ih1_13 <- lapply(I_H1, function(x) confusion_mat(x[151:180], 13))
ih1_combine_positions <- mapply(rbind, ih1_1, ih1_2, ih1_3, ih1_7, ih1_8, ih1_13 , SIMPLIFY=FALSE)

#I_H2 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/I_H2/output1564868846", scenario = 1)
ih2_2and8 <- lapply(I_H2, function(x) confusion_mat(x[1:30], c(2, 8)))
ih2_3and8 <- lapply(I_H2, function(x) confusion_mat(x[31:60], c(3, 8)))
ih2_3and13 <- lapply(I_H2, function(x) confusion_mat(x[61:90], c(3, 13)))
ih2_7and8 <- lapply(I_H2, function(x) confusion_mat(x[91:120], c(7, 8)))
ih2_7and13 <- lapply(I_H2, function(x) confusion_mat(x[121:150], c(7, 13)))
ih2_8and13 <- lapply(I_H2, function(x) confusion_mat(x[151:180], c(8, 13)))
ih2_combine_positions <- mapply(rbind, ih2_2and8, ih2_3and8, ih2_3and13, ih2_7and8, ih2_7and13, ih2_8and13,
                                SIMPLIFY=FALSE)

#I_H3 <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/I_H3/output1566011636", scenario = 1)
ih3_2and3and8 <- lapply(I_H3, function(x) confusion_mat(x[1:30], c(2, 3, 8)))
ih3_2and3and8 <- lapply(I_H3, function(x) confusion_mat(x[31:60], c(2, 7, 13)))
ih3_2and3and8 <- lapply(I_H3, function(x) confusion_mat(x[61:90], c(2, 8, 13)))
ih3_3and7and8 <- lapply(I_H3, function(x) confusion_mat(x[91:120], c(3, 7, 8)))
ih3_3and7and13 <- lapply(I_H3, function(x) confusion_mat(x[121:150], c(3, 7, 13)))
ih3_7and8and13 <- lapply(I_H3, function(x) confusion_mat(x[151:180], c(7, 8, 13)))
ih3_combine_positions <- mapply(rbind, ih3_2and3and8, ih3_2and3and8, ih3_2and3and8, ih3_3and7and8,
                                ih3_3and7and13, ih3_7and8and13, SIMPLIFY=FALSE)

#C_L <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/C_L/output1563923314", scenario = 0)
cl <- lapply(C_L, function(x) FPR_control(x))

#C_M <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/C_M/output1563923273", scenario = 0)
cm <- lapply(C_M, function(x) FPR_control(x))

#C_H <- TBI_test_auto_allp(rep = 180, path = "E:/Julian_simulations/C_H/output1564188765", scenario = 0)
ch <- lapply(C_H, function(x) FPR_control(x))

###########################################################################################################
# 
# # I_L1_perm2 <- TBI_test_auto_allp_old(rep = 180, path = "E:/Julian_simulations/I_L1/output1563928846",
# #                                     scenario = 1, perm = 2)
# il1p2_1 <- lapply(I_L1_perm2 , function(x) confusion_mat(x[1:30], 1))
# il1p2_2 <- lapply(I_L1_perm2 , function(x) confusion_mat(x[31:60], 2))
# il1p2_3 <- lapply(I_L1_perm2 , function(x) confusion_mat(x[61:90], 3))
# il1p2_7 <- lapply(I_L1_perm2 , function(x) confusion_mat(x[91:120], 7))
# il1p2_8 <- lapply(I_L1_perm2 , function(x) confusion_mat(x[121:150], 8))
# il1p2_13 <- lapply(I_L1_perm2 , function(x) confusion_mat(x[151:180], 13))
# il1p2_combine_positions <- mapply(rbind, il1p2_1, il1p2_2, il1p2_3, il1p2_7, il1p2_8, il1p2_13
#                                   , SIMPLIFY=FALSE)
# 
# # I_L1_perm3 <- TBI_test_auto_allp_old(rep = 180, path = "E:/Julian_simulations/I_L1/output1563928846",
# #                                     scenario = 1, perm = 3)
# il1p3_1 <- lapply(I_L1_perm3 , function(x) confusion_mat(x[1:30], 1))
# il1p3_2 <- lapply(I_L1_perm3 , function(x) confusion_mat(x[31:60], 2))
# il1p3_3 <- lapply(I_L1_perm3 , function(x) confusion_mat(x[61:90], 3))
# il1p3_7 <- lapply(I_L1_perm3 , function(x) confusion_mat(x[91:120], 7))
# il1p3_8 <- lapply(I_L1_perm3 , function(x) confusion_mat(x[121:150], 8))
# il1p3_13 <- lapply(I_L1_perm3 , function(x) confusion_mat(x[151:180], 13))
# il1p3_combine_positions <- mapply(rbind, il1p3_1, il1p3_2, il1p3_3, il1p3_7, il1p3_8, il1p3_13
#                                   , SIMPLIFY=FALSE)
# 
# #C_L_perm2 <- TBI_test_auto_allp_old(rep = 180, path = "E:/Julian_simulations/C_L/output1563923314",
# #                                    scenario = 0, perm = 2)
# clp2 <- lapply(C_L_perm2, function(x) FPR_control(x))
# 
# #C_L_perm3 <- TBI_test_auto_allp_old(rep = 180, path = "E:/Julian_simulations/C_L/output1563923314",
# #                                    scenario = 0, perm = 3)
# clp3 <- lapply(C_L_perm3, function(x) FPR_control(x))

###########################################################################################################

# I_L1_2yearsafter <- TBI_test_auto_allp(rep = 180, latest = 102,
#                                            path = "E:/Julian_simulations/I_L1/output1563928846",
#                                            scenario = 1)
i2l1_1 <- lapply(I_L1_2yearsafter, function(x) confusion_mat(x[1:30], 1))
i2l1_2 <- lapply(I_L1_2yearsafter, function(x) confusion_mat(x[31:60], 2))
i2l1_3 <- lapply(I_L1_2yearsafter, function(x) confusion_mat(x[61:90], 3))
i2l1_7 <- lapply(I_L1_2yearsafter, function(x) confusion_mat(x[91:120], 7))
i2l1_8 <- lapply(I_L1_2yearsafter, function(x) confusion_mat(x[121:150], 8))
i2l1_13 <- lapply(I_L1_2yearsafter, function(x) confusion_mat(x[151:180], 13))
i2l1_combine_positions <- mapply(rbind, i2l1_1, i2l1_2, i2l1_3, i2l1_7, i2l1_8, i2l1_13, SIMPLIFY=FALSE)

# I_L1_3yearsafter <- TBI_test_auto_allp_old(rep = 180, latest = 103,
#                                            path = "E:/Julian_simulations/I_L1/output1563928846",
#                                            scenario = 1)
i3l1_1 <- lapply(I_L1_3yearsafter, function(x) confusion_mat(x[1:30], 1))
i3l1_2 <- lapply(I_L1_3yearsafter, function(x) confusion_mat(x[31:60], 2))
i3l1_3 <- lapply(I_L1_3yearsafter, function(x) confusion_mat(x[61:90], 3))
i3l1_7 <- lapply(I_L1_3yearsafter, function(x) confusion_mat(x[91:120], 7))
i3l1_8 <- lapply(I_L1_3yearsafter, function(x) confusion_mat(x[121:150], 8))
i3l1_13 <- lapply(I_L1_3yearsafter, function(x) confusion_mat(x[151:180], 13))
i3l1_combine_positions <- mapply(rbind, i3l1_1, i3l1_2, i3l1_3, i3l1_7, i3l1_8, i3l1_13, SIMPLIFY=FALSE)
# 
# I_L1_4yearsafter <- TBI_test_auto_allp_old(rep = 180, latest = 104,
#                                            path = "E:/Julian_simulations/I_L1/output1563928846",
#                                            scenario = 1)
i4l1_1 <- lapply(I_L1_4yearsafter, function(x) confusion_mat(x[1:30], 1))
i4l1_2 <- lapply(I_L1_4yearsafter, function(x) confusion_mat(x[31:60], 2))
i4l1_3 <- lapply(I_L1_4yearsafter, function(x) confusion_mat(x[61:90], 3))
i4l1_7 <- lapply(I_L1_4yearsafter, function(x) confusion_mat(x[91:120], 7))
i4l1_8 <- lapply(I_L1_4yearsafter, function(x) confusion_mat(x[121:150], 8))
i4l1_13 <- lapply(I_L1_4yearsafter, function(x) confusion_mat(x[151:180], 13))
i4l1_combine_positions <- mapply(rbind, i4l1_1, i4l1_2, i4l1_3, i4l1_7, i4l1_8, i4l1_13, SIMPLIFY=FALSE)
# 
# I_L1_5yearsafter <- TBI_test_auto_allp_old(rep = 180, latest = 105,
#                                           path = "E:/Julian_simulations/I_L1/output1563928846",
#                                           scenario = 1)
i5l1_1 <- lapply(I_L1_5yearsafter, function(x) confusion_mat(x[1:30], 1))
i5l1_2 <- lapply(I_L1_5yearsafter, function(x) confusion_mat(x[31:60], 2))
i5l1_3 <- lapply(I_L1_5yearsafter, function(x) confusion_mat(x[61:90], 3))
i5l1_7 <- lapply(I_L1_5yearsafter, function(x) confusion_mat(x[91:120], 7))
i5l1_8 <- lapply(I_L1_5yearsafter, function(x) confusion_mat(x[121:150], 8))
i5l1_13 <- lapply(I_L1_5yearsafter, function(x) confusion_mat(x[151:180], 13))
i5l1_combine_positions <- mapply(rbind, i5l1_1, i5l1_2, i5l1_3, i5l1_7, i5l1_8, i5l1_13, SIMPLIFY=FALSE)
# 
# I_H3_2yearsafter <- TBI_test_auto_allp_old(rep = 180, latest = 102,
                                           # path = "E:/Julian_simulations/I_H3/output1566011636",
                                           # scenario = 1)
i2h3_1 <- lapply(I_H3_2yearsafter, function(x) confusion_mat(x[1:30], c(2, 3, 8)))
i2h3_2 <- lapply(I_H3_2yearsafter, function(x) confusion_mat(x[31:60], c(2, 7, 13)))
i2h3_3 <- lapply(I_H3_2yearsafter, function(x) confusion_mat(x[61:90], c(2, 8, 13)))
i2h3_7 <- lapply(I_H3_2yearsafter, function(x) confusion_mat(x[91:120], c(3, 7, 8)))
i2h3_8 <- lapply(I_H3_2yearsafter, function(x) confusion_mat(x[121:150], c(3, 7, 13)))
i2h3_13 <- lapply(I_H3_2yearsafter, function(x) confusion_mat(x[151:180], c(7, 8, 13)))
i2h3_combine_positions <- mapply(rbind, i2h3_1, i2h3_2, i2h3_3, i2h3_7, i2h3_8, i2h3_13, SIMPLIFY=FALSE)
# 
I_H3_3yearsafter <- TBI_test_auto_allp_old(rep = 180, latest = 103,
                                           path = "E:/Julian_simulations/I_H3/output1566011636",
                                           scenario = 1)
i3h3_1 <- lapply(I_H3_3yearsafter, function(x) confusion_mat(x[1:30], c(2, 3, 8)))
i3h3_2 <- lapply(I_H3_3yearsafter, function(x) confusion_mat(x[31:60], c(2, 7, 13)))
i3h3_3 <- lapply(I_H3_3yearsafter, function(x) confusion_mat(x[61:90], c(2, 8, 13)))
i3h3_7 <- lapply(I_H3_3yearsafter, function(x) confusion_mat(x[91:120], c(3, 7, 8)))
i3h3_8 <- lapply(I_H3_3yearsafter, function(x) confusion_mat(x[121:150], c(3, 7, 13)))
i3h3_13 <- lapply(I_H3_3yearsafter, function(x) confusion_mat(x[151:180], c(7, 8, 13)))
i3h3_combine_positions <- mapply(rbind, i3h3_1, i3h3_2, i3h3_3, i3h3_7, i3h3_8, i3h3_13, SIMPLIFY=FALSE)
# 
I_H3_4yearsafter <- TBI_test_auto_allp_old(rep = 180, latest = 104,
                                           path = "E:/Julian_simulations/I_H3/output1566011636",
                                           scenario = 1)
i4h3_1 <- lapply(I_H3_4yearsafter, function(x) confusion_mat(x[1:30], c(2, 3, 8)))
i4h3_2 <- lapply(I_H3_4yearsafter, function(x) confusion_mat(x[31:60], c(2, 7, 13)))
i4h3_3 <- lapply(I_H3_4yearsafter, function(x) confusion_mat(x[61:90], c(2, 8, 13)))
i4h3_7 <- lapply(I_H3_4yearsafter, function(x) confusion_mat(x[91:120], c(3, 7, 8)))
i4h3_8 <- lapply(I_H3_4yearsafter, function(x) confusion_mat(x[121:150], c(3, 7, 13)))
i4h3_13 <- lapply(I_H3_4yearsafter, function(x) confusion_mat(x[151:180], c(7, 8, 13)))
i4h3_combine_positions <- mapply(rbind, i4h3_1, i4h3_2, i4h3_3, i4h3_7, i4h3_8, i4h3_13, SIMPLIFY=FALSE)
# 
I_H3_5yearsafter <- TBI_test_auto_allp_old(rep = 180, latest = 105,
                                          path = "E:/Julian_simulations/I_H3/output1566011636",
                                          scenario = 1)
i5h3_1 <- lapply(I_H3_5yearsafter, function(x) confusion_mat(x[1:30], c(2, 3, 8)))
i5h3_2 <- lapply(I_H3_5yearsafter, function(x) confusion_mat(x[31:60], c(2, 7, 13)))
i5h3_3 <- lapply(I_H3_5yearsafter, function(x) confusion_mat(x[61:90], c(2, 8, 13)))
i5h3_7 <- lapply(I_H3_5yearsafter, function(x) confusion_mat(x[91:120], c(3, 7, 8)))
i5h3_8 <- lapply(I_H3_5yearsafter, function(x) confusion_mat(x[121:150], c(3, 7, 13)))
i5h3_13 <- lapply(I_H3_5yearsafter, function(x) confusion_mat(x[151:180], c(7, 8, 13)))
i5h3_combine_positions <- mapply(rbind, i5h3_1, i5h3_2, i5h3_3, i5h3_7, i5h3_8, i5h3_13, SIMPLIFY=FALSE)
# 
##########################################################################################################

# I_L1_2yearsbefore <- TBI_test_auto_allp_old(rep = 180, earliest = 99,
#                                            path = "E:/Julian_simulations/I_L1/output1563928846",
#                                            scenario = 1)
i2l1b_1 <- lapply(I_L1_2yearsbefore, function(x) confusion_mat(x[1:30], 1))
i2l1b_2 <- lapply(I_L1_2yearsbefore, function(x) confusion_mat(x[31:60], 2))
i2l1b_3 <- lapply(I_L1_2yearsbefore, function(x) confusion_mat(x[61:90], 3))
i2l1b_7 <- lapply(I_L1_2yearsbefore, function(x) confusion_mat(x[91:120], 7))
i2l1b_8 <- lapply(I_L1_2yearsbefore, function(x) confusion_mat(x[121:150], 8))
i2l1b_13 <- lapply(I_L1_2yearsbefore, function(x) confusion_mat(x[151:180], 13))
i2l1b_combine_positions <- mapply(rbind, i2l1b_1, i2l1b_2, i2l1b_3, i2l1b_7, i2l1b_8, i2l1b_13, SIMPLIFY=FALSE)

# I_L1_3yearsbefore <- TBI_test_auto_allp_old(rep = 180, earliest = 98,
#                                            path = "E:/Julian_simulations/I_L1/output1563928846",
#                                            scenario = 1)
i3l1b_1 <- lapply(I_L1_3yearsbefore, function(x) confusion_mat(x[1:30], 1))
i3l1b_2 <- lapply(I_L1_3yearsbefore, function(x) confusion_mat(x[31:60], 2))
i3l1b_3 <- lapply(I_L1_3yearsbefore, function(x) confusion_mat(x[61:90], 3))
i3l1b_7 <- lapply(I_L1_3yearsbefore, function(x) confusion_mat(x[91:120], 7))
i3l1b_8 <- lapply(I_L1_3yearsbefore, function(x) confusion_mat(x[121:150], 8))
i3l1b_13 <- lapply(I_L1_3yearsbefore, function(x) confusion_mat(x[151:180], 13))
i3l1b_combine_positions <- mapply(rbind, i3l1b_1, i3l1b_2, i3l1b_3, i3l1b_7, i3l1b_8, i3l1b_13, SIMPLIFY=FALSE)

# I_L1_4yearsbefore <- TBI_test_auto_allp_old(rep = 180, earliest = 97,
#                                            path = "E:/Julian_simulations/I_L1/output1563928846",
#                                            scenario = 1)
i4l1b_1 <- lapply(I_L1_4yearsbefore, function(x) confusion_mat(x[1:30], 1))
i4l1b_2 <- lapply(I_L1_4yearsbefore, function(x) confusion_mat(x[31:60], 2))
i4l1b_3 <- lapply(I_L1_4yearsbefore, function(x) confusion_mat(x[61:90], 3))
i4l1b_7 <- lapply(I_L1_4yearsbefore, function(x) confusion_mat(x[91:120], 7))
i4l1b_8 <- lapply(I_L1_4yearsbefore, function(x) confusion_mat(x[121:150], 8))
i4l1b_13 <- lapply(I_L1_4yearsbefore, function(x) confusion_mat(x[151:180], 13))
i4l1b_combine_positions <- mapply(rbind, i4l1b_1, i4l1b_2, i4l1b_3, i4l1b_7, i4l1b_8, i4l1b_13, SIMPLIFY=FALSE)

# I_L1_5yearsbefore <- TBI_test_auto_allp_old(rep = 180, earliest = 96,
#                                           path = "E:/Julian_simulations/I_L1/output1563928846",
#                                           scenario = 1)
i5l1b_1 <- lapply(I_L1_5yearsbefore, function(x) confusion_mat(x[1:30], 1))
i5l1b_2 <- lapply(I_L1_5yearsbefore, function(x) confusion_mat(x[31:60], 2))
i5l1b_3 <- lapply(I_L1_5yearsbefore, function(x) confusion_mat(x[61:90], 3))
i5l1b_7 <- lapply(I_L1_5yearsbefore, function(x) confusion_mat(x[91:120], 7))
i5l1b_8 <- lapply(I_L1_5yearsbefore, function(x) confusion_mat(x[121:150], 8))
i5l1b_13 <- lapply(I_L1_5yearsbefore, function(x) confusion_mat(x[151:180], 13))
i5l1b_combine_positions <- mapply(rbind, i5l1b_1, i5l1b_2, i5l1b_3, i5l1b_7, i5l1b_8, i5l1b_13, SIMPLIFY=FALSE)

# I_H3_2yearsbefore <- TBI_test_auto_allp_old(rep = 180, earliest = 99,
#                                            path = "E:/Julian_simulations/I_H3/output1566011636",
#                                            scenario = 1)
i2h3b_1 <- lapply(I_H3_2yearsbefore, function(x) confusion_mat(x[1:30], c(2, 3, 8)))
i2h3b_2 <- lapply(I_H3_2yearsbefore, function(x) confusion_mat(x[31:60], c(2, 7, 13)))
i2h3b_3 <- lapply(I_H3_2yearsbefore, function(x) confusion_mat(x[61:90], c(2, 8, 13)))
i2h3b_7 <- lapply(I_H3_2yearsbefore, function(x) confusion_mat(x[91:120], c(3, 7, 8)))
i2h3b_8 <- lapply(I_H3_2yearsbefore, function(x) confusion_mat(x[121:150], c(3, 7, 13)))
i2h3b_13 <- lapply(I_H3_2yearsbefore, function(x) confusion_mat(x[151:180], c(7, 8, 13)))
i2h3b_combine_positions <- mapply(rbind, i2h3b_1, i2h3b_2, i2h3b_3, i2h3b_7, i2h3b_8, i2h3b_13, SIMPLIFY=FALSE)

# I_H3_3yearsbefore <- TBI_test_auto_allp_old(rep = 180, earliest = 98,
#                                            path = "E:/Julian_simulations/I_H3/output1566011636",
#                                            scenario = 1)
i3h3b_1 <- lapply(I_H3_3yearsbefore, function(x) confusion_mat(x[1:30], c(2, 3, 8)))
i3h3b_2 <- lapply(I_H3_3yearsbefore, function(x) confusion_mat(x[31:60], c(2, 7, 13)))
i3h3b_3 <- lapply(I_H3_3yearsbefore, function(x) confusion_mat(x[61:90], c(2, 8, 13)))
i3h3b_7 <- lapply(I_H3_3yearsbefore, function(x) confusion_mat(x[91:120], c(3, 7, 8)))
i3h3b_8 <- lapply(I_H3_3yearsbefore, function(x) confusion_mat(x[121:150], c(3, 7, 13)))
i3h3b_13 <- lapply(I_H3_3yearsbefore, function(x) confusion_mat(x[151:180], c(7, 8, 13)))
i3h3b_combine_positions <- mapply(rbind, i3h3b_1, i3h3b_2, i3h3b_3, i3h3b_7, i3h3b_8, i3h3b_13, SIMPLIFY=FALSE)

# I_H3_4yearsbefore <- TBI_test_auto_allp_old(rep = 180, earliest = 97,
#                                            path = "E:/Julian_simulations/I_H3/output1566011636",
#                                            scenario = 1)
i4h3b_1 <- lapply(I_H3_4yearsbefore, function(x) confusion_mat(x[1:30], c(2, 3, 8)))
i4h3b_2 <- lapply(I_H3_4yearsbefore, function(x) confusion_mat(x[31:60], c(2, 7, 13)))
i4h3b_3 <- lapply(I_H3_4yearsbefore, function(x) confusion_mat(x[61:90], c(2, 8, 13)))
i4h3b_7 <- lapply(I_H3_4yearsbefore, function(x) confusion_mat(x[91:120], c(3, 7, 8)))
i4h3b_8 <- lapply(I_H3_4yearsbefore, function(x) confusion_mat(x[121:150], c(3, 7, 13)))
i4h3b_13 <- lapply(I_H3_4yearsbefore, function(x) confusion_mat(x[151:180], c(7, 8, 13)))
i4h3b_combine_positions <- mapply(rbind, i4h3b_1, i4h3b_2, i4h3b_3, i4h3b_7, i4h3b_8, i4h3b_13, SIMPLIFY=FALSE)

# I_H3_5yearsbefore <- TBI_test_auto_allp_old(rep = 180, earliest = 96,
#                                           path = "E:/Julian_simulations/I_H3/output1566011636",
#                                           scenario = 1)
i5h3b_1 <- lapply(I_H3_5yearsbefore, function(x) confusion_mat(x[1:30], c(2, 3, 8)))
i5h3b_2 <- lapply(I_H3_5yearsbefore, function(x) confusion_mat(x[31:60], c(2, 7, 13)))
i5h3b_3 <- lapply(I_H3_5yearsbefore, function(x) confusion_mat(x[61:90], c(2, 8, 13)))
i5h3b_7 <- lapply(I_H3_5yearsbefore, function(x) confusion_mat(x[91:120], c(3, 7, 8)))
i5h3b_8 <- lapply(I_H3_5yearsbefore, function(x) confusion_mat(x[121:150], c(3, 7, 13)))
i5h3b_13 <- lapply(I_H3_5yearsbefore, function(x) confusion_mat(x[151:180], c(7, 8, 13)))
i5h3b_combine_positions <- mapply(rbind, i5h3b_1, i5h3b_2, i5h3b_3, i5h3b_7, i5h3b_8, i5h3b_13, SIMPLIFY=FALSE)

###########################################################################################################

 # save(B_L1, B_L2, B_L3, B_M1, B_M2, B_M3, B_H1, B_H2, B_H3,
 #      I_L1, I_L2, I_L3, I_M1, I_M2, I_M3, I_H1, I_H2, I_H3,
 #      C_L, C_M, C_H,
 #      I_L1_perm2, I_L1_perm3, C_L_perm2, C_L_perm3,
 #      I_L1_2yearsafter, I_L1_3yearsafter, I_L1_4yearsafter, I_L1_5yearsafter,
 #      I_H3_2yearsafter, I_H3_3yearsafter, I_H3_4yearsafter, I_H3_5yearsafter,
 #      I_L1_2yearsbefore, I_L1_3yearsbefore, I_L1_4yearsbefore, I_L1_5yearsbefore,
 #      I_H3_2yearsbefore, I_H3_3yearsbefore, I_H3_4yearsbefore, I_H3_5yearsbefore,
 #      file = "E:/Julian_simulations/data.RData")


###########################################################################################################