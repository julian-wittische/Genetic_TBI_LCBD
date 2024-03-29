start <- proc.time()
TEST <- TBI_test_auto_allp_TBIgenJW_test(path = "E:/Julian_simulations/MICRO_B_L1/output1568926112", scenario = 0,
                                         nloci = 10, nalleles = 10)
end <- proc.time()
(end-start)/60/60

TEST2 <- TBI_test_auto_allp(rep = 2, path = "E:/Julian_simulations/B_L1/output1563381261", scenario = 0)

#
# # Various adegenet tests
# genp@tab[1:2,]
# genp@tab[2,] <- genp@tab[1,]
# dist.genpop(genp[1:2,])

# setwd("E:/Julian_simulations/MICRO_B_L1/output1568926112")
# yeeet1 <- CDmicro2genpop(0,100)
# yeeet2 <- CDmicro2genpop(0,101)
# 
# setwd("E:/Julian_simulations/B_L1/output1563381261")
# yeeet3 <- CDmicro2genpop(0,100, nloci=100, nalleles=2)
# yeeet4 <- CDmicro2genpop(0,101, nloci=100, nalleles=2)
# 
# yeeet5 <- CD2TBI(0, 100)
#   
# dissim_test(yeeet1, yeeet2)
# dissim_test(yeeet3, yeeet4)

TBIgenJW_test(yeeet3, yeeet4, nperm=999)
TBIgenJW_test(CDmicro2genpop(1,100, nloci=100, nalleles=2), CDmicro2genpop(1,101, nloci=100, nalleles=2), nperm=99, method=4)



#
B_L1_light <- TBI_test_auto_allp_TBIgenJW_test(rep = 180,
                                               path = "E:/Julian_simulations/B_L1/output1563381261",
                                               scenario = 0)
bl1_1_light <- lapply(B_L1_light, function(x) confusion_mat(x[1:30], 1))
bl1_1[[13]]
bl1_1_light[[13]]
##

# Testing TBI_test_auto()
setwd("C:/Users/jwitt/OneDrive/Desktop/Git_Projects/Genetic_TBI_LCBD")
a <- "E:/Julian_simulations/B_L/output1563381261"

b <- TBI_test_auto(rep = 100, path = a)
confusion_mat(b[1:30], 1)

mean(confusion_mat(b[1:30], 1)$FPR, na.rm = TRUE)
sd(confusion_mat(b[1:30], 1)$FPR, na.rm = TRUE)

mean(confusion_mat(b[1:30], 1)$FNR, na.rm = TRUE)
sd(confusion_mat(b[1:30], 1)$FNR, na.rm = TRUE)

# Combine from different targets
c <- rbind(confusion_mat(b[1:30], 1),
           confusion_mat(b[31:60], 2),
           confusion_mat(b[61:90], 3)
           )

mean(c$FPR, na.rm = TRUE)
sd(c$FPR, na.rm = TRUE)
mean(c$FNR, na.rm = TRUE)
sd(c$FNR, na.rm = TRUE)

#Testing two pops
aa <- "E:/Julian_simulations/B_L2/output1563381263"
bb <- TBI_test_auto(rep = 90, path = aa)
confusion_mat(bb[1:30], 1)

# Combine from different targets
cc <- rbind(confusion_mat(bb[1:30], c(2, 8)),
           confusion_mat(bb[31:60], c(3, 8)),
           confusion_mat(bb[61:90], c(3, 13))
)

mean(cc$FPR, na.rm = TRUE)
sd(cc$FPR, na.rm = TRUE)
mean(cc$FNR, na.rm = TRUE)
sd(cc$FNR, na.rm = TRUE)

# Testing years later
byl <- TBI_test_auto(rep = 30, path = a, latest = 102)

mean(confusion_mat(byl, 1)$FPR, na.rm = TRUE)
sd(confusion_mat(byl, 1)$FPR, na.rm = TRUE)

mean(confusion_mat(byl, 1)$FNR, na.rm = TRUE)
sd(confusion_mat(byl, 1)$FNR, na.rm = TRUE)

byl10 <- TBI_test_auto(rep = 30, path = a, latest = 110)

mean(confusion_mat(byl10, 1)$FPR, na.rm = TRUE)
sd(confusion_mat(byl10, 1)$FPR, na.rm = TRUE)

mean(confusion_mat(byl10, 1)$FNR, na.rm = TRUE)
sd(confusion_mat(byl10, 1)$FNR, na.rm = TRUE)

##############################################################################################################
setwd("C:/Users/jwitt/OneDrive/Desktop/Git_Projects/Genetic_TBI_LCBD")
a <- "E:/Julian_simulations/B_M/output1563805196"

b <- TBI_test_auto(rep = 60, path = a)
confusion_mat(b[1:30], 1)

mean(confusion_mat(b[1:30], 1)$FPR, na.rm = TRUE)
sd(confusion_mat(b[1:30], 1)$FPR, na.rm = TRUE)

mean(confusion_mat(b[1:30], 1)$FNR, na.rm = TRUE)
sd(confusion_mat(b[1:30], 1)$FNR, na.rm = TRUE)

# Combine from different targets
c <- rbind(confusion_mat(b[1:30], 1),
           confusion_mat(b[31:60], 2),
           confusion_mat(b[61:90], 3)
)

mean(c$FPR, na.rm = TRUE)
sd(c$FPR, na.rm = TRUE)
mean(c$FNR, na.rm = TRUE)
sd(c$FNR, na.rm = TRUE)

#Testing two pops
aa <- "E:/Julian_simulations/B_L2/output1563381263"
bb <- TBI_test_auto(rep = 90, path = aa)
confusion_mat(bb[1:30], 1)

# Combine from different targets
cc <- rbind(confusion_mat(bb[1:30], c(2, 8)),
            confusion_mat(bb[31:60], c(3, 8)),
            confusion_mat(bb[61:90], c(3, 13))
)

mean(cc$FPR, na.rm = TRUE)
sd(cc$FPR, na.rm = TRUE)
mean(cc$FNR, na.rm = TRUE)
sd(cc$FNR, na.rm = TRUE)

# Testing years later
byl <- TBI_test_auto(rep = 30, path = a, latest = 102)

mean(confusion_mat(byl, 1)$FPR, na.rm = TRUE)
sd(confusion_mat(byl, 1)$FPR, na.rm = TRUE)

mean(confusion_mat(byl, 1)$FNR, na.rm = TRUE)
sd(confusion_mat(byl, 1)$FNR, na.rm = TRUE)

byl10 <- TBI_test_auto(rep = 30, path = a, latest = 110)

mean(confusion_mat(byl10, 1)$FPR, na.rm = TRUE)
sd(confusion_mat(byl10, 1)$FPR, na.rm = TRUE)

mean(confusion_mat(byl10, 1)$FNR, na.rm = TRUE)
sd(confusion_mat(byl10, 1)$FNR, na.rm = TRUE)
##############################################################################################################
setwd("C:/Users/jwitt/OneDrive/Desktop/Git_Projects/Genetic_TBI_LCBD")
a <- "E:/Julian_simulations/B_L1/output1563381261"

b <- TBI_test_auto(rep = 180, path = a, alpha = 0.0025, scenario = 0)
confusion_mat(b[1:30], 1)

bb <- TBI_test_auto_allp(rep = 30, path = a, scenario = 0)
lapply(bb, function(x) confusion_mat(x, 1))
confusion_mat(bb[1][1:30], 1)

mean(confusion_mat(b[1:30], 1)$FPR, na.rm = TRUE)
sd(confusion_mat(b[1:30], 1)$FPR, na.rm = TRUE)

mean(confusion_mat(b[1:30], 1)$FNR, na.rm = TRUE)
sd(confusion_mat(b[1:30], 1)$FNR, na.rm = TRUE)

# Combine from different targets
c <- rbind(confusion_mat(b[1:30], 1),
           confusion_mat(b[31:60], 2),
           confusion_mat(b[61:90], 3)
)

mean(c$FPR, na.rm = TRUE)
sd(c$FPR, na.rm = TRUE)
mean(c$FNR, na.rm = TRUE)
sd(c$FNR, na.rm = TRUE)

#Testing two pops
aa <- "E:/Julian_simulations/C_L/output1563381263"
bb <- TBI_test_auto(rep = 90, path = aa)
confusion_mat(bb[1:30], 1)

# Combine from different targets
cc <- rbind(confusion_mat(bb[1:30], c(2, 8)),
            confusion_mat(bb[31:60], c(3, 8)),
            confusion_mat(bb[61:90], c(3, 13))
)

mean(cc$FPR, na.rm = TRUE)
sd(cc$FPR, na.rm = TRUE)
mean(cc$FNR, na.rm = TRUE)
sd(cc$FNR, na.rm = TRUE)

# Testing years later
byl <- TBI_test_auto(rep = 30, path = a, latest = 102)

mean(confusion_mat(byl, 1)$FPR, na.rm = TRUE)
sd(confusion_mat(byl, 1)$FPR, na.rm = TRUE)

mean(confusion_mat(byl, 1)$FNR, na.rm = TRUE)
sd(confusion_mat(byl, 1)$FNR, na.rm = TRUE)

byl10 <- TBI_test_auto(rep = 30, path = a, latest = 110)

mean(confusion_mat(byl10, 1)$FPR, na.rm = TRUE)
sd(confusion_mat(byl10, 1)$FPR, na.rm = TRUE)

mean(confusion_mat(byl10, 1)$FNR, na.rm = TRUE)
sd(confusion_mat(byl10, 1)$FNR, na.rm = TRUE)

##############################################################################################################
# Testing if my algorithm is right
# n: 14
a <- c(1,4,13) #TRUE TARGETS
b <- 1:7 #RESULTS
# TP: 2 (1, 4)
# FN: 1 (13)
# FP: 5 (2, 3, 5, 6, 7)
# TN: 6 (8, 9, 10, 11, 12, 14)

a %in% b

b %in% a

a %!in% b

b %!in% a

sum(b%in%a)
sum(b%!in%a)
sum(a%!in%b)
14 - length(a) - sum(b%!in%a)
 