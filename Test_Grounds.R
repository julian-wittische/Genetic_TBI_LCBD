# Testing TBI_test_auto()
setwd("C:/Users/jwitt/OneDrive/Desktop/Git_Projects/Genetic_TBI_LCBD")
a <- "E:/Julian_simulations/B_L/output1563381261"
b <- TBI_test_auto(rep = 100, path = a)

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
 