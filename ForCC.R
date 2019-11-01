# Source function which creates TBI input from CDMetaPOP
source("CD2TBI.R")

# Source function which computes True Positive Rates and False negative Rates, as well as other
# confusion matrix-derived indices
#source("TPR_FNR.R")
source("TPR_FNR_with_pop_argument.R")

source("TBI_test_auto_allp.R")
source("TBI_test_auto_allp_old.R")
source("TBIold.R")
source("TBI_test_auto_allp_TBIgenJW_test.R")
source("TBIgenJW_test.R")
source("Cdmicro2genind.R")

source("PlotRates.R")

'%!in%' <- function(x,y)!('%in%'(x,y))

library(gtools)
library(adespatial)
library(vegan)
library(ggplot2)
library(reshape2)
library(cowplot)
library(extrafont)
library(adegenet)
library(pegas)
library(hierfstat)

start <- Sys.time()
B_L1_m <- TBI_test_auto_allp_TBIgenJW_test(rep = 180, path = "/project/6005201/p1092272/Globus"
, scenario = 0,
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
save(B_L1_m, file=B_L1_m.RData)
