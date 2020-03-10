setwd("E:/Globus/I_L1/output1563928846")
T1 <- CDmicro2genind(0, 100, scenario= 1, nloci = 100, nalleles = 2)
T2 <- CDmicro2genind(0, 101, scenario= 1, nloci = 100, nalleles = 2)
# new <- TGI2(T1, T2, nperm = 999)
# old <- TBIgenJW_test2(T1, T2, nperm = 999)
library(microbenchmark)
mbm1 <- microbenchmark(TGI2(T1, T2, nperm = 999), times=10)
mbm2 <- microbenchmark(TBIgenJW_test2(T1, T2, nperm = 999), times=1)
testauto <- TGI_auto_allp(path="E:/Globus/I_L1/output1563928846")


