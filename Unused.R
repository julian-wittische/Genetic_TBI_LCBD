##### ILLUSTRATE FALSE 
### Pick one replicate (temp)
rep <- 0

test <- TBI(CD2TBI(rep, 199), CD2TBI(rep, 200), method="chord", n=1000)
test$p.TBI
test$p.adj # 1 FALSE POSITIVE

test <- TBI(CD2TBI(rep, 199), CD2TBI(rep, 201), method="chord", n=1000)
test$p.TBI
test$p.adj # 1 TRUE POSITIVE

test <- TBI(CD2TBI(rep, 200), CD2TBI(rep, 201), method="chord", n=1000)
test$p.TBI
test$p.adj # 1 TRUE POSITIVE

# Source TBI function
source("TBI.R")

# Data directory (smaller sample for now)
setwd('..')
setwd(paste0(getwd(),"/TBI_simulation"))
setwd(paste0(getwd(),"/simulation_bottelneck"))
setwd(paste0(getwd(),"/1000replicats1535732106"))