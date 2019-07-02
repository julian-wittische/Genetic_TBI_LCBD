# Reading the source files
# Julian Wittische 
# February 2019

# Source script to install and load necessary packages
source("Installing_missing_packages.R")

# Source function which calculates TBI and tests
source("TBI.R")

# Source function which creates TBI input
source("CD2TBI.R")

# Data directory (smaller sample for now)
setwd('..')
setwd(paste0(getwd(),"/TBI_simulation"))
setwd(paste0(getwd(),"/simulation_bottelneck"))
setwd(paste0(getwd(),"/1000replicats1535732106"))
