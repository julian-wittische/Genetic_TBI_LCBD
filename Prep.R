# Reading the source files
# Julian Wittische 
# February 2019

# Scripts directory
setwd("/home/labo_james/Desktop/Projects.GitHub/Genetic_TBI_LCBD")

# Source script to install and load necessary packages
source("Installing_missing_packages.R")

# Source function which calculates TBI and tests
source("TBI.R")

# Source function which creates TBI input
source("CD2TBI.R")

# Data directory (smaller sample for now)
setwd(paste0(getwd(),"/Test10"))