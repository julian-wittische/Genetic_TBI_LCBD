# Reading the source files
# Julian Wittische 
# July 2019

# Source script to install and load necessary packages
source("Installing_missing_packages.R")

# Source function which creates TBI input from CDMetaPOP
source("CD2TBI.R")

# Source function which computes True Positive Rates and False negative Rates, as well as other
# confusion matrix-derived indices
#source("TPR_FNR.R")
source("TPR_FNR_with_pop_argument.R")

'%!in%' <- function(x,y)!('%in%'(x,y))