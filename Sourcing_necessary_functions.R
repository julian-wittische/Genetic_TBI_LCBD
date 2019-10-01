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

source("TBI_test_auto_allp.R")
source("TBI_test_auto_allp_old.R")
source("TBIold.R")
source("TBI_test_auto_allp_TBIgenJW_test.R")
source("TBIgenJW_test.R")
source("CDmicro2genind.R")

source("PlotRates.R")

'%!in%' <- function(x,y)!('%in%'(x,y))

library(gtools)
library(adespatial)
library(vegan)
library(ggplot2)
library(reshape2)
library(cowplot)