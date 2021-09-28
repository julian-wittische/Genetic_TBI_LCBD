# Sourcing functions and loading packages
# Julian Wittische 
# July 2020
#setwd("C:/Users/jwitt/OneDrive/Desktop/Git_Projects/Genetic_TBI_LCBD")
setwd("C:/Users/Utilisateur/Desktop/Projects/Genetic_TBI_LCBD")

# Source script to install and load necessary packages
source("Installing_missing_packages.R")

# Source function which creates TBI input (using adegenet) from CDMetaPOP output
source("Cdmicro2genind.R")

# Source TGI function
source("TBIgenJW_test.R") #oldest
source("TBIgenJW_test2.R") #old
source("TGI.R") #old
source("TGI2.R") #new/best

# Source function which automatize TGI across replicates
source("TBI_test_auto_allp_TBIgenJW_test.R")
source("TGI_auto_allp.R")

# Source function which computes True Positive Rates and False negative Rates, as well as other
# confusion matrix-derived indices
source("TPR_FNR_with_pop_argument.R")

# Source functions to extract perf
source("Performances.R")

# Useful
'%!in%' <- function(x,y)!('%in%'(x,y))

# Loading necessary packages (not sure all are still necessary)
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
library(colorspace)
library(Rmisc)
library(poppr)

