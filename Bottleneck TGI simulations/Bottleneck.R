################################################################################
###### Julian Wittische - Spring 2024 - Simulating bottleneck for TGI test #####
################################################################################
# Most of this code comes from the "Landscape Genetic Data Analysis in R" editor by Helen Wagner
# The section it comes from has also be contributed by Bernd Gruber and Erin Landguth (hi!!)
################################################################################

################################################################################
########## Libraries and functions
##### Libraries
devtools::install_github("hhwagner1/LandGenCourse")
if(!requireNamespace("popgraph", quietly = TRUE))
{
  install.packages(c("RgoogleMaps", "geosphere", "proto", "sampling",
                     "seqinr", "spacetime", "spdep"), dependencies=TRUE)
  remotes::install_github("dyerlab/popgraph")
}
if(!requireNamespace("gstudio", quietly = TRUE)) remotes::install_github("dyerlab/gstudio")
library(LandGenCourse)
library(PopGenReport)
library(secr)
library(raster)
library(beepr)
knitr::opts_knit$set(root.dir = normalizePath("C:/Users/jwitt/OneDrive/Desktop/Git_projects/TriturusSim/Bottleneck TGI simulations/"))

##### Select n populations in proper habitat and at a distance of each other

source("./Bottleneck TGI simulations/createpops.R")
source("./Bottleneck TGI simulations/TGI2.R")

################################################################################
########## Initialize landscape

### Randomization control
set.seed(999)

### 50 longitude units
nx=50

### 50 latitude units
ny=50

### Create 50 by 50 grid
tempgrid<-secr::make.grid(nx=nx,ny=ny,spacing=1)

### Fill raster with random habitat with intermediate levels of fragmentation
### and with half of the matrix being usable as habitat for the simulated species 
tmp <- secr::randomHabitat(secr::as.mask(tempgrid), p = 0.5, A = 0.7)
r <- as.data.frame(tempgrid)

### Non-habitat will resist movement five times as much as habitat
r$resistance <- 5
r$resistance[as.numeric(row.names(tmp))] <- 1
r <- raster::rasterFromXYZ(r)
# Habitat is white and non-habitat is grey

########## Set up populations in the landscape

##### Simulation parameters
para<- list()
### Number of subpopulations
para$n.pops=25
### Number of individuals per population
para$n.ind=50
### Sex-ratio
para$sex.ratio <- 0.5
### Number of columns/covariates in the data.frame before the loci info
para$n.cov <- 3 
### Reproduction
para$n.offspring = 4
### Migration
para$mig.rate <- 0.1 
### Dispersal: exponential dispersal with maximal distance in map units
para$disp.max=50   #average  dispersal of an individual in meters
para$disp.rate = 0.25 #proportion of dispersing individuals
### Define markers
para$n.allels <- 10
para$n.loci <- 20
para$mut.rate <- 0.00000001
### Cost distance method
par(mar=c(1,1,1,1))
para$method <- "leastcost" #rSPDdistance, commute
para$NN <- 8  #number of neighbours for the cost distance method
### Landscape (see above for landscape creation)
landscape <- r 
### Define x and y locations (function sourced from createpops.R)
para$cells <- createpops(n=para$n.pops, minDist = 5, # here 10% of the landscape width/length
                         landscape = landscape, plot = TRUE)
# Habitat is white and non-habitat is grey, populated landscape cells are blue
# Numbers in the output refer to populated cell ID (652 means the 652th cell)
### Population locations
para$locs <- raster::xyFromCell(landscape, para$cells)
### Give the population some names 
rownames(para$locs) <- LETTERS[1:para$n.pops]
### Create a matrix of pairwise cost distances 
cost.mat <- PopGenReport::costdistances(landscape, para$locs, 
                                        para$method, para$NN)
### Create a matrix of pairwise Euclidean distances
eucl.mat <- as.matrix(dist(para$locs))  #needed for the analysis later
### Visualization of the landscape
plot(landscape)
points(para$locs[,1], para$locs[,2], pch=16, cex=2, col="orange")
text(para$locs[,1],para$locs[,2], row.names(para$locs), cex=1.5)

########## Populate the selected populations

##### Checkpoint
simpops.0 <- PopGenReport::init.popgensim(para$n.pops, para$n.ind, 
                                          para$sex.ratio, para$n.loci, 
                                          para$n.allels, para$locs, para$n.cov) 
names(simpops.0)
head(simpops.0$A[,1:6])

##### Running simulation

simpops.1to99 <- PopGenReport::run.popgensim(simpops.0, steps=99, cost.mat, 
                                       n.offspring=para$n.offspring, n.ind=para$n.ind,
                                       para$mig.rate, para$disp.max, para$disp.rate, 
                                       para$n.allels, para$mut.rate,
                                       n.cov=para$n.cov, rec="null")

# Destroy population W
simpops.event <- simpops.1to99
simpops.event$W <- simpops.event$W[sample(1:50,10),]

simpops.100 <- PopGenReport::run.popgensim(simpops.event, steps=1, cost.mat, 
                                                n.offspring=para$n.offspring, n.ind=para$n.ind,
                                                para$mig.rate, para$disp.max, para$disp.rate, 
                                                para$n.allels, para$mut.rate,
                                                n.cov=para$n.cov, rec="null")

sim_df_pre <- PopGenReport::pops2genind(simpops.1to99, para$locs, para$n.cov)
sim_df_post <- PopGenReport::pops2genind(simpops.100, para$locs, para$n.cov)

pre <- Sys.time()
results <- TGI2(sim_df_pre, sim_df_post, nperm=9999)
Sys.time()-pre
beep(7)
results

simpops.105 <- PopGenReport::run.popgensim(simpops.100, steps=5, cost.mat, 
                                           n.offspring=para$n.offspring, n.ind=para$n.ind,
                                           para$mig.rate, para$disp.max, para$disp.rate, 
                                           para$n.allels, para$mut.rate,
                                           n.cov=para$n.cov, rec="null")

sim_df_post5 <- PopGenReport::pops2genind(simpops.105, para$locs, para$n.cov)

pre <- Sys.time()
results <- TGI2(sim_df_pre, sim_df_post5, nperm=9999)
Sys.time()-pre
beep(7)
results
rep <- list(sim_df_pre, sim_df_post, sim_df_post5)
save(rep, file = paste0(here::here(),"/Bottleneck TGI simulations/simulation_outputs/","rep10",".RData"))

load("C:/Users/YNM724/Desktop/Projects/TriturusSim/Bottleneck TGI simulations/simulation_outputs/rep2.RData")
TGI2(rep[[1]],rep[[3]], nperm=9999)

           