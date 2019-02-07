# Function to create input for TBI() from CDMetaPOP outputs
# Julian Wittische
# October 2018

# NOTES: 

# Prior to sourcing this function you will need to set the working directory to the 
# scenario of interest 

# Replicate IDs start with 0

CD2GSTUDIO <- function(replicate, timestep){
  # Get path for subdir associated with chosen replicate
  rep <-gtools::mixedsort(list.dirs(getwd())[replicate+2])
  # Import output of the chosen timestep
  ind <- paste("ind", timestep,".csv", sep="")
  data <- read_population(paste(rep,'/',ind,sep=""), 
                          locus.columns = seq(18, 216, 2), type = "snp", header=T, sep=",")
  invisible(data)
}

