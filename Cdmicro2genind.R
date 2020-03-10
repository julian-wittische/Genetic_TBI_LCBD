# Function to create genind (adegenet) objects from CDMetaPOP outputs
# Julian Wittische
# October 2019

# NOTES: 

# Prior to sourcing this function you will need to set the working directory to the 
# scenario of interest 

# Replicate IDs start with 0
# scenario 0 is bottleneck, scenario 1 is massive migration (25 vs 26 populations)

CDmicro2genind <- function(replicate, timestep, scenario = 1, nloci = 10, nalleles = 10){

  # Get path for subdir associated with chosen replicate
  repl <<-gtools::mixedsort(list.dirs(getwd()))[replicate+2] #+2 bc 1 summary file
  #
  # Import output of the chosen timestep
  ind <- paste("ind", timestep,".csv", sep="")
  input <<- read.table(paste(repl,'/',ind,sep=""), header=T, sep=",")
  
  cdgeno <- input[18:ncol(input)]
  genprep <- data.frame(mat.or.vec(nr = nrow(input), nc = nloci)) 
  fakemic <- paste(0:9)
  
    for (ind in 1: nrow(cdgeno)){
    
      for (loci in 1:nloci){
      
        if (any(cdgeno[ind,((loci-1)*nalleles+1):((loci-1)*nalleles+nalleles)] == 2)){
          genprep[ind,loci] <- paste(LETTERS[which(cdgeno[ind, ((loci-1)*nalleles+1):((loci-1)*nalleles+nalleles)] == 2)],
                                  "/", LETTERS[which(cdgeno[ind, ((loci-1)*nalleles+1):((loci-1)*nalleles+nalleles)] == 2)], sep="")
          
        } else {
          genprep[ind,loci] <- paste(LETTERS[which(cdgeno[ind, ((loci-1)*nalleles+1):((loci-1)*nalleles+nalleles)] == 1)[1]],
                                  "/", LETTERS[which(cdgeno[ind, ((loci-1)*nalleles+1):((loci-1)*nalleles+nalleles)] == 1)[2]], sep="")
        }
      }
    }
  colnames(genprep) <- paste("L", 0:9, sep="")
  
  genloc <- as.loci(genprep)
  gen <- loci2genind(genloc)
  pop(gen) <- as.factor(input[,17])
  # genp <- genind2genpop(gen)
  # genp <- genp[,order(colnames(genp@tab))]
  # return(genp)
  if (scenario==1) {
    gen <- gen[gen@pop!="26"]]
  } 
  return(gen) #MODIF
}