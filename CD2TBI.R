# Function to create input for TBI() from CDMetaPOP outputs
# Julian Wittische
# July 2019

# NOTES: 

# Prior to sourcing this function you will need to set the working directory to the 
# scenario of interest 

# Replicate IDs start with 0
# scenario 0 is bottleneck, scenario 1 is massive migration (25 vs 26 populations)

CD2TBI <- function(replicate, timestep, scenario = 0, nloci = 100){
  # Get path for subdir associated with chosen replicate
  rep <-gtools::mixedsort(list.dirs(getwd())[replicate+2])
  #
  # Import output of the chosen timestep
  ind <- paste("ind", timestep,".csv", sep="")
  input <- read.table(paste(rep,'/',ind,sep=""), header=T, sep=",")
  
  cells <- unique(input[,1])
  all <- ncol(input)-16
  tags <- colnames(input[17:(16+all)])
  freq.file <- NULL
  freq.file <- cbind(tags, freq.file)
  
  for(sub in cells){
    row <- which(input[,1]==sub)
    nb.indiv <- length(row)
    #newpatch[sub,6]<-nb.indiv
    freqs<-NULL
    
    for(l in 0:(all-1)){
      data<-input[row,(17+l)]
      abs<-sum(data)
      nb.all<-nb.indiv*2
      relat<-abs/nb.all
      freqs<-rbind(freqs,relat)
    }
    
    freq.file<-cbind(freq.file,freqs)
  }
  
  freq.file<-t(freq.file)[,seq(1,nloci,2)]
  colnames(freq.file) <- freq.file[1,]
  freq.file <- freq.file[-1,]
  rownames(freq.file) <- 1:(25+scenario)
  freq.file <- apply(freq.file, 2, as.numeric)
  # Get rid of population 26 (isolated)
  freq.file <- freq.file[-nrow(freq.file),]
  invisible(freq.file)
}
