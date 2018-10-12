# Function to create input for TBI() from CDMetaPOP outputs
# Julian Wittische
# October 2018

# NOTES: 

# Prior to sourcing this function you will need to set the working directory to the 
# scenario of interest 

# Replicate IDs start with 0

# This was made for 100 loci, be sure to change line 44

CD2TBI <- function(replicate, timestep){
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
  
  freq.file<-t(freq.file)[,seq(1,200,2)]
  colnames(freq.file) <- freq.file[1,]
  freq.file <- freq.file[-1,]
  rownames(freq.file) <- 1:26
  freq.file <- apply(freq.file, 2, as.numeric)
  # Get rid of population 26 (isolated)
  freq.file <- freq.file[-nrow(freq.file),]
  invisible(freq.file)
}
