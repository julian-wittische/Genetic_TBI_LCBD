# Running TBI - automatic
# Julian Wittische 
# October 2019

# The event affects the 101th generation.
# "alpha" refers to the p.value threshold
# "permute" refers to the permutation approach chosen (see ?TBI and Legendre 2019 for details)
# "earliest" refers to the number of simulation replicates to be used.
# "latest" refers to the number of simulation replicates to be used.
# "rep" refers to the number of simulation replicates to be used (between 1 and 180).

############################################################################################################

TGI_auto_allp <- function(earliest = 100, latest = 101,
                                             rep = length(list.files(path))-1, path, scenario,
                                             alpha = c(0.0001, 0.00025, 0.0005, 0.00075,
                                                       0.001, 0.0025, 0.005, 0.0075,
                                                       0.01, 0.025, 0.05, 0.075,
                                                       0.1),
                                             nloci = 100,
                                             nalleles = 2,
                                             nperm = 999){
  setwd(path)
  
  pro <- txtProgressBar(max = rep, style = 3)
  
  POS <- lapply(vector("list", length = length(alpha)), function(x) x <- vector("list", length = rep))
  POS2 <- lapply(vector("list", length = length(alpha)), function(x) x <- vector("list", length = rep))
  
  for (i in 0:(rep-1)) {
    test <- TGI2(CDmicro2genind(i, earliest, scenario = scenario, nloci = nloci, nalleles = nalleles),
                          CDmicro2genind(i, latest, scenario = scenario, nloci = nloci, nalleles = nalleles),
                          method = 4,
                          nperm = nperm)
    print(paste("Replicate: ", i, sep=""))
    
    for (j in 1:length(alpha)) {
      pop <- which(test$p.TBI < alpha[j]) #NEXT: change TBI for adj
      POS[[j]][[i+1]] <- pop
      pop2 <- which(test$p.adj < alpha[j])
      POS[[j]][[i+1]] <- pop2
      #print(paste("Which alpha ", alpha[j], sep=""))
    }
    
    setTxtProgressBar(pro, i+1)
    
  }
  
  return(list(POS, POS2))
  
}
############################################################################################################