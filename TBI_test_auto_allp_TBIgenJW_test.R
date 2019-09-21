# Running TBI - automatic
# Julian Wittische 
# July 2019

# The event affects the 101th generation.
# "alpha" refers to the p.value threshold
# "permute" refers to the permutation approach chosen (see ?TBI and Legendre 2019 for details)
# "earliest" refers to the number of simulation replicates to be used.
# "latest" refers to the number of simulation replicates to be used.
# "rep" refers to the number of simulation replicates to be used (between 1 and 180).

############################################################################################################

TBI_test_auto_allp_TBIgenJW_test <- function(earliest = 100, latest = 101,
                               rep = length(list.files(path))-1, path, scenario,
                               alpha = c(0.0001, 0.00025, 0.0005, 0.00075,
                                         0.001, 0.0025, 0.005, 0.0075,
                                         0.01, 0.025, 0.05, 0.075,
                                         0.1)){
  setwd(path)
  
  pro <- txtProgressBar(max = rep, style = 3)
  
  POS <- lapply(vector("list", length = length(alpha)), function(x) x <- vector("list", length = rep))
  
  for (i in 0:(rep-1)) {
    test <- TBIgenJW_test(CD2TBI(i, earliest, scenario = scenario),
                CD2TBI(i, latest, scenario = scenario), method = "chord", n = 1000 )
    print(paste("Replicate: ", i, sep=""))
    
    for (j in 1:length(alpha)) {
      pop <- which(test$p.TBI < alpha[j])
      POS[[j]][[i+1]] <- pop
      #print(paste("Which alpha ", alpha[j], sep=""))
    }
    
    setTxtProgressBar(pro, i+1)
    
  }
  
  return(POS)
  
}
############################################################################################################