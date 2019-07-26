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

TBI_test_auto <- function(alpha = 0.05, earliest = 100, latest = 101,
                       rep = length(list.files(path))-1, path, scenario){
  setwd(path)

pro <- txtProgressBar(max=rep, style=3)

pos <- vector("list", length = rep)
  for (i in 0:(rep-1)) {
    test <- TBI(CD2TBI(i, earliest, scenario = scenario), CD2TBI(i, latest, scenario = scenario), method="chord", n=1000 )
    pop <- which(test$p.TBI < alpha)
    pos[[i+1]] <- pop
    
    setTxtProgressBar(pro,i+1)
  }

return(pos)
}
############################################################################################################