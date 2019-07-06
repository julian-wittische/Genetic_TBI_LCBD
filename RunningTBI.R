# Running TBI
# Julian Wittische 
# July 2019

# The event affects the 201th generation.
# "t1" refers to the generation used as earliest sample in TBI()
# "t2" refers to the generation used as latest sample in TBI()

############################################################################################################
# Type 1 error rate
alpha <- 0.025

# Number of years to go back
yearsback <- 5
POS <- vector("list", length = yearsback)

# "rep_num" refers to the number of simulation replicates to be used
rep_num <- 100 # use length(list.files(getwd())) if you want all replicates # NOT THE MAX

# "earliest" refers to the number of simulation
earliest <- 200 # 150 is the lowest possible
time <- 201-earliest
print(paste("Testing detection of the event happening", time, "year(s)/generation(s) later"))

####  TPR: DOES IT IDENTIFY POSITIVE WHEN IT SHOULD? - WITH ONE STEP CRITERION ####

pro <- txtProgressBar(max=rep_num, style=3)
pos <- vector("list", length = rep_num)
for (i in 0:(rep_num-1)){
  test <- TBI(CD2TBI(i, earliest), CD2TBI(i, 204), method="chord", n=1000, permute.sp = 1)
  pop <- which(test$p.adj < 0.025)
  pos[[i+1]] <- pop
  setTxtProgressBar(pro,i+1)
}