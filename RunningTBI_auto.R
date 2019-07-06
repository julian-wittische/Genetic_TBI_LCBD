# Running TBI - automatic
# Julian Wittische 
# July 2019

# The event affects the 201th generation.
# "t1" refers to the generation used as earliest sample in TBI()
# "t2" refers to the generation used as latest sample in TBI()

############################################################################################################

input_df <- data.frame(alpha = numeric(0), permute = numeric(0), yearsback = numeric(0))

# Type 1 error rate
alpha <- 0.025

# Number of years to go back
yearsback <- 5
POS <- vector("list", length = yearsback)

# "rep_num" refers to the number of simulation replicates to be used
rep_num <- 130 # use length(list.files(getwd())) if you want all replicates

# "earliest" refers to the number of simulation
earliest <- 200 # 150 is the lowest possible
time <- 201-earliest
print(paste("Testing detection of the event happening", time, "year(s)/generation(s) later"))

####  TPR: DOES IT IDENTIFY POSITIVE WHEN IT SHOULD? - WITH ONE STEP CRITERION ####

# permute method 1
pro <- txtProgressBar(max=rep_num, style=3)
pos <- vector("list", length = rep_num)
for (i in 0:(rep_num-1)) {
  test <- TBI(CD2TBI(i, earliest), CD2TBI(i, 201), method="chord", n=1000, permute.sp = 2)
  pop <- which(test$p.adj < 0.025)
  pos[[i+1]] <- pop
  setTxtProgressBar(pro,i+1)
}