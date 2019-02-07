# False positive and true positive rates MIGRATION EVENT
# Julian Wittische 
# February 2019

# The event affects the 201th generation.
# "t1" refers to the generation used as earliest sample in TBI()
# "t2" refers to the generation used as latest sample in TBI()

# "rep_num" refers to the number of simulation replicates to be used
rep_num <- 10 # use length(list.files(getwd())) if you want all replicates

# "earliest" refers to the number of simulation
earliest <- 190 # 150 is the lowest possible


# "com" enumerates the possible combinations
com <- permutations(n=201-earliest,r=2,v=earliest:201,repeats.allowed=F)

# "iter" refers to the number of pairs of times
iter <- 10 # use nrow(com) if you want to use or know the maximum

#### DOES IT IDENTIFY POSITIVE WHEN IT SHOULD NOT? - WITH ONE STEP CRITERION ####
#### random pairs of generations between "earliest" and 200

# permute method 1
COUNTS1 <- numeric(rep_num)
pro <- txtProgressBar()
for (i in 0:(rep_num-1)) {
  count <- 0
  for (j in 1:iter){
   rpair <- sort(sample(earliest:200, 2, replace=FALSE))
    test <- TBI(CD2TBI(i, rpair[1]), CD2TBI(i, rpair[2]), method="chord", n=1000)
    if (sum(test$p.adj < 0.025) > 0) {
      count <- count + 1
    }
  }
  COUNTS1[i+1] <- count
  setTxtProgressBar(pro,i)
}
mean(COUNTS1)/iter*100
# RESULT: 28% AT 2.5%

# permute method 2
COUNTS2 <- numeric(rep_num)
pro <- txtProgressBar()
for (i in 0:(rep_num-1)) {
  count <- 0
  for (j in 1:iter){
    rpair <- sort(sample(earliest:200, 2, replace=FALSE))
    test <- TBI(CD2TBI(i, rpair[1]), CD2TBI(i, rpair[2]), method="chord", n=1000,
                permute.sp = 2)
    if (sum(test$p.adj < 0.025) > 0) {
      count <- count + 1
    }
  }
  COUNTS2[i+1] <- count
  setTxtProgressBar(pro,i)
}
mean(COUNTS2)/iter*100
# RESULT: 10% AT 2.5%

# permute method 3
COUNTS3 <- numeric(rep_num)
pro <- txtProgressBar()
for (i in 0:(rep_num-1)) {
  count <- 0
  for (j in 1:iter){
    rpair <- sort(sample(earliest:200, 2, replace=FALSE))
    test <- TBI(CD2TBI(i, rpair[1]), CD2TBI(i, rpair[2]), method="chord", n=1000,
                permute.sp = 3)
    if (sum(test$p.adj < 0.025) > 0) {
      count <- count + 1
    }
  }
  COUNTS3[i+1] <- count
  setTxtProgressBar(pro,i)
}
mean(COUNTS3)/iter*100
# RESULT: 1% AT 2.5%

####  DOES IT IDENTIFY POSITIVE WHEN IT SHOULD? - WITH ONE STEP CRITERION ####

##### TEST 1  ####
#### first step: t1all generation between 180 and 200, t2= 201
#### second step: intersection of results with those of t1+1; t2

# permute method 1
POS1 <- vector("list", length = rep_num)
pro <- txtProgressBar(max=rep_num)
for (i in 0:(rep_num-1)) {
  pos <- vector("list", length = 21)
  for (j in (earliest-150):50){
    rpair <- c(150+j,201)
    test <- TBI(CD2TBI(i, rpair[1]), CD2TBI(i, rpair[2]), method="chord", n=1000,
                permute.sp = 1)
    pop <- which(test$p.adj < 0.025)
    pos[[j-(earliest-150)+1]] <- pop
  }
  POS1[[i+1]] <- pos
  setTxtProgressBar(pro,i+1)
}
POS1

unlist(POS1[1])
!13%in%c(unlist(POS1[1]))
c(unlist(POS1[1]))%in%!c(13)
!c(unlist(POS1[1]))%in%c(13)
sum(!c(unlist(POS1[1]))%in%c(13))
sum(!c(unlist(POS1[1]))%in%c(13))/length(c(unlist(POS1[1])))

unlist(POS1[2])

unlist(POS1[3])


# RESULT: 1% AT 0.025

# permute method 1
POS <- vector("list", length = rep_num)
pro <- txtProgressBar()
for (i in 0:(rep_num-1)) {
  pos <- vector("list", length = 21)
  for (j in 30:50){
    rpair <- c(150+j,201)
    
    test <- TBI(CD2TBI(i, rpair[1]), CD2TBI(i, rpair[2]), method="chord", n=1000,
                permute.sp = 1)
    pop <- which(test$p.adj < 0.025)
    time1bis <- rpair[1]+1
    test_plus1 <- TBI(CD2TBI(i, time1bis), CD2TBI(i, rpair[2]), method="chord", n=1000,
                      permute.sp = 1)
    pop1 <- which(test_plus1$p.adj < 0.025)
    pos[[j+1]] <- ifelse(length(intersect(pop,pop1))>0, intersect(pop,pop1), NA)
  }
  POS[[i+1]] <- pos
  setTxtProgressBar(pro,i)
}
POS

# permute method 2
POS <- vector("list", length = rep_num)
pro <- txtProgressBar()
for (i in 0:(rep_num-1)) {
  pos <- vector("list", length = 21)
  for (j in 30:50){
    rpair <- c(150+j,201)
    
    test <- TBI(CD2TBI(i, rpair[1]), CD2TBI(i, rpair[2]), method="chord", n=1000,
                permute.sp = 2)
    pop <- which(test$p.adj < 0.025)
    time1bis <- rpair[1]+1
    test_plus1 <- TBI(CD2TBI(i, time1bis), CD2TBI(i, rpair[2]), method="chord", n=1000,
                      permute.sp = 2)
    pop1 <- which(test_plus1$p.adj < 0.025)
    pos[[j+1]] <- ifelse(length(intersect(pop,pop1))>0, intersect(pop,pop1), NA)
  }
  POS[[i+1]] <- pos
  setTxtProgressBar(pro,i)
}
POS



###########################################################################################
###########################################################################################
##### DOES IT IDENTIFY POSITIVE WHEN IT SHOULD NOT? - WITH TWO STEP CRITERION: random pairs between 150 and 200
##### random pairs of generations between 150 and 200

COUNTS <- numeric(rep_num)
for (i in 0:(rep_num-1)) {
  count <- 0
  for (j in 1:iter){
    rpair <- sort(sample(150:200, 2, replace=FALSE))
    test <- TBI(CD2TBI(rep, rpair[1]), CD2TBI(rep, rpair[2]), method="chord", n=1000)
    if (sum(test$p.adj < 0.025) > 0) {
      time2bis <- rpair[2] + 1
      test_plus1 <- TBI(CD2TBI(rep, rpair[1]), CD2TBI(rep, time2bis), method="chord", n=1000)
      pop1 <- which(test_plus1$p.adj < 0.025)
      if (sum(pop==pop1)>0) {
        count <- count + 1
      }
    }
  }
  COUNTS[i+1] <- count
}
mean(COUNTS)/iter*100
# RESULT: 0% AT 0.025

############################################################################################
#####  DOES IT IDENTIFY POSITIVE WHEN IT SHOULD? - WITH TWO STEP CRITERION
##### random generation between 150 and 200, and 201

POS <- vector("list", length = rep_num)
for (i in 0:(rep_num-1)) {
  pos <- vector("list", length = iter)
  for (j in 0:50){
    rpair <- c(150+j,201)
    test <- TBI(CD2TBI(i, rpair[1]), CD2TBI(i, rpair[2]), method="chord", n=1000)
    if (sum(test$p.adj < 0.025) > 0) {
      pop <- which(test$p.adj < 0.025)
      time2bis <- rpair[2] + 1
      test_plus1 <- TBI(CD2TBI(i, rpair[1]), CD2TBI(i, time2bis), method="chord", n=1000)
      pop1 <- which(test_plus1$p.adj < 0.025)
      pos[[j]] <- ifelse(length(intersect(pop,pop1))>0, intersect(pop,pop1), NA)
    }
  }
  POS[[i+1]] <- pos
}
POS

###########################################################################################
POS <- vector("list", length = rep_num)
TIME <- vector("list", length = rep_num)
TP <- vector("list", length = rep_num)
for (i in 0:(rep_num-1)) {
  pos <- NULL
  time <- NULL
  for (j in 1:iter){
    rpair <- c(sample(150:200, 1, replace=FALSE),201)
    test <- TBI(CD2TBI(rep, rpair[1]), CD2TBI(rep, rpair[2]), method="chord", n=1000)
    if (sum(test$p.adj < 0.025) > 0) {
      pop <- which(test$p.adj < 0.025)
      time2bis <- rpair[2] + 1
      test_plus1 <- TBI(CD2TBI(rep, rpair[1]-1), CD2TBI(rep, rpair[2]), method="chord", n=1000)
      pop1 <- which(test_plus1$p.adj < 0.025)
      if (sum(pop==pop1)>0) {
        pos <- c(pos,pop)
        time <- c(time, paste0(rpair[1],rpair[2]))
      }
    }
  }
  POS[i+1] <- pop
  TIME[i+1] <- time
}
POS
TIME

POS <- vector("list", length = rep_num)
TIME <- vector("list", length = rep_num)
TP <- vector("list", length = rep_num)
for (i in 0:(rep_num-1)) {
  pos <- NULL
  time <- NULL
  for (j in 1:iter){
    rpair <- c(sample(150:200, 1, replace=FALSE),201)
    test <- TBI(CD2TBI(rep, rpair[1]), CD2TBI(rep, rpair[2]), method="chord", n=1000)
    if (sum(test$p.adj < 0.025) > 0) {
      pop <- which(test$p.adj < 0.025)
      time2bis <- rpair[2] + 1
      test_plus1 <- TBI(CD2TBI(rep, rpair[1]), CD2TBI(rep, time2bis), method="chord", n=1000)
      pop1 <- which(test_plus1$p.adj < 0.025)
      if (sum(pop%in%pop1)>0) {
        pos <- c(pos,pop)
        time <- c(time, paste0(rpair[1],rpair[2]))
      }
    }
  }
  POS[i+1] <- pop
  TIME[i+1] <- time
}
POS
TIME

###########################################################################################















POS <- vector("list", length = rep_num)
TIME <- vector("list", length = rep_num)
TP <- vector("list", length = rep_num)
for (i in 0:(rep_num-1)) {
  pos <- NULL
  time <- NULL
  for (j in 1:iter){
    rpair <- c(sample(150:200, 1, replace=FALSE), 201)
    test <- TBI(CD2TBI(rep, rpair[1]), CD2TBI(rep, rpair[2]), method="chord", n=1000)
    if (sum(test$p.adj < 0.025) > 0) {
      pop <- which(test$p.adj < 0.025)
      time2bis <- rpair[2] + 1
      test_plus1 <- TBI(CD2TBI(rep, rpair[1]), CD2TBI(rep, time2bis), method="chord", n=1000)
      pop1 <- which(test_plus1$p.adj < 0.025)
      if (sum(pop==pop1)>0) {
        pos <- c(pos,pop)
        time <- c(time, rpair[2])
      }
    }
  }
  POS[i] <- pop
  TIME[i] <- time
}
POS
TIME


POS <- vector("list", length = rep_num)
TIME <- vector("list", length = rep_num)
TP <- vector("list", length = rep_num)
for (i in 0:(rep_num-1)) {
  pos <- NULL
  time <- NULL
  for (j in 1:iter){
    rpair <- sort(sample(150:201, 2, replace=FALSE))
    test <- TBI(CD2TBI(rep, rpair[1]), CD2TBI(rep, rpair[2]), method="chord", n=1000)
    if (sum(test$p.adj < 0.025) > 0) {
      pop <- which(test$p.adj < 0.025)
      time2bis <- rpair[2] + 1
      test_plus1 <- TBI(CD2TBI(rep, rpair[1]), CD2TBI(rep, time2bis), method="chord", n=1000)
      pop1 <- which(test_plus1$p.adj < 0.025)
      if (sum(pop==pop1)>0) {
        pos <- c(pos,pop)
        time <- c(time, rpair[2])
      }
    }
  }
  POS[i] <- pop
  TIME[i] <- time
}
POS
TIME

###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################

POS <- NULL
TIME <- NULL
for (i in 1:iter){
   rpair <- sort(sample(198:201, 2, replace=FALSE))
  test <- TBI(CD2TBI(rep, rpair[1]), CD2TBI(rep, rpair[2]), method="chord", n=1000)
  test$p.adj
  
  if (sum(test$p.adj < 0.025) > 0) {
    pop <- which(test$p.adj < 0.025)
    time2bis <- rpair[2] + 1
    test_plus1 <- TBI(CD2TBI(rep, rpair[1]), CD2TBI(rep, time2bis), method="chord", n=1000)
    pop1 <- which(test_plus1$p.adj < 0.025)
    if (sum(pop==pop1)>0) {
      POS <- unique(c(POS,pop))
      TIME <- unique(c(TIME, rpair[2]))
    }
  }
}
POS
TIME

### Across reps
COUNTS <- NULL
for (i in 0:9) { #eventually 999
  count <- 0
  iter <- 500
  for (i in 1:iter){
    rpair <- sort(sample(150:201, 2, replace=FALSE))
    test <- TBI(CD2TBI(rep, rpair[1]), CD2TBI(rep, rpair[2]), method="chord", n=1000)
    if (sum(test$p.adj < 0.025) > 0) {
      pop <- which(test$p.adj < 0.025)
      time2bis <- rpair[2] + 1
      test_plus1 <- TBI(CD2TBI(rep, rpair[1]), CD2TBI(rep, time2bis), method="chord", n=1000)
      pop1 <- which(test_plus1$p.adj < 0.025)
      if (sum(pop==pop1)>0) {
        POS <- unique(c(POS,pop))
        TIME <- unique(c(TIME, rpair[2]))
      }
    }
      count <- count + 1
    }
  }
  COUNTS <- c(COUNTS, count)
} 
# 30% FALSE POSITIVE
start_time = Sys.time()
end_time = Sys.time()
end_time - start_time