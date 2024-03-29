# False positive and true positive rates MIGRATION EVENT
# Julian Wittische 
# October 2018

# The event affects the 201th generation.
# t1 refers to the generation used as earliest sample in TBI()
# t2 refers to the generation used as latest sample in TBI()

rep_num <- 3 #length(list.files(getwd()))
iter <- 50

###########################################################################################
###########################################################################################
##### DOES IT IDENTIFY POSITIVE WHEN IT SHOULD NOT? - WITH ONE STEP CRITERION
##### random pairs of generations between 150 and 200

# permute method 1
COUNTS1 <- numeric(rep_num)
pro <- txtProgressBar()
for (i in 0:(rep_num-1)) {
  count <- 0
  for (j in 1:iter){
   rpair <- sort(sample(150:200, 2, replace=FALSE))
    test <- TBI(CD2TBI(i, rpair[1]), CD2TBI(i, rpair[2]), method="chord", n=1000)
    if (sum(test$p.adj < 0.025) > 0) {
      count <- count + 1
    }
  }
  COUNTS1[i+1] <- count
  setTxtProgressBar(pro,i)
}
mean(COUNTS1)/iter*100
# RESULT: 30% AT 0.025, ABOUT 10 TIMES THE ACCEPTABLE RATE?

# permute method 2
COUNTS2 <- numeric(rep_num)
pro <- txtProgressBar()
for (i in 0:(rep_num-1)) {
  count <- 0
  for (j in 1:iter){
    rpair <- sort(sample(150:200, 2, replace=FALSE))
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
# RESULT: 17% AT 0.025, ABOUT 10 TIMES THE ACCEPTABLE RATE?

# permute method 3
COUNTS3 <- numeric(rep_num)
pro <- txtProgressBar()
for (i in 0:(rep_num-1)) {
  count <- 0
  for (j in 1:iter){
    rpair <- sort(sample(150:200, 2, replace=FALSE))
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
# RESULT: 1% AT 0.025, ABOUT 10 TIMES THE ACCEPTABLE RATE?

###########################################################################################
###########################################################################################
#####  DOES IT IDENTIFY POSITIVE WHEN IT SHOULD? - WITH TWO STEP CRITERION

##### TEST 1
##### first step: random t1 generation between 180 and 200, t2= 201
##### second step: intersection of results with those of t1+1; t2

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
for (i in 0:(rep_num-1)) {
  
  pos <- vector("list", length = 51)
  for (j in 0:50){
    rpair <- c(150+j,201)
    
    test <- TBI(CD2TBI(i, rpair[1]), CD2TBI(i, rpair[2]), method="chord", n=1000,
                permute.sp = 2)
    pop <- which(test$p.adj < 0.025)
    time2bis <- rpair[2] + 1
    test_plus1 <- TBI(CD2TBI(i, rpair[1]), CD2TBI(i, time2bis), method="chord", n=1000,
                      permute.sp = 2)
    pop1 <- which(test_plus1$p.adj < 0.025)
    pos[[j+1]] <- ifelse(length(intersect(pop,pop1))>0, intersect(pop,pop1), NA)
  }
  
  POS[[i+1]] <- pos
}
POS

# permute method 3
POS <- vector("list", length = rep_num)
for (i in 0:(rep_num-1)) {
  
  pos <- vector("list", length = 51)
  for (j in 0:50){
    rpair <- c(150+j,201)
    
    test <- TBI(CD2TBI(i, rpair[1]), CD2TBI(i, rpair[2]), method="chord", n=1000,
                permute.sp = 3)
    pop <- which(test$p.adj < 0.025)
    time2bis <- rpair[2] + 1
    test_plus1 <- TBI(CD2TBI(i, rpair[1]), CD2TBI(i, time2bis), method="chord", n=1000,
                      permute.sp = 3)
    pop1 <- which(test_plus1$p.adj < 0.025)
    pos[[j+1]] <- ifelse(length(intersect(pop,pop1))>0, intersect(pop,pop1), NA)
  }
  
  POS[[i+1]] <- pos
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