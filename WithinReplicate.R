# Test within replicate
# Julian Wittische 
# October 2018

# Pick one replicate (temp)
rep <- 0

# 
test <- TBI(CD2TBI(rep, 150),CD2TBI(rep, 151), n=1000)
test$p.TBI
test$p.adj

test <- TBI(CD2TBI(rep, 198),CD2TBI(rep, 200), n=1000)
test$p.TBI
test$p.adj

# Random pairs between 150 and 200

count <- 0
iter <- 500
for (i in 1:iter){
  rpair <- sample(150:200, 2, replace=FALSE)
    test <- TBI(CD2TBI(rep, rpair[1]),CD2TBI(rep, rpair[2]), n=1000)
  if (sum(test$p.adj<0.05)>0) {
    count <- count + 1
  }
}
count


