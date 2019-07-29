# False positive, true positive rates, etc
# Julian Wittische 
# July 2019

TP <- function(pos_object, targets) sapply(pos_object, function(x) sum(c(unlist(x))%in%targets)) 

FP <- function(pos_object, targets) sapply(pos_object, function(x) sum(c(unlist(x))%!in%targets)) 

FN <- function(pos_object, targets) sapply(pos_object, function(x) sum(targets%!in%c(unlist(x)))) 

TN <- function(pos_object, targets) 25 - length(targets) - FP(pos_object, targets)
#rep(25, length(pos_object))  - rep(length(targets), length(pos_object)) - FP(pos_object)
TP_FP_check <- function(pos_object, targets){
  sapply(pos_object, length) - FP(pos_object, targets) - TP(pos_object, targets)
}

confusion_mat <- function(pos_object, targets) {
  data.frame(TP= TP(pos_object, targets),
             FP= FP(pos_object, targets),
             FN= FN(pos_object, targets),
             TN= TN(pos_object, targets),
             total= TP(pos_object, targets) + FP(pos_object, targets) + FN(pos_object, targets)
             + TN(pos_object, targets),
             TPR= TP(pos_object, targets) / (TP(pos_object, targets) + FN(pos_object, targets)),
             FNR= 1 - (TP(pos_object, targets) / (TP(pos_object, targets) + FN(pos_object, targets))),
             FPR= FP(pos_object, targets) / (FP(pos_object, targets) + TN(pos_object, targets)),
             TNR= 1 - (FP(pos_object, targets) / (FP(pos_object, targets) + TN(pos_object, targets))),
             ACC= (TP(pos_object, targets) + TN(pos_object, targets)) / 
               (TP(pos_object, targets) + FP(pos_object, targets) + FN(pos_object, targets) + TN(pos_object, targets)),
             PPV= TP(pos_object, targets)/(TP(pos_object, targets)+FP(pos_object, targets)),
             FDR= FP(pos_object, targets)/(TP(pos_object, targets)+FP(pos_object, targets)),
             FOR= FN(pos_object, targets)/(FN(pos_object, targets)+TN(pos_object, targets)),
             NPV= TN(pos_object, targets)/(FN(pos_object, targets)+TN(pos_object, targets)))
}

FP_control <- function(pos_object) sapply(pos_object, function(x) sum(c(unlist(x))%!in%NULL))
TN_control <- function(pos_object) 25 - FP_control(pos_object)
FPR_control <- function(pos_object) FP_control(pos_object) / (FP_control(pos_object) + TN_control(pos_object))