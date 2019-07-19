# False positive and true positive rates
# Julian Wittische 
# July 2019

'%!in%' <- function(x,y)!('%in%'(x,y))

TP <- function(pos_object) sapply(pos_object, function(x) sum(c(unlist(x))%in%c(13))) 

FP <- function(pos_object) sapply(pos_object, function(x) sum(c(unlist(x))%!in%c(13))) 

FN <- function(pos_object) sapply(pos_object, function(x) sum(c(13)%!in%c(unlist(x)))) 

TN <- function(pos_object) 24 - FP(pos_object)

TP_FP_check <- function(pos_object) sapply(pos_object, length) - FP(pos_object) - TP(pos_object)

confusion_mat <- function(pos_object) {
  data.frame(TP=TP(pos_object), FP=FP(pos_object), FN=FN(pos_object), TN=TN(pos_object),
                            total=TP(pos_object)+FP(pos_object)+FN(pos_object)+TN(pos_object),
                            TPR= TP(pos_object) / (TP(pos_object) + FN(pos_object)),
                            FNR= 1 - (TP(pos_object) / (TP(pos_object) + FN(pos_object))),
                            FPR= FP(pos_object) / (FP(pos_object) + TN(pos_object)),
                            TNR= 1 - (FP(pos_object) / (FP(pos_object) + TN(pos_object))),
                            ACC= (TP(pos_object)+TN(pos_object))/(TP(pos_object)+FP(pos_object)+FN(pos_object)+TN(pos_object)),
                            PPV= TP(pos_object)/(TP(pos_object)+FP(pos_object)),
                            FDR= FP(pos_object)/(TP(pos_object)+FP(pos_object)),
                            FOR= FN(pos_object)/(FN(pos_object)+TN(pos_object)),
                            NPV= TN(pos_object)/(FN(pos_object)+TN(pos_object)))
}