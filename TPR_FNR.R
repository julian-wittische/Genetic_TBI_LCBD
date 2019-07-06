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

# CHECK STEP: should be all zeros
TP_FP_check(pos)

###########################################################################################################

mean(confusion_mat(POS[[1]])$FPR, na.rm = TRUE)
mean(confusion_mat(POS[[1]])$FNR, na.rm = TRUE)
sd(confusion_mat(POS[[1]])$FPR, na.rm = TRUE)
sd(confusion_mat(POS[[1]])$FNR, na.rm = TRUE)

mean(confusion_mat(POS[[2]])$FPR, na.rm = TRUE)
mean(confusion_mat(POS[[2]])$FNR, na.rm = TRUE)
sd(confusion_mat(POS[[2]])$FPR, na.rm = TRUE)
sd(confusion_mat(POS[[2]])$FNR, na.rm = TRUE)

mean(confusion_mat(POS[[3]])$FPR, na.rm = TRUE)
mean(confusion_mat(POS[[3]])$FNR, na.rm = TRUE)
sd(confusion_mat(POS[[3]])$FPR, na.rm = TRUE)
sd(confusion_mat(POS[[3]])$FNR, na.rm = TRUE)

mean(confusion_mat(POS[[4]])$FPR, na.rm = TRUE)
mean(confusion_mat(POS[[4]])$FNR, na.rm = TRUE)
sd(confusion_mat(POS[[4]])$FPR, na.rm = TRUE)
sd(confusion_mat(POS[[4]])$FNR, na.rm = TRUE)

mean(confusion_mat(POS[[5]])$FPR, na.rm = TRUE)
mean(confusion_mat(POS[[5]])$FNR, na.rm = TRUE)
sd(confusion_mat(POS[[5]])$FPR, na.rm = TRUE)
sd(confusion_mat(POS[[5]])$FNR, na.rm = TRUE)

mean(confusion_mat(POS[[6]])$FPR, na.rm = TRUE)
mean(confusion_mat(POS[[6]])$FNR, na.rm = TRUE)
sd(confusion_mat(POS[[6]])$FPR, na.rm = TRUE)
sd(confusion_mat(POS[[6]])$FNR, na.rm = TRUE)

mean(confusion_mat(POS[[7]])$FPR, na.rm = TRUE)
mean(confusion_mat(POS[[7]])$FNR, na.rm = TRUE)
sd(confusion_mat(POS[[7]])$FPR, na.rm = TRUE)
sd(confusion_mat(POS[[7]])$FNR, na.rm = TRUE)

mean(confusion_mat(POS[[8]])$FPR, na.rm = TRUE)
mean(confusion_mat(POS[[8]])$FNR, na.rm = TRUE)
sd(confusion_mat(POS[[8]])$FPR, na.rm = TRUE)
sd(confusion_mat(POS[[8]])$FNR, na.rm = TRUE)

mean(confusion_mat(POS[[9]])$FPR, na.rm = TRUE)
mean(confusion_mat(POS[[9]])$FNR, na.rm = TRUE)
sd(confusion_mat(POS[[9]])$FPR, na.rm = TRUE)
sd(confusion_mat(POS[[9]])$FNR, na.rm = TRUE)

mean(confusion_mat(POS[[10]])$FPR, na.rm = TRUE)
sd(confusion_mat(POS[[10]])$FPR, na.rm = TRUE)
mean(confusion_mat(POS[[10]])$FNR, na.rm = TRUE)
sd(confusion_mat(POS[[10]])$FNR, na.rm = TRUE)

mean(confusion_mat(POS[[11]])$FPR, na.rm = TRUE)
sd(confusion_mat(POS[[11]])$FPR, na.rm = TRUE)
mean(confusion_mat(POS[[11]])$FNR, na.rm = TRUE)
sd(confusion_mat(POS[[11]])$FNR, na.rm = TRUE)

mean(confusion_mat(POS[[12]])$FPR, na.rm = TRUE)
sd(confusion_mat(POS[[12]])$FPR, na.rm = TRUE)
mean(confusion_mat(POS[[12]])$FNR, na.rm = TRUE)
sd(confusion_mat(POS[[12]])$FNR, na.rm = TRUE)

mean(confusion_mat(POS[[13]])$FPR, na.rm = TRUE)
sd(confusion_mat(POS[[13]])$FPR, na.rm = TRUE)
mean(confusion_mat(POS[[13]])$FNR, na.rm = TRUE)
sd(confusion_mat(POS[[13]])$FNR, na.rm = TRUE)

mean(confusion_mat(POS[[15]])$FPR, na.rm = TRUE)
sd(confusion_mat(POS[[15]])$FPR, na.rm = TRUE)
mean(confusion_mat(POS[[15]])$FNR, na.rm = TRUE)
sd(confusion_mat(POS[[15]])$FNR, na.rm = TRUE)

mean(confusion_mat(POS[[16]])$FPR, na.rm = TRUE)
sd(confusion_mat(POS[[16]])$FPR, na.rm = TRUE)
mean(confusion_mat(POS[[16]])$FNR, na.rm = TRUE)
sd(confusion_mat(POS[[16]])$FNR, na.rm = TRUE)

mean(confusion_mat(POS[[17]])$FPR, na.rm = TRUE)
sd(confusion_mat(POS[[17]])$FPR, na.rm = TRUE)
mean(confusion_mat(POS[[17]])$FNR, na.rm = TRUE)
sd(confusion_mat(POS[[17]])$FNR, na.rm = TRUE)

mean(confusion_mat(POS[[18]])$FPR, na.rm = TRUE)
sd(confusion_mat(POS[[18]])$FPR, na.rm = TRUE)
mean(confusion_mat(POS[[18]])$FNR, na.rm = TRUE)
sd(confusion_mat(POS[[18]])$FNR, na.rm = TRUE)

mean(confusion_mat(POS[[19]])$FPR, na.rm = TRUE)
sd(confusion_mat(POS[[19]])$FPR, na.rm = TRUE)
mean(confusion_mat(POS[[19]])$FNR, na.rm = TRUE)
sd(confusion_mat(POS[[19]])$FNR, na.rm = TRUE)

mean(confusion_mat(POS[[9]])$FDR, na.rm = TRUE)
sd(confusion_mat(POS[[9]])$FDR, na.rm = TRUE)

mean(confusion_mat(POS[[5]])$FPR, na.rm = TRUE)
sd(confusion_mat(POS[[5]])$FPR, na.rm = TRUE)
###########################################################################################################
POS[[15]] <- pos
