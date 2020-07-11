# Functions to extract the performances from the outcomes of analyses
# Author: Julian Wittische 
# December 2019

load_RData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}

load_perf <- function(scen18, popnum){
  if(popnum == 1){
    return(mapply(rbind,
                  lapply(scen18, function(x) confusion_mat(x[1:30], 1)),
                  lapply(scen18, function(x) confusion_mat(x[31:60], 2)),
                  lapply(scen18, function(x) confusion_mat(x[61:90], 3)),
                  lapply(scen18, function(x) confusion_mat(x[91:120], 7)),
                  lapply(scen18, function(x) confusion_mat(x[121:150], 8)),
                  lapply(scen18, function(x) confusion_mat(x[151:180], 13)),
                  SIMPLIFY=FALSE))
  }
  if(popnum == 2){
    return(mapply(rbind,
                  lapply(scen18, function(x) confusion_mat(x[1:30],  c(2, 8))),
                  lapply(scen18, function(x) confusion_mat(x[31:60], c(3, 8))),
                  lapply(scen18, function(x) confusion_mat(x[61:90], c(3, 13))),
                  lapply(scen18, function(x) confusion_mat(x[91:120], c(7, 8))),
                  lapply(scen18, function(x) confusion_mat(x[121:150], c(7, 13))),
                  lapply(scen18, function(x) confusion_mat(x[151:180], c(8, 13))),
                  SIMPLIFY=FALSE))
  }
  if(popnum == 3){
    return(mapply(rbind,
                  lapply(scen18, function(x) confusion_mat(x[1:30],  c(2, 3, 8))),
                  lapply(scen18, function(x) confusion_mat(x[31:60], c(2, 7, 13))),
                  lapply(scen18, function(x) confusion_mat(x[61:90], c(2, 8, 13))),
                  lapply(scen18, function(x) confusion_mat(x[91:120], c(3, 7, 8))),
                  lapply(scen18, function(x) confusion_mat(x[121:150], c(3, 7, 13))),
                  lapply(scen18, function(x) confusion_mat(x[151:180], c(7, 8, 13))),
                  SIMPLIFY=FALSE))
  }
  if(popnum == "control"){
    return(lapply(scen18, function(x)FPR_control(x)))
  }
}

sum_perf <- function(laodperfoutput, FPRorFNR="FPR", controlTRUEFALSE=FALSE){ #j'ai changé mean vers CI et t()
  if(!controlTRUEFALSE){
    t(sapply(laodperfoutput, function(x) CI(x[,FPRorFNR])))
  } else {
    t(sapply(laodperfoutput, CI))
  }
}

CI_TBI <- function(x,threshold_number, FPR_or_FNR){
  sapply(x, function(x) CI(x[,paste(FPR_or_FNR)]))[,threshold_number]
}
# 
# load_perf2 <- function(scen18, popnum, adj=FALSE){
#   scen18 <- ifelse(adj, scen18[[2]], scen18[[1]])
#   print(str(scen18))
#   if(popnum == 1){
#     return(mapply(rbind,
#                   lapply(scen18, function(x) confusion_mat(x[1:30], 1)),
#                   lapply(scen18, function(x) confusion_mat(x[31:60], 2)),
#                   lapply(scen18, function(x) confusion_mat(x[61:90], 3)),
#                   lapply(scen18, function(x) confusion_mat(x[91:120], 7)),
#                   lapply(scen18, function(x) confusion_mat(x[121:150], 8)),
#                   lapply(scen18, function(x) confusion_mat(x[151:180], 13)),
#                   SIMPLIFY=FALSE))
#   }
#   if(popnum == 2){
#     return(mapply(rbind,
#                   lapply(scen18, function(x) confusion_mat(x[1:30],  c(2, 8))),
#                   lapply(scen18, function(x) confusion_mat(x[31:60], c(3, 8))),
#                   lapply(scen18, function(x) confusion_mat(x[61:90], c(3, 13))),
#                   lapply(scen18, function(x) confusion_mat(x[91:120], c(7, 8))),
#                   lapply(scen18, function(x) confusion_mat(x[121:150], c(7, 13))),
#                   lapply(scen18, function(x) confusion_mat(x[151:180], c(8, 13))),
#                   SIMPLIFY=FALSE))
#   }
#   if(popnum == 3){
#     return(mapply(rbind,
#                   lapply(scen18, function(x) confusion_mat(x[1:30],  c(2, 3, 8))),
#                   lapply(scen18, function(x) confusion_mat(x[31:60], c(2, 7, 13))),
#                   lapply(scen18, function(x) confusion_mat(x[61:90], c(2, 8, 13))),
#                   lapply(scen18, function(x) confusion_mat(x[91:120], c(3, 7, 8))),
#                   lapply(scen18, function(x) confusion_mat(x[121:150], c(3, 7, 13))),
#                   lapply(scen18, function(x) confusion_mat(x[151:180], c(7, 8, 13))),
#                   SIMPLIFY=FALSE))
#   }
#   if(popnum == "control"){
#     return(lapply(scen18, function(x)FPR_control(x)))
#   }
# }
# 
# sum_perf <- function(laodperfoutput, FPRorFNR="FPR", controlTRUEFALSE=FALSE){ #j'ai changé mean vers CI et t()
#   if(!controlTRUEFALSE){
#     t(sapply(laodperfoutput, function(x) CI(x[,FPRorFNR])))
#   } else {
#     t(sapply(laodperfoutput, CI))
#   }
# }
#
# CI_TBI <- function(x,threshold_number, FPR_or_FNR){
#   sapply(x, function(x) CI(x[,paste(FPR_or_FNR)]))[,threshold_number]
# }

