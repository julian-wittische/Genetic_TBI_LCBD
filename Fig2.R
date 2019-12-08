setwd("E:/Globus/RData2")

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

sum_perf <- function(laodperfoutput, FPRorFNR="FPR", controlTRUEFALSE=FALSE){
  if(!controlTRUEFALSE){
    sapply(laodperfoutput, function(x) mean(x[,FPRorFNR]))
  } else {
    sapply(laodperfoutput, mean)
  }
}

IL1 <- load_RData("I_L1_m.RData")
IL2 <- load_RData("I_L2_m.RData")
IL3 <- load_RData("I_L3_m.RData")
IM1 <- load_RData("I_M1_m.RData")
IM2 <- load_RData("I_M2_m.RData")
IM3 <- load_RData("I_M3_m.RData")
IH1 <- load_RData("I_H1_m.RData")
IH2 <- load_RData("I_H2_m.RData")
IH3 <- load_RData("I_H3_m.RData")

CL <- load_RData("C_L_m.RData")
CM <- load_RData("C_M_m.RData")
CH <- load_RData("C_H_m.RData")

alpha <- c(0.001, 0.0025, 0.005, 0.0075, 0.01, 0.025, 0.05, 0.075, 0.1)