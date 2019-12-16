setwd("E:/Globus/RData2")
setwd("C:/Users/Field/Documents/Glob")

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

library(Rmisc)

CI_TBI <- function(x,threshold_number, FPR_or_FNR){
  sapply(x, function(x) CI(x[,paste(FPR_or_FNR)]))[,threshold_number]
}

alpha <- c(0.001, 0.0025, 0.005, 0.0075, 0.01, 0.025, 0.05, 0.075, 0.1)

IL1 <- load_perf(load_RData("I_L1_m.RData"), 1)
IL1b1 <- load_perf(load_RData("I_L1_m1.RData"), 1)
IL1b2 <- load_perf(load_RData("I_L1_m2.RData"), 1)
IL1b3 <- load_perf(load_RData("I_L1_m3.RData"), 1)
IL1b4 <- load_perf(load_RData("I_L1_m4.RData"), 1)
IL1b5 <- load_perf(load_RData("I_L1_m5.RData"), 1)
IL1b6 <- load_perf(load_RData("I_L1_m6.RData"), 1)
IL1b7 <- load_perf(load_RData("I_L1_m7.RData"), 1)
IL1b8 <- load_perf(load_RData("I_L1_m8.RData"), 1)
IL1b9 <- load_perf(load_RData("I_L1_m9.RData"), 1)
IL1a1 <- load_perf(load_RData("I_L1_mp1.RData"), 1)
IL1a2 <- load_perf(load_RData("I_L1_mp2.RData"), 1)
IL1a3 <- load_perf(load_RData("I_L1_mp3.RData"), 1)
IL1a4 <- load_perf(load_RData("I_L1_mp4.RData"), 1)
IL1a5 <- load_perf(load_RData("I_L1_mp5.RData"), 1)
IL1a6 <- load_perf(load_RData("I_L1_mp6.RData"), 1)
IL1a7 <- load_perf(load_RData("I_L1_mp7.RData"), 1)
IL1a8 <- load_perf(load_RData("I_L1_mp8.RData"), 1)
IL1a9 <- load_perf(load_RData("I_L1_mp9.RData"), 1)

before_FPR_L1 <- rbind(CI_TBI(IL1, 7, "FPR"),
                       CI_TBI(IL1b1, 2, "FPR"),   
                       CI_TBI(IL1b2, 2, "FPR"),
                       CI_TBI(IL1b3, 2, "FPR"),
                       CI_TBI(IL1b4, 2, "FPR"),
                       CI_TBI(IL1b5, 2, "FPR"),
                       CI_TBI(IL1b6, 2, "FPR"),
                       CI_TBI(IL1b7, 2, "FPR"),
                       CI_TBI(IL1b8, 2, "FPR"),
                       CI_TBI(IL1b9, 2, "FPR"))

before_FNR_L1 <- rbind(CI_TBI(IL1, 7, "FNR"),
                       CI_TBI(IL1b1, 2, "FNR"),   
                       CI_TBI(IL1b2, 2, "FNR"),
                       CI_TBI(IL1b3, 2, "FNR"),
                       CI_TBI(IL1b4, 2, "FNR"),
                       CI_TBI(IL1b5, 2, "FNR"),
                       CI_TBI(IL1b6, 2, "FNR"),
                       CI_TBI(IL1b7, 2, "FNR"),
                       CI_TBI(IL1b8, 2, "FNR"),
                       CI_TBI(IL1b9, 2, "FNR"))
###
IL2 <- load_perf(load_RData("I_L2_m.RData"), 2)
IL2b1 <- load_perf(load_RData("I_L2_m1.RData"), 2)
IL2b2 <- load_perf(load_RData("I_L2_m2.RData"), 2)
IL2b3 <- load_perf(load_RData("I_L2_m3.RData"), 2)
IL2b4 <- load_perf(load_RData("I_L2_m4.RData"), 2)
IL2b5 <- load_perf(load_RData("I_L2_m5.RData"), 2)
IL2b6 <- load_perf(load_RData("I_L2_m6.RData"), 2)
IL2b7 <- load_perf(load_RData("I_L2_m7.RData"), 2)
IL2b8 <- load_perf(load_RData("I_L2_m8.RData"), 2)
IL2b9 <- load_perf(load_RData("I_L2_m9.RData"), 2)
IL2a1 <- load_perf(load_RData("I_L2_mp1.RData"), 2)
IL2a2 <- load_perf(load_RData("I_L2_mp2.RData"), 2)
IL2a3 <- load_perf(load_RData("I_L2_mp3.RData"), 2)
IL2a4 <- load_perf(load_RData("I_L2_mp4.RData"), 2)
IL2a5 <- load_perf(load_RData("I_L2_mp5.RData"), 2)
IL2a6 <- load_perf(load_RData("I_L2_mp6.RData"), 2)
IL2a7 <- load_perf(load_RData("I_L2_mp7.RData"), 2)
IL2a8 <- load_perf(load_RData("I_L2_mp8.RData"), 2)
IL2a9 <- load_perf(load_RData("I_L2_mp9.RData"), 2)

before_FPR_L2 <- rbind(CI_TBI(IL2, 7, "FPR"),
                       CI_TBI(IL2b1, 2, "FPR"),   
                       CI_TBI(IL2b2, 2, "FPR"),
                       CI_TBI(IL2b3, 2, "FPR"),
                       CI_TBI(IL2b4, 2, "FPR"),
                       CI_TBI(IL2b5, 2, "FPR"),
                       CI_TBI(IL2b6, 2, "FPR"),
                       CI_TBI(IL2b7, 2, "FPR"),
                       CI_TBI(IL2b8, 2, "FPR"),
                       CI_TBI(IL2b9, 2, "FPR"))

before_FNR_L2 <- rbind(CI_TBI(IL2, 7, "FNR"),
                       CI_TBI(IL2b1, 2, "FNR"),   
                       CI_TBI(IL2b2, 2, "FNR"),
                       CI_TBI(IL2b3, 2, "FNR"),
                       CI_TBI(IL2b4, 2, "FNR"),
                       CI_TBI(IL2b5, 2, "FNR"),
                       CI_TBI(IL2b6, 2, "FNR"),
                       CI_TBI(IL2b7, 2, "FNR"),
                       CI_TBI(IL2b8, 2, "FNR"),
                       CI_TBI(IL2b9, 2, "FNR"))
###
IL3 <- load_perf(load_RData("I_L3_m.RData"), 3)
IL3b1 <- load_perf(load_RData("I_L3_m1.RData"), 3)
IL3b2 <- load_perf(load_RData("I_L3_m2.RData"), 3)
IL3b3 <- load_perf(load_RData("I_L3_m3.RData"), 3)
IL3b4 <- load_perf(load_RData("I_L3_m4.RData"), 3)
IL3b5 <- load_perf(load_RData("I_L3_m5.RData"), 3)
IL3b6 <- load_perf(load_RData("I_L3_m6.RData"), 3)
IL3b7 <- load_perf(load_RData("I_L3_m7.RData"), 3)
IL3b8 <- load_perf(load_RData("I_L3_m8.RData"), 3)
IL3b9 <- load_perf(load_RData("I_L3_m9.RData"), 3)
IL3a1 <- load_perf(load_RData("I_L3_mp1.RData"), 3)
IL3a2 <- load_perf(load_RData("I_L3_mp2.RData"), 3)
IL3a3 <- load_perf(load_RData("I_L3_mp3.RData"), 3)
IL3a4 <- load_perf(load_RData("I_L3_mp4.RData"), 3)
IL3a5 <- load_perf(load_RData("I_L3_mp5.RData"), 3)
IL3a6 <- load_perf(load_RData("I_L3_mp6.RData"), 3)
IL3a7 <- load_perf(load_RData("I_L3_mp7.RData"), 3)
IL3a8 <- load_perf(load_RData("I_L3_mp8.RData"), 3)
IL3a9 <- load_perf(load_RData("I_L3_mp9.RData"), 3)

before_FPR_L3 <- rbind(CI_TBI(IL3, 7, "FPR"),
                       CI_TBI(IL3b1, 2, "FPR"),   
                       CI_TBI(IL3b2, 2, "FPR"),
                       CI_TBI(IL3b3, 2, "FPR"),
                       CI_TBI(IL3b4, 2, "FPR"),
                       CI_TBI(IL3b5, 2, "FPR"),
                       CI_TBI(IL3b6, 2, "FPR"),
                       CI_TBI(IL3b7, 2, "FPR"),
                       CI_TBI(IL3b8, 2, "FPR"),
                       CI_TBI(IL3b9, 2, "FPR"))

before_FNR_L3 <- rbind(CI_TBI(IL3, 7, "FNR"),
                       CI_TBI(IL3b1, 2, "FNR"),   
                       CI_TBI(IL3b2, 2, "FNR"),
                       CI_TBI(IL3b3, 2, "FNR"),
                       CI_TBI(IL3b4, 2, "FNR"),
                       CI_TBI(IL3b5, 2, "FNR"),
                       CI_TBI(IL3b6, 2, "FNR"),
                       CI_TBI(IL3b7, 2, "FNR"),
                       CI_TBI(IL3b8, 2, "FNR"),
                       CI_TBI(IL3b9, 2, "FNR"))
####################################################

IM1 <- load_perf(load_RData("I_M1_m.RData"), 1)
IM1b1 <- load_perf(load_RData("I_M1_m1.RData"), 1)
IM1b2 <- load_perf(load_RData("I_M1_m2.RData"), 1)
IM1b3 <- load_perf(load_RData("I_M1_m3.RData"), 1)
IM1b4 <- load_perf(load_RData("I_M1_m4.RData"), 1)
IM1b5 <- load_perf(load_RData("I_M1_m5.RData"), 1)
IM1b6 <- load_perf(load_RData("I_M1_m6.RData"), 1)
IM1b7 <- load_perf(load_RData("I_M1_m7.RData"), 1)
IM1b8 <- load_perf(load_RData("I_M1_m8.RData"), 1)
IM1b9 <- load_perf(load_RData("I_M1_m9.RData"), 1)
IM1a1 <- load_perf(load_RData("I_M1_mp1.RData"), 1)
IM1a2 <- load_perf(load_RData("I_M1_mp2.RData"), 1)
IM1a3 <- load_perf(load_RData("I_M1_mp3.RData"), 1)
IM1a4 <- load_perf(load_RData("I_M1_mp4.RData"), 1)
IM1a5 <- load_perf(load_RData("I_M1_mp5.RData"), 1)
IM1a6 <- load_perf(load_RData("I_M1_mp6.RData"), 1)
IM1a7 <- load_perf(load_RData("I_M1_mp7.RData"), 1)
IM1a8 <- load_perf(load_RData("I_M1_mp8.RData"), 1)
IM1a9 <- load_perf(load_RData("I_M1_mp9.RData"), 1)

before_FPR_M1 <- rbind(CI_TBI(IM1, 7, "FPR"),
                       CI_TBI(IM1b1, 2, "FPR"),   
                       CI_TBI(IM1b2, 2, "FPR"),
                       CI_TBI(IM1b3, 2, "FPR"),
                       CI_TBI(IM1b4, 2, "FPR"),
                       CI_TBI(IM1b5, 2, "FPR"),
                       CI_TBI(IM1b6, 2, "FPR"),
                       CI_TBI(IM1b7, 2, "FPR"),
                       CI_TBI(IM1b8, 2, "FPR"),
                       CI_TBI(IM1b9, 2, "FPR"))

before_FNR_M1 <- rbind(CI_TBI(IM1, 7, "FNR"),
                       CI_TBI(IM1b1, 2, "FNR"),   
                       CI_TBI(IM1b2, 2, "FNR"),
                       CI_TBI(IM1b3, 2, "FNR"),
                       CI_TBI(IM1b4, 2, "FNR"),
                       CI_TBI(IM1b5, 2, "FNR"),
                       CI_TBI(IM1b6, 2, "FNR"),
                       CI_TBI(IM1b7, 2, "FNR"),
                       CI_TBI(IM1b8, 2, "FNR"),
                       CI_TBI(IM1b9, 2, "FNR"))
###
IM2 <- load_perf(load_RData("I_M2_m.RData"), 2)
IM2b1 <- load_perf(load_RData("I_M2_m1.RData"), 2)
IM2b2 <- load_perf(load_RData("I_M2_m2.RData"), 2)
IM2b3 <- load_perf(load_RData("I_M2_m3.RData"), 2)
IM2b4 <- load_perf(load_RData("I_M2_m4.RData"), 2)
IM2b5 <- load_perf(load_RData("I_M2_m5.RData"), 2)
IM2b6 <- load_perf(load_RData("I_M2_m6.RData"), 2)
IM2b7 <- load_perf(load_RData("I_M2_m7.RData"), 2)
IM2b8 <- load_perf(load_RData("I_M2_m8.RData"), 2)
IM2b9 <- load_perf(load_RData("I_M2_m9.RData"), 2)
IM2a1 <- load_perf(load_RData("I_M2_mp1.RData"), 2)
IM2a2 <- load_perf(load_RData("I_M2_mp2.RData"), 2)
IM2a3 <- load_perf(load_RData("I_M2_mp3.RData"), 2)
IM2a4 <- load_perf(load_RData("I_M2_mp4.RData"), 2)
IM2a5 <- load_perf(load_RData("I_M2_mp5.RData"), 2)
IM2a6 <- load_perf(load_RData("I_M2_mp6.RData"), 2)
IM2a7 <- load_perf(load_RData("I_M2_mp7.RData"), 2)
IM2a8 <- load_perf(load_RData("I_M2_mp8.RData"), 2)
IM2a9 <- load_perf(load_RData("I_M2_mp9.RData"), 2)

before_FPR_M2 <- rbind(CI_TBI(IM2, 7, "FPR"),
                       CI_TBI(IM2b1, 2, "FPR"),   
                       CI_TBI(IM2b2, 2, "FPR"),
                       CI_TBI(IM2b3, 2, "FPR"),
                       CI_TBI(IM2b4, 2, "FPR"),
                       CI_TBI(IM2b5, 2, "FPR"),
                       CI_TBI(IM2b6, 2, "FPR"),
                       CI_TBI(IM2b7, 2, "FPR"),
                       CI_TBI(IM2b8, 2, "FPR"),
                       CI_TBI(IM2b9, 2, "FPR"))

before_FNR_M2 <- rbind(CI_TBI(IM2, 7, "FNR"),
                       CI_TBI(IM2b1, 2, "FNR"),   
                       CI_TBI(IM2b2, 2, "FNR"),
                       CI_TBI(IM2b3, 2, "FNR"),
                       CI_TBI(IM2b4, 2, "FNR"),
                       CI_TBI(IM2b5, 2, "FNR"),
                       CI_TBI(IM2b6, 2, "FNR"),
                       CI_TBI(IM2b7, 2, "FNR"),
                       CI_TBI(IM2b8, 2, "FNR"),
                       CI_TBI(IM2b9, 2, "FNR"))
###
IM3 <- load_perf(load_RData("I_M3_m.RData"), 3)
IM3b1 <- load_perf(load_RData("I_M3_m1.RData"), 3)
IM3b2 <- load_perf(load_RData("I_M3_m2.RData"), 3)
IM3b3 <- load_perf(load_RData("I_M3_m3.RData"), 3)
IM3b4 <- load_perf(load_RData("I_M3_m4.RData"), 3)
IM3b5 <- load_perf(load_RData("I_M3_m5.RData"), 3)
IM3b6 <- load_perf(load_RData("I_M3_m6.RData"), 3)
IM3b7 <- load_perf(load_RData("I_M3_m7.RData"), 3)
IM3b8 <- load_perf(load_RData("I_M3_m8.RData"), 3)
IM3b9 <- load_perf(load_RData("I_M3_m9.RData"), 3)
IM3a1 <- load_perf(load_RData("I_M3_mp1.RData"), 3)
IM3a2 <- load_perf(load_RData("I_M3_mp2.RData"), 3)
IM3a3 <- load_perf(load_RData("I_M3_mp3.RData"), 3)
IM3a4 <- load_perf(load_RData("I_M3_mp4.RData"), 3)
IM3a5 <- load_perf(load_RData("I_M3_mp5.RData"), 3)
IM3a6 <- load_perf(load_RData("I_M3_mp6.RData"), 3)
IM3a7 <- load_perf(load_RData("I_M3_mp7.RData"), 3)
IM3a8 <- load_perf(load_RData("I_M3_mp8.RData"), 3)
IM3a9 <- load_perf(load_RData("I_M3_mp9.RData"), 3)

before_FPR_M3 <- rbind(CI_TBI(IM3, 7, "FPR"),
                       CI_TBI(IM3b1, 2, "FPR"),   
                       CI_TBI(IM3b2, 2, "FPR"),
                       CI_TBI(IM3b3, 2, "FPR"),
                       CI_TBI(IM3b4, 2, "FPR"),
                       CI_TBI(IM3b5, 2, "FPR"),
                       CI_TBI(IM3b6, 2, "FPR"),
                       CI_TBI(IM3b7, 2, "FPR"),
                       CI_TBI(IM3b8, 2, "FPR"),
                       CI_TBI(IM3b9, 2, "FPR"))

before_FNR_M3 <- rbind(CI_TBI(IM3, 7, "FNR"),
                       CI_TBI(IM3b1, 2, "FNR"),   
                       CI_TBI(IM3b2, 2, "FNR"),
                       CI_TBI(IM3b3, 2, "FNR"),
                       CI_TBI(IM3b4, 2, "FNR"),
                       CI_TBI(IM3b5, 2, "FNR"),
                       CI_TBI(IM3b6, 2, "FNR"),
                       CI_TBI(IM3b7, 2, "FNR"),
                       CI_TBI(IM3b8, 2, "FNR"),
                       CI_TBI(IM3b9, 2, "FNR"))

################################################

IH1 <- load_perf(load_RData("I_H1_m.RData"), 1)
IH1b1 <- load_perf(load_RData("I_H1_m1.RData"), 1)
IH1b2 <- load_perf(load_RData("I_H1_m2.RData"), 1)
IH1b3 <- load_perf(load_RData("I_H1_m3.RData"), 1)
IH1b4 <- load_perf(load_RData("I_H1_m4.RData"), 1)
IH1b5 <- load_perf(load_RData("I_H1_m5.RData"), 1)
IH1b6 <- load_perf(load_RData("I_H1_m6.RData"), 1)
IH1b7 <- load_perf(load_RData("I_H1_m7.RData"), 1)
IH1b8 <- load_perf(load_RData("I_H1_m8.RData"), 1)
IH1b9 <- load_perf(load_RData("I_H1_m9.RData"), 1)
IH1a1 <- load_perf(load_RData("I_H1_mp1.RData"), 1)
IH1a2 <- load_perf(load_RData("I_H1_mp2.RData"), 1)
IH1a3 <- load_perf(load_RData("I_H1_mp3.RData"), 1)
IH1a4 <- load_perf(load_RData("I_H1_mp4.RData"), 1)
IH1a5 <- load_perf(load_RData("I_H1_mp5.RData"), 1)
IH1a6 <- load_perf(load_RData("I_H1_mp6.RData"), 1)
IH1a7 <- load_perf(load_RData("I_H1_mp7.RData"), 1)
IH1a8 <- load_perf(load_RData("I_H1_mp8.RData"), 1)
IH1a9 <- load_perf(load_RData("I_H1_mp9.RData"), 1)

before_FPR_H1 <- rbind(CI_TBI(IH1, 7, "FPR"),
                       CI_TBI(IH1b1, 2, "FPR"),   
                       CI_TBI(IH1b2, 2, "FPR"),
                       CI_TBI(IH1b3, 2, "FPR"),
                       CI_TBI(IH1b4, 2, "FPR"),
                       CI_TBI(IH1b5, 2, "FPR"),
                       CI_TBI(IH1b6, 2, "FPR"),
                       CI_TBI(IH1b7, 2, "FPR"),
                       CI_TBI(IH1b8, 2, "FPR"),
                       CI_TBI(IH1b9, 2, "FPR"))

before_FNR_H1 <- rbind(CI_TBI(IH1, 7, "FNR"),
                       CI_TBI(IH1b1, 2, "FNR"),   
                       CI_TBI(IH1b2, 2, "FNR"),
                       CI_TBI(IH1b3, 2, "FNR"),
                       CI_TBI(IH1b4, 2, "FNR"),
                       CI_TBI(IH1b5, 2, "FNR"),
                       CI_TBI(IH1b6, 2, "FNR"),
                       CI_TBI(IH1b7, 2, "FNR"),
                       CI_TBI(IH1b8, 2, "FNR"),
                       CI_TBI(IH1b9, 2, "FNR"))
###
IH2 <- load_perf(load_RData("I_H2_m.RData"), 2)
IH2b1 <- load_perf(load_RData("I_H2_m1.RData"), 2)
IH2b2 <- load_perf(load_RData("I_H2_m2.RData"), 2)
IH2b3 <- load_perf(load_RData("I_H2_m3.RData"), 2)
IH2b4 <- load_perf(load_RData("I_H2_m4.RData"), 2)
IH2b5 <- load_perf(load_RData("I_H2_m5.RData"), 2)
IH2b6 <- load_perf(load_RData("I_H2_m6.RData"), 2)
IH2b7 <- load_perf(load_RData("I_H2_m7.RData"), 2)
IH2b8 <- load_perf(load_RData("I_H2_m8.RData"), 2)
IH2b9 <- load_perf(load_RData("I_H2_m9.RData"), 2)
IH2a1 <- load_perf(load_RData("I_H2_mp1.RData"), 2)
IH2a2 <- load_perf(load_RData("I_H2_mp2.RData"), 2)
IH2a3 <- load_perf(load_RData("I_H2_mp3.RData"), 2)
IH2a4 <- load_perf(load_RData("I_H2_mp4.RData"), 2)
IH2a5 <- load_perf(load_RData("I_H2_mp5.RData"), 2)
IH2a6 <- load_perf(load_RData("I_H2_mp6.RData"), 2)
IH2a7 <- load_perf(load_RData("I_H2_mp7.RData"), 2)
IH2a8 <- load_perf(load_RData("I_H2_mp8.RData"), 2)
IH2a9 <- load_perf(load_RData("I_H2_mp9.RData"), 2)

before_FPR_H2 <- rbind(CI_TBI(IH2, 7, "FPR"),
                       CI_TBI(IH2b1, 2, "FPR"),   
                       CI_TBI(IH2b2, 2, "FPR"),
                       CI_TBI(IH2b3, 2, "FPR"),
                       CI_TBI(IH2b4, 2, "FPR"),
                       CI_TBI(IH2b5, 2, "FPR"),
                       CI_TBI(IH2b6, 2, "FPR"),
                       CI_TBI(IH2b7, 2, "FPR"),
                       CI_TBI(IH2b8, 2, "FPR"),
                       CI_TBI(IH2b9, 2, "FPR"))

before_FNR_H2 <- rbind(CI_TBI(IH2, 7, "FNR"),
                       CI_TBI(IH2b1, 2, "FNR"),   
                       CI_TBI(IH2b2, 2, "FNR"),
                       CI_TBI(IH2b3, 2, "FNR"),
                       CI_TBI(IH2b4, 2, "FNR"),
                       CI_TBI(IH2b5, 2, "FNR"),
                       CI_TBI(IH2b6, 2, "FNR"),
                       CI_TBI(IH2b7, 2, "FNR"),
                       CI_TBI(IH2b8, 2, "FNR"),
                       CI_TBI(IH2b9, 2, "FNR"))
###
IH3 <- load_perf(load_RData("I_H3_m.RData"), 3)
IH3b1 <- load_perf(load_RData("I_H3_m1.RData"), 3)
IH3b2 <- load_perf(load_RData("I_H3_m2.RData"), 3)
IH3b3 <- load_perf(load_RData("I_H3_m3.RData"), 3)
IH3b4 <- load_perf(load_RData("I_H3_m4.RData"), 3)
IH3b5 <- load_perf(load_RData("I_H3_m5.RData"), 3)
IH3b6 <- load_perf(load_RData("I_H3_m6.RData"), 3)
IH3b7 <- load_perf(load_RData("I_H3_m7.RData"), 3)
IH3b8 <- load_perf(load_RData("I_H3_m8.RData"), 3)
IH3b9 <- load_perf(load_RData("I_H3_m9.RData"), 3)
IH3a1 <- load_perf(load_RData("I_H3_mp1.RData"), 3)
IH3a2 <- load_perf(load_RData("I_H3_mp2.RData"), 3)
IH3a3 <- load_perf(load_RData("I_H3_mp3.RData"), 3)
IH3a4 <- load_perf(load_RData("I_H3_mp4.RData"), 3)
IH3a5 <- load_perf(load_RData("I_H3_mp5.RData"), 3)
IH3a6 <- load_perf(load_RData("I_H3_mp6.RData"), 3)
IH3a7 <- load_perf(load_RData("I_H3_mp7.RData"), 3)
IH3a8 <- load_perf(load_RData("I_H3_mp8.RData"), 3)
IH3a9 <- load_perf(load_RData("I_H3_mp9.RData"), 3)

before_FPR_H3 <- rbind(CI_TBI(IH3, 7, "FPR"),
                       CI_TBI(IH3b1, 2, "FPR"),   
                       CI_TBI(IH3b2, 2, "FPR"),
                       CI_TBI(IH3b3, 2, "FPR"),
                       CI_TBI(IH3b4, 2, "FPR"),
                       CI_TBI(IH3b5, 2, "FPR"),
                       CI_TBI(IH3b6, 2, "FPR"),
                       CI_TBI(IH3b7, 2, "FPR"),
                       CI_TBI(IH3b8, 2, "FPR"),
                       CI_TBI(IH3b9, 2, "FPR"))

before_FNR_H3 <- rbind(CI_TBI(IH3, 7, "FNR"),
                       CI_TBI(IH3b1, 2, "FNR"),   
                       CI_TBI(IH3b2, 2, "FNR"),
                       CI_TBI(IH3b3, 2, "FNR"),
                       CI_TBI(IH3b4, 2, "FNR"),
                       CI_TBI(IH3b5, 2, "FNR"),
                       CI_TBI(IH3b6, 2, "FNR"),
                       CI_TBI(IH3b7, 2, "FNR"),
                       CI_TBI(IH3b8, 2, "FNR"),
                       CI_TBI(IH3b9, 2, "FNR"))

after_FPR_H3 <- rbind(CI_TBI(IH3, 7, "FPR"),
                       CI_TBI(IH3a1, 2, "FPR"),   
                       CI_TBI(IH3a2, 2, "FPR"),
                       CI_TBI(IH3a3, 2, "FPR"),
                       CI_TBI(IH3a4, 2, "FPR"),
                       CI_TBI(IH3a5, 2, "FPR"),
                       CI_TBI(IH3a6, 2, "FPR"),
                       CI_TBI(IH3a7, 2, "FPR"),
                       CI_TBI(IH3a8, 2, "FPR"),
                       CI_TBI(IH3a9, 2, "FPR"))

after_FNR_H3 <- rbind(CI_TBI(IH3, 7, "FNR"),
                       CI_TBI(IH3a1, 2, "FNR"),   
                       CI_TBI(IH3a2, 2, "FNR"),
                       CI_TBI(IH3a3, 2, "FNR"),
                       CI_TBI(IH3a4, 2, "FNR"),
                       CI_TBI(IH3a5, 2, "FNR"),
                       CI_TBI(IH3a6, 2, "FNR"),
                       CI_TBI(IH3a7, 2, "FNR"),
                       CI_TBI(IH3a8, 2, "FNR"),
                       CI_TBI(IH3a9, 2, "FNR"))

after_FPR_H2 <- rbind(CI_TBI(IH2, 7, "FPR"),
                      CI_TBI(IH2a1, 2, "FPR"),   
                      CI_TBI(IH2a2, 2, "FPR"),
                      CI_TBI(IH2a3, 2, "FPR"),
                      CI_TBI(IH2a4, 2, "FPR"),
                      CI_TBI(IH2a5, 2, "FPR"),
                      CI_TBI(IH2a6, 2, "FPR"),
                      CI_TBI(IH2a7, 2, "FPR"),
                      CI_TBI(IH2a8, 2, "FPR"),
                      CI_TBI(IH2a9, 2, "FPR"))

after_FNR_H2 <- rbind(CI_TBI(IH2, 7, "FNR"),
                      CI_TBI(IH2a1, 2, "FNR"),   
                      CI_TBI(IH2a2, 2, "FNR"),
                      CI_TBI(IH2a3, 2, "FNR"),
                      CI_TBI(IH2a4, 2, "FNR"),
                      CI_TBI(IH2a5, 2, "FNR"),
                      CI_TBI(IH2a6, 2, "FNR"),
                      CI_TBI(IH2a7, 2, "FNR"),
                      CI_TBI(IH2a8, 2, "FNR"),
                      CI_TBI(IH2a9, 2, "FNR"))

after_FPR_H1 <- rbind(CI_TBI(IH1, 7, "FPR"),
                      CI_TBI(IH1a1, 2, "FPR"),   
                      CI_TBI(IH1a2, 2, "FPR"),
                      CI_TBI(IH1a3, 2, "FPR"),
                      CI_TBI(IH1a4, 2, "FPR"),
                      CI_TBI(IH1a5, 2, "FPR"),
                      CI_TBI(IH1a6, 2, "FPR"),
                      CI_TBI(IH1a7, 2, "FPR"),
                      CI_TBI(IH1a8, 2, "FPR"),
                      CI_TBI(IH1a9, 2, "FPR"))

after_FNR_H1 <- rbind(CI_TBI(IH1, 7, "FNR"),
                      CI_TBI(IH1a1, 2, "FNR"),   
                      CI_TBI(IH1a2, 2, "FNR"),
                      CI_TBI(IH1a3, 2, "FNR"),
                      CI_TBI(IH1a4, 2, "FNR"),
                      CI_TBI(IH1a5, 2, "FNR"),
                      CI_TBI(IH1a6, 2, "FNR"),
                      CI_TBI(IH1a7, 2, "FNR"),
                      CI_TBI(IH1a8, 2, "FNR"),
                      CI_TBI(IH1a9, 2, "FNR"))


after_FPR_M3 <- rbind(CI_TBI(IM3, 7, "FPR"),
                      CI_TBI(IM3a1, 2, "FPR"),   
                      CI_TBI(IM3a2, 2, "FPR"),
                      CI_TBI(IM3a3, 2, "FPR"),
                      CI_TBI(IM3a4, 2, "FPR"),
                      CI_TBI(IM3a5, 2, "FPR"),
                      CI_TBI(IM3a6, 2, "FPR"),
                      CI_TBI(IM3a7, 2, "FPR"),
                      CI_TBI(IM3a8, 2, "FPR"),
                      CI_TBI(IM3a9, 2, "FPR"))

after_FNR_M3 <- rbind(CI_TBI(IM3, 7, "FNR"),
                      CI_TBI(IM3a1, 2, "FNR"),   
                      CI_TBI(IM3a2, 2, "FNR"),
                      CI_TBI(IM3a3, 2, "FNR"),
                      CI_TBI(IM3a4, 2, "FNR"),
                      CI_TBI(IM3a5, 2, "FNR"),
                      CI_TBI(IM3a6, 2, "FNR"),
                      CI_TBI(IM3a7, 2, "FNR"),
                      CI_TBI(IM3a8, 2, "FNR"),
                      CI_TBI(IM3a9, 2, "FNR"))

after_FPR_M2 <- rbind(CI_TBI(IM2, 7, "FPR"),
                      CI_TBI(IM2a1, 2, "FPR"),   
                      CI_TBI(IM2a2, 2, "FPR"),
                      CI_TBI(IM2a3, 2, "FPR"),
                      CI_TBI(IM2a4, 2, "FPR"),
                      CI_TBI(IM2a5, 2, "FPR"),
                      CI_TBI(IM2a6, 2, "FPR"),
                      CI_TBI(IM2a7, 2, "FPR"),
                      CI_TBI(IM2a8, 2, "FPR"),
                      CI_TBI(IM2a9, 2, "FPR"))

after_FNR_M2 <- rbind(CI_TBI(IM2, 7, "FNR"),
                      CI_TBI(IM2a1, 2, "FNR"),   
                      CI_TBI(IM2a2, 2, "FNR"),
                      CI_TBI(IM2a3, 2, "FNR"),
                      CI_TBI(IM2a4, 2, "FNR"),
                      CI_TBI(IM2a5, 2, "FNR"),
                      CI_TBI(IM2a6, 2, "FNR"),
                      CI_TBI(IM2a7, 2, "FNR"),
                      CI_TBI(IM2a8, 2, "FNR"),
                      CI_TBI(IM2a9, 2, "FNR"))

after_FPR_M1 <- rbind(CI_TBI(IM1, 7, "FPR"),
                      CI_TBI(IM1a1, 2, "FPR"),   
                      CI_TBI(IM1a2, 2, "FPR"),
                      CI_TBI(IM1a3, 2, "FPR"),
                      CI_TBI(IM1a4, 2, "FPR"),
                      CI_TBI(IM1a5, 2, "FPR"),
                      CI_TBI(IM1a6, 2, "FPR"),
                      CI_TBI(IM1a7, 2, "FPR"),
                      CI_TBI(IM1a8, 2, "FPR"),
                      CI_TBI(IM1a9, 2, "FPR"))

after_FNR_M1 <- rbind(CI_TBI(IM1, 7, "FNR"),
                      CI_TBI(IM1a1, 2, "FNR"),   
                      CI_TBI(IM1a2, 2, "FNR"),
                      CI_TBI(IM1a3, 2, "FNR"),
                      CI_TBI(IM1a4, 2, "FNR"),
                      CI_TBI(IM1a5, 2, "FNR"),
                      CI_TBI(IM1a6, 2, "FNR"),
                      CI_TBI(IM1a7, 2, "FNR"),
                      CI_TBI(IM1a8, 2, "FNR"),
                      CI_TBI(IM1a9, 2, "FNR"))


after_FPR_L3 <- rbind(CI_TBI(IL3, 7, "FPR"),
                      CI_TBI(IL3a1, 2, "FPR"),   
                      CI_TBI(IL3a2, 2, "FPR"),
                      CI_TBI(IL3a3, 2, "FPR"),
                      CI_TBI(IL3a4, 2, "FPR"),
                      CI_TBI(IL3a5, 2, "FPR"),
                      CI_TBI(IL3a6, 2, "FPR"),
                      CI_TBI(IL3a7, 2, "FPR"),
                      CI_TBI(IL3a8, 2, "FPR"),
                      CI_TBI(IL3a9, 2, "FPR"))

after_FNR_L3 <- rbind(CI_TBI(IL3, 7, "FNR"),
                      CI_TBI(IL3a1, 2, "FNR"),   
                      CI_TBI(IL3a2, 2, "FNR"),
                      CI_TBI(IL3a3, 2, "FNR"),
                      CI_TBI(IL3a4, 2, "FNR"),
                      CI_TBI(IL3a5, 2, "FNR"),
                      CI_TBI(IL3a6, 2, "FNR"),
                      CI_TBI(IL3a7, 2, "FNR"),
                      CI_TBI(IL3a8, 2, "FNR"),
                      CI_TBI(IL3a9, 2, "FNR"))

after_FPR_L2 <- rbind(CI_TBI(IL2, 7, "FPR"),
                      CI_TBI(IL2a1, 2, "FPR"),   
                      CI_TBI(IL2a2, 2, "FPR"),
                      CI_TBI(IL2a3, 2, "FPR"),
                      CI_TBI(IL2a4, 2, "FPR"),
                      CI_TBI(IL2a5, 2, "FPR"),
                      CI_TBI(IL2a6, 2, "FPR"),
                      CI_TBI(IL2a7, 2, "FPR"),
                      CI_TBI(IL2a8, 2, "FPR"),
                      CI_TBI(IL2a9, 2, "FPR"))

after_FNR_L2 <- rbind(CI_TBI(IL2, 7, "FNR"),
                      CI_TBI(IL2a1, 2, "FNR"),   
                      CI_TBI(IL2a2, 2, "FNR"),
                      CI_TBI(IL2a3, 2, "FNR"),
                      CI_TBI(IL2a4, 2, "FNR"),
                      CI_TBI(IL2a5, 2, "FNR"),
                      CI_TBI(IL2a6, 2, "FNR"),
                      CI_TBI(IL2a7, 2, "FNR"),
                      CI_TBI(IL2a8, 2, "FNR"),
                      CI_TBI(IL2a9, 2, "FNR"))

after_FPR_L1 <- rbind(CI_TBI(IL1, 7, "FPR"),
                      CI_TBI(IL1a1, 2, "FPR"),   
                      CI_TBI(IL1a2, 2, "FPR"),
                      CI_TBI(IL1a3, 2, "FPR"),
                      CI_TBI(IL1a4, 2, "FPR"),
                      CI_TBI(IL1a5, 2, "FPR"),
                      CI_TBI(IL1a6, 2, "FPR"),
                      CI_TBI(IL1a7, 2, "FPR"),
                      CI_TBI(IL1a8, 2, "FPR"),
                      CI_TBI(IL1a9, 2, "FPR"))

after_FNR_L1 <- rbind(CI_TBI(IL1, 7, "FNR"),
                      CI_TBI(IL1a1, 2, "FNR"),   
                      CI_TBI(IL1a2, 2, "FNR"),
                      CI_TBI(IL1a3, 2, "FNR"),
                      CI_TBI(IL1a4, 2, "FNR"),
                      CI_TBI(IL1a5, 2, "FNR"),
                      CI_TBI(IL1a6, 2, "FNR"),
                      CI_TBI(IL1a7, 2, "FNR"),
                      CI_TBI(IL1a8, 2, "FNR"),
                      CI_TBI(IL1a9, 2, "FNR"))

####################################################

dfFNR_before <- data.frame(years=9:0,
                           IL1=before_FNR_L1[,2],
                           IL2=before_FNR_L2[,2],
                           IL3=before_FNR_L3[,2],
                           IM1=before_FNR_M1[,2],
                           IM2=before_FNR_M2[,2],
                           IM3=before_FNR_M3[,2],
                           IH1=before_FNR_H1[,2],
                           IH2=before_FNR_H2[,2],
                           IH3=before_FNR_H3[,2])

dfFPR_before <- data.frame(years=9:0,
                           IL1=before_FPR_L1[,2],
                           IL2=before_FPR_L2[,2],
                           IL3=before_FPR_L3[,2],
                           IM1=before_FPR_M1[,2],
                           IM2=before_FPR_M2[,2],
                           IM3=before_FPR_M3[,2],
                           IH1=before_FPR_H1[,2],
                           IH2=before_FPR_H2[,2],
                           IH3=before_FPR_H3[,2])

dfFNR_after <- data.frame(years=0:9,
                           IL1=after_FNR_L1[,2],
                           IL2=after_FNR_L2[,2],
                           #IL3=after_FNR_L3[,2],
                           #IM1=after_FNR_M1[,2],
                           #IM2=after_FNR_M2[,2],
                           #IM3=after_FNR_M3[,2],
                           IH1=after_FNR_H1[,2],
                           IH2=after_FNR_H2[,2],
                           IH3=after_FNR_H3[,2])

dfFPR_after <- data.frame(years=0:9,
                           IL1=after_FPR_L1[,2],
                           IL2=after_FPR_L2[,2],
                           #IL3=after_FPR_L3[,2],
                           #IM1=after_FPR_M1[,2],
                           #IM2=after_FPR_M2[,2],
                           #IM3=after_FPR_M3[,2],
                           IH1=after_FPR_H1[,2],
                           IH2=after_FPR_H2[,2],
                           IH3=after_FPR_H3[,2])

sz <- 3
sz2 <- 1
col1 <- "yellow"
col2 <- "orange"
col3 <- "red"

ggplot(dfFNR_before, aes(years, y = value)) + 
  geom_point(aes(y = IL1),  color = col1, pch=17, size=sz) +
  geom_point(aes(y = IL2),  color =col1, pch=15, size=sz) +
  geom_point(aes(y = IL3),  color =col1, pch=19, size=sz) +
  geom_point(aes(y = IM1),  color =col2, pch=17, size=sz) +
  geom_point(aes(y = IM2),  color =col2, pch=15, size=sz) +
  geom_point(aes(y = IM3),  color =col2, pch=19, size=sz) +
  geom_point(aes(y = IH1),  color =col3, pch=17, size=sz) +
  geom_point(aes(y = IH2),  color =col3, pch=15, size=sz) +
  geom_point(aes(y = IH3),  color =col3, pch=19, size=sz) +
  geom_line(aes(y = IL1),  color =col1, size=sz2) +
  geom_line(aes(y = IL2),  color =col1, size=sz2) +
  geom_line(aes(y = IL3),  color =col1, size=sz2) +
  geom_line(aes(y = IM1),  color =col2, size=sz2) +
  geom_line(aes(y = IM2),  color =col2, size=sz2) +
  geom_line(aes(y = IM3),  color =col2, size=sz2) +
  geom_line(aes(y = IH1),  color =col3, size=sz2) +
  geom_line(aes(y = IH2),  color =col3, size=sz2) +
  geom_line(aes(y = IH3),  color =col3, size=sz2) +
  labs(fill = "scenario") +
  ylab("FNR") +
  xlab("Years") +
  ylim(0, 1) +
  theme(text=element_text(size=12,  family="serif"))

ggplot(dfFPR_before, aes(years, y = value)) + 
  geom_point(aes(y = IL1),  color = col1, pch=17, size=sz) +
  geom_point(aes(y = IL2),  color =col1, pch=15, size=sz) +
  geom_point(aes(y = IL3),  color =col1, pch=19, size=sz) +
  geom_point(aes(y = IM1),  color =col2, pch=17, size=sz) +
  geom_point(aes(y = IM2),  color =col2, pch=15, size=sz) +
  geom_point(aes(y = IM3),  color =col2, pch=19, size=sz) +
  geom_point(aes(y = IH1),  color =col3, pch=17, size=sz) +
  geom_point(aes(y = IH2),  color =col3, pch=15, size=sz) +
  geom_point(aes(y = IH3),  color =col3, pch=19, size=sz) +
  geom_line(aes(y = IL1),  color =col1, size=sz2) +
  geom_line(aes(y = IL2),  color =col1, size=sz2) +
  geom_line(aes(y = IL3),  color =col1, size=sz2) +
  geom_line(aes(y = IM1),  color =col2, size=sz2) +
  geom_line(aes(y = IM2),  color =col2, size=sz2) +
  geom_line(aes(y = IM3),  color =col2, size=sz2) +
  geom_line(aes(y = IH1),  color =col3, size=sz2) +
  geom_line(aes(y = IH2),  color =col3, size=sz2) +
  geom_line(aes(y = IH3),  color =col3, size=sz2) +
  labs(fill = "scenario") +
  ylab("FPR") +
  xlab("Years") +
  ylim(0, 1) +
  theme(text=element_text(size=12,  family="serif"))

ggplot(dfFNR_after, aes(years, y = value)) + 
  geom_point(aes(y = IL1),  color = col1, pch=17, size=sz) +
  geom_point(aes(y = IL2),  color =col1, pch=15, size=sz) +
  geom_point(aes(y = IH1),  color =col3, pch=17, size=sz) +
  geom_point(aes(y = IH2),  color =col3, pch=15, size=sz) +
  geom_point(aes(y = IH3),  color =col3, pch=19, size=sz) +
  geom_line(aes(y = IL1),  color =col1, size=sz2) +
  geom_line(aes(y = IL2),  color =col1, size=sz2) +
  geom_line(aes(y = IH1),  color =col3, size=sz2) +
  geom_line(aes(y = IH2),  color =col3, size=sz2) +
  geom_line(aes(y = IH3),  color =col3, size=sz2) +
  labs(fill = "scenario") +
  ylab("FNR") +
  xlab("Years") +
  ylim(0, 1) +
  theme(text=element_text(size=12,  family="serif"))

ggplot(dfFPR_after, aes(years, y = value)) + 
  geom_point(aes(y = IL1),  color = col1, pch=17, size=sz) +
  geom_point(aes(y = IL2),  color =col1, pch=15, size=sz) +
  geom_point(aes(y = IH1),  color =col3, pch=17, size=sz) +
  geom_point(aes(y = IH2),  color =col3, pch=15, size=sz) +
  geom_point(aes(y = IH3),  color =col3, pch=19, size=sz) +
  geom_line(aes(y = IL1),  color =col1, size=sz2) +
  geom_line(aes(y = IL2),  color =col1, size=sz2) +
  geom_line(aes(y = IH1),  color =col3, size=sz2) +
  geom_line(aes(y = IH2),  color =col3, size=sz2) +
  geom_line(aes(y = IH3),  color =col3, size=sz2) +
  labs(fill = "scenario") +
  ylab("FPR") +
  xlab("Years") +
  ylim(0, 1) +
  theme(text=element_text(size=12,  family="serif"))

                     

before_FPR_L1_d <- data.frame(years=9:0,L1before_FPR_L1[,2])
before_FPR_M1_d <- data.frame(years=9:0,before_FPR_M1[,2])

dfFPR_I <- data.frame(alpha,
                      IL1=sum_perf(load_perf(IL1, 1), "FPR"),
                      IL2=sum_perf(load_perf(IL2, 2), "FPR"),
                      IL3=sum_perf(load_perf(IL3, 3), "FPR"),
                      IM1=sum_perf(load_perf(IM1, 1), "FPR"),
                      IM2=sum_perf(load_perf(IM2, 2), "FPR"),
                      IM3=sum_perf(load_perf(IM3, 3), "FPR"),
                      IH1=sum_perf(load_perf(IH1, 1), "FPR"),
                      IH2=sum_perf(load_perf(IH2, 2), "FPR"),
                      IH3=sum_perf(load_perf(IH3, 3), "FPR"),
                      CL=sum_perf(load_perf(CL, "control"), "FPR", TRUE),
                      CM=sum_perf(load_perf(CM, "control"), "FPR", TRUE),
                      CH=sum_perf(load_perf(CH, "control"), "FPR", TRUE))

dfFNR_I <- data.frame(alpha,
                      IL1=sum_perf(load_perf(IL1, 1), "FNR"),
                      IL2=sum_perf(load_perf(IL2, 2), "FNR"),
                      IL3=sum_perf(load_perf(IL3, 3), "FNR"),
                      IM1=sum_perf(load_perf(IM1, 1), "FNR"),
                      IM2=sum_perf(load_perf(IM2, 2), "FNR"),
                      IM3=sum_perf(load_perf(IM3, 3), "FNR"),
                      IH1=sum_perf(load_perf(IH1, 1), "FNR"),
                      IH2=sum_perf(load_perf(IH2, 2), "FNR"),
                      IH3=sum_perf(load_perf(IH3, 3), "FNR"))