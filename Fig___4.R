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

IH3 <- load_perf(load_RData("I_H3_m.RData"), 1)
IH3b1 <- load_perf(load_RData("I_H3_m1.RData"), 1)
IH3b2 <- load_perf(load_RData("I_H3_m2.RData"), 1)
IH3b3 <- load_perf(load_RData("I_H3_m3.RData"), 1)
IH3b4 <- load_perf(load_RData("I_H3_m4.RData"), 1)
IH3b5 <- load_perf(load_RData("I_H3_m5.RData"), 1)
IH3b6 <- load_perf(load_RData("I_H3_m6.RData"), 1)
IH3b7 <- load_perf(load_RData("I_H3_m7.RData"), 1)
IH3b8 <- load_perf(load_RData("I_H3_m8.RData"), 1)
IH3b9 <- load_perf(load_RData("I_H3_m9.RData"), 1)

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

dfFPR_before <- data.frame(years=9:0,
                      IL1=before_FPR_L1[,2],
                      IM1=before_FPR_M1[,2],
                      IH3=before_FPR_H3[,2])

dfFNR_before <- data.frame(years=9:0,
                           IL1=before_FNR_L1[,2],
                           IM1=before_FNR_M1[,2],
                           IH3=before_FPR_H3[,2])

ggplot(dfFPR_before, aes(years, y = value)) + 
  geom_point(aes(y = IL1),  color =col1, pch=17, size=sz) +
  geom_point(aes(y = IM1),  color =col2, pch=17, size=sz) +
  geom_point(aes(y = IH3),  color =col3, pch=19, size=sz) +
  geom_line(aes(y = IL1),  color =col1, size=sz2) +
  geom_line(aes(y = IM1),  color =col2, size=sz2) +
  geom_line(aes(y = IH3),  color =col3, size=sz2) +
  labs(fill = "scenario") +
  ylab("FPR") +
  xlab("Threshold") +
  theme(text=element_text(size=12,  family="serif"))

ggplot(dfFNR_before, aes(years, y = value)) + 
  geom_point(aes(y = IL1),  color =col1, pch=17, size=sz) +
  geom_point(aes(y = IM1),  color =col2, pch=17, size=sz) +
  geom_point(aes(y = IH3),  color =col3, pch=19, size=sz) +
  geom_line(aes(y = IL1),  color =col1, size=sz2) +
  geom_line(aes(y = IM1),  color =col2, size=sz2) +
  geom_line(aes(y = IH3),  color =col3, size=sz2) +
  labs(fill = "scenario") +
  ylab("FNR") +
  xlab("Threshold") +
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