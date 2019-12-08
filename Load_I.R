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

alpha <- c(0.0001, 0.00025, 0.0005, 0.00075, 0.001, 0.0025, 0.005, 0.0075, 0.01, 0.025, 0.05, 0.075, 0.1, 0.15)

dfFPR_I <- data.frame(alpha,
                      IL1=sum_perf(load_perf(IL1, 1), "FPR"),
                      IL2=sum_perf(load_perf(IL2, 2), "FPR"),
                      IL3=sum_perf(load_perf(IL3, 3), "FPR"),
                      IM1=sum_perf(load_perf(IM1, 1), "FPR"),
                      IM2=sum_perf(load_perf(IM2, 2), "FPR"),
                      IM3=sum_perf(load_perf(IM3, 3), "FPR"),
                      IH1=sum_perf(load_perf(IH1, 1), "FPR"),
                      IH2=sum_perf(load_perf(IH2, 2), "FPR"),
                      IH3=sum_perf(load_perf(IH3, 3), "FPR"))

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

ggplot(dfFNR_I, aes(alpha, y = value)) + 
  geom_point(aes(y = IL1),  color = col1, pch=17, size=sz) +
  geom_point(aes(y = IL2),  color =col2, pch=17, size=sz) +
  geom_point(aes(y = IL3),  color =col3, pch=17, size=sz) +
  geom_point(aes(y = IM1),  color =col1, pch=15, size=sz) +
  geom_point(aes(y = IM2),  color =col2, pch=15, size=sz) +
  geom_point(aes(y = IM3),  color =col3, pch=15, size=sz) +
  geom_point(aes(y = IH1),  color =col1, pch=19, size=sz) +
  geom_point(aes(y = IH2),  color =col2, pch=19, size=sz) +
  geom_point(aes(y = IH3),  color =col3, pch=19, size=sz) +
  geom_line(aes(y = IL1),  color =col1, size=sz2) +
  geom_line(aes(y = IL2),  color =col2, size=sz2) +
  geom_line(aes(y = IL3),  color =col3, size=sz2) +
  geom_line(aes(y = IM1),  color =col1, size=sz2) +
  geom_line(aes(y = IM2),  color =col2, size=sz2) +
  geom_line(aes(y = IM3),  color =col3, size=sz2) +
  geom_line(aes(y = IH1),  color =col1, size=sz2) +
  geom_line(aes(y = IH2),  color =col2, size=sz2) +
  geom_line(aes(y = IH3),  color =col3, size=sz2) + 
  # scale_colour_manual(values = c(col1, col2, col3), labels = c("FPR", "FNR"), name = "Performance") +
  labs(fill = "scenario") +
  theme(text=element_text(size=12,  family="serif"))

ggplot(dfFPR_I, aes(alpha, y = value)) + 
  geom_point(aes(y = IL1),  color = col1, pch=17, size=sz) +
  geom_point(aes(y = IL2),  color =col2, pch=17, size=sz) +
  geom_point(aes(y = IL3),  color =col3, pch=17, size=sz) +
  geom_point(aes(y = IM1),  color =col1, pch=15, size=sz) +
  geom_point(aes(y = IM2),  color =col2, pch=15, size=sz) +
  geom_point(aes(y = IM3),  color =col3, pch=15, size=sz) +
  geom_point(aes(y = IH1),  color =col1, pch=19, size=sz) +
  geom_point(aes(y = IH2),  color =col2, pch=19, size=sz) +
  geom_point(aes(y = IH3),  color =col3, pch=19, size=sz) +
  geom_line(aes(y = IL1),  color =col1, size=sz2) +
  geom_line(aes(y = IL2),  color =col2, size=sz2) +
  geom_line(aes(y = IL3),  color =col3, size=sz2) +
  geom_line(aes(y = IM1),  color =col1, size=sz2) +
  geom_line(aes(y = IM2),  color =col2, size=sz2) +
  geom_line(aes(y = IM3),  color =col3, size=sz2) +
  geom_line(aes(y = IH1),  color =col1, size=sz2) +
  geom_line(aes(y = IH2),  color =col2, size=sz2) +
  geom_line(aes(y = IH3),  color =col3, size=sz2) + 
  # scale_colour_manual(values = c(col1, col2, col3), labels = c("FPR", "FNR"), name = "Performance") +
  labs(fill = "scenario") +
  theme(text=element_text(size=12,  family="serif"))