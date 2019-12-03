setwd("E:/Globus/RData")

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

BL1 <- load_RData("B_L1_m.RData")
BL2 <- load_RData("B_L2_m.RData")
BL3 <- load_RData("B_L3_m.RData")
BM1 <- load_RData("B_M1_m.RData")
BM2 <- load_RData("B_M2_m.RData")
BM3 <- load_RData("B_M3_m.RData")
BH1 <- load_RData("B_H1_m.RData")
BH2 <- load_RData("B_H2_m.RData")
BH3 <- load_RData("B_H3_m.RData")
CL <- load_RData("C_L_m.RData")
CM <- load_RData("C_M_m.RData")
CH <- load_RData("C_H_m.RData")

sum_perf(load_perf(BH3, 3), "FPR", FALSE)
sum_perf(load_perf(CL, "control"), "FPR", TRUE)
sum_perf(load_perf(CM, "control"), "FPR", TRUE)
sum_perf(load_perf(CH, "control"), "FPR", TRUE)

alpha <- c(0.001, 0.0025, 0.005, 0.0075, 0.01, 0.025, 0.05, 0.075, 0.1)

dfFPR <- data.frame(alpha,
                 BL1=sum_perf(load_perf(BL1, 1), "FPR"),
                 BL2=sum_perf(load_perf(BL2, 2), "FPR"),
                 BL3=sum_perf(load_perf(BL3, 3), "FPR"),
                 BM1=sum_perf(load_perf(BM1, 1), "FPR"),
                 BM2=sum_perf(load_perf(BM2, 2), "FPR"),
                 BM3=sum_perf(load_perf(BM3, 3), "FPR"),
                 BH1=sum_perf(load_perf(BH1, 1), "FPR"),
                 BH2=sum_perf(load_perf(BH2, 2), "FPR"),
                 BH3=sum_perf(load_perf(BH3, 3), "FPR"))

dfFNR <- data.frame(alpha,
                    BL1=sum_perf(load_perf(BL1, 1), "FNR"),
                    BL2=sum_perf(load_perf(BL2, 2), "FNR"),
                    BL3=sum_perf(load_perf(BL3, 3), "FNR"),
                    BM1=sum_perf(load_perf(BM1, 1), "FNR"),
                    BM2=sum_perf(load_perf(BM2, 2), "FNR"),
                    BM3=sum_perf(load_perf(BM3, 3), "FNR"),
                    BH1=sum_perf(load_perf(BH1, 1), "FNR"),
                    BH2=sum_perf(load_perf(BH2, 2), "FNR"),
                    BH3=sum_perf(load_perf(BH3, 3), "FNR"))

dfcontrol <- data.frame(alpha,
                        CL=sum_perf(load_perf(CL, "control"), "FPR", TRUE),
                        CM=sum_perf(load_perf(CM, "control"), "FPR", TRUE),
                        CH=sum_perf(load_perf(CH, "control"), "FPR", TRUE))
                   

sz <- 3
sz2 <- 1
col1 <- "yellow"
col2 <- "orange"
col3 <- "red"
ggplot(dfFNR, aes(alpha, y = value)) + 
  geom_point(aes(y = BL1),  color = col1, pch=17, size=sz) +
  geom_point(aes(y = BL2),  color =col2, pch=17, size=sz) +
  geom_point(aes(y = BL3),  color =col3, pch=17, size=sz) +
  geom_point(aes(y = BM1),  color =col1, pch=15, size=sz) +
  geom_point(aes(y = BM2),  color =col2, pch=15, size=sz) +
  geom_point(aes(y = BM3),  color =col3, pch=15, size=sz) +
  geom_point(aes(y = BH1),  color =col1, pch=19, size=sz) +
  geom_point(aes(y = BH2),  color =col2, pch=19, size=sz) +
  geom_point(aes(y = BH3),  color =col3, pch=19, size=sz) +
  geom_line(aes(y = BL1),  color =col1, size=sz2) +
  geom_line(aes(y = BL2),  color =col2, size=sz2) +
  geom_line(aes(y = BL3),  color =col3, size=sz2) +
  geom_line(aes(y = BM1),  color =col1, size=sz2) +
  geom_line(aes(y = BM2),  color =col2, size=sz2) +
  geom_line(aes(y = BM3),  color =col3, size=sz2) +
  geom_line(aes(y = BH1),  color =col1, size=sz2) +
  geom_line(aes(y = BH2),  color =col2, size=sz2) +
  geom_line(aes(y = BH3),  color =col3, size=sz2)
######################################################################
sz <- 3
sz2 <- 1
col1 <- "yellow"
col2 <- "orange"
col3 <- "red"
ggplot(dfFPR, aes(alpha, y = value)) + 
  geom_point(aes(y = BL1),  color = col1, pch=17, size=sz) +
  geom_point(aes(y = BL2),  color =col2, pch=17, size=sz) +
  geom_point(aes(y = BL3),  color =col3, pch=17, size=sz) +
  geom_point(aes(y = BM1),  color =col1, pch=15, size=sz) +
  geom_point(aes(y = BM2),  color =col2, pch=15, size=sz) +
  geom_point(aes(y = BM3),  color =col3, pch=15, size=sz) +
  geom_point(aes(y = BH1),  color =col1, pch=19, size=sz) +
  geom_point(aes(y = BH2),  color =col2, pch=19, size=sz) +
  geom_point(aes(y = BH3),  color =col3, pch=19, size=sz) +
  geom_line(aes(y = BL1),  color =col1, size=sz2) +
  geom_line(aes(y = BL2),  color =col2, size=sz2) +
  geom_line(aes(y = BL3),  color =col3, size=sz2) +
  geom_line(aes(y = BM1),  color =col1, size=sz2) +
  geom_line(aes(y = BM2),  color =col2, size=sz2) +
  geom_line(aes(y = BM3),  color =col3, size=sz2) +
  geom_line(aes(y = BH1),  color =col1, size=sz2) +
  geom_line(aes(y = BH2),  color =col2, size=sz2) +
  geom_line(aes(y = BH3),  color =col3, size=sz2) 
######################################################################
sz <- 3
sz2 <- 1
col <- "black"

ggplot(dfcontrol, aes(alpha, y = value)) + 
  geom_point(aes(y = CL),  color = col, pch=17, size=sz) +
  geom_point(aes(y = CM),  color =col, pch=15, size=sz) +
  geom_point(aes(y = CH),  color =col, pch=19, size=sz) +
  geom_line(aes(y = CL),  color =col, size=sz2) +
  geom_line(aes(y = CM),  color =col, size=sz2) +
  geom_line(aes(y = CH),  color =col, size=sz2)


##################################################################################################################################################
# dfFPR_melt <- melt(dfFPR ,  id.vars = 'alpha', variable.name = 'scenario')
# dfFNR_melt <- melt(dfFNR ,  id.vars = 'alpha', variable.name = 'scenario')
# 
# dfFNR_melt <- cbind(dfFNR_melt, col=rep(c("black", "blue", "green"), each = 27))
# 
# ggplot(dfFNR_melt, aes(alpha,value)) +
#   geom_line(aes(colour = scenario))
ggplot(dfFNR, aes(alpha, y = value, color = variable)) + 
  geom_point(aes(y = BL1, col = "BL1")) + 
  geom_point(aes(y = BL2, col = "BL1")) +
  geom_point(aes(y = BL3, col = "BL1")) +
  geom_point(aes(y = BM1, col = "BL2")) +
  geom_point(aes(y = BL2, col = "BL2")) +
  geom_point(aes(y = BL2, col = "BL2")) +
  geom_point(aes(y = BL2, col = "BL2")) +
  geom_point(aes(y = BL2, col = "BL2")) +
  geom_point(aes(y = BL2, col = "BL2")) +
# allbl1 <- data.frame(alpha, meansfprbl1, meansfnrbl1)
# allbl1_m <- melt(allbl1, id.vars="alpha")


pl <- ggplot(data=allbl1_m, aes(x=alpha, y=value, col=variable)) +
  geom_line() +
  geom_point() +
  scale_colour_manual(values = c("blue","black"),
                      labels = c("FPR", "FNR"),
                      name = "Performance") +
  geom_hline(yintercept = 0.1, linetype = "dashed") +
  theme(text=element_text(size=12,  family="serif")) +
  coord_fixed(ratio=0.25)

pl

pl <- ggplot(data=allbl2_m, aes(x=alpha, y=value, col=variable)) +
  geom_line() +
  geom_point() +
  scale_colour_manual(values = c("blue","black"),
                      labels = c("FPR", "FNR"),
                      name = "Performance") +
  geom_hline(yintercept = 0.1, linetype = "dashed") +
  theme(text=element_text(size=12,  family="serif")) +
  coord_fixed(ratio=0.25)

pl

pl <- ggplot(data=allbh3_m, aes(x=alpha, y=value, col=variable)) +
  geom_line() +
  geom_point() +
  scale_colour_manual(values = c("blue","black"),
                      labels = c("FPR", "FNR"),
                      name = "Performance") +
  geom_hline(yintercept = 0.1, linetype = "dashed") +
  theme(text=element_text(size=12,  family="serif")) +
  coord_fixed(ratio=0.25)

pl