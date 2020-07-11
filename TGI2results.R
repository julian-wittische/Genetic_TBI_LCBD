# Function to turn saved TGI outcomes into usable results
# Julian Wittische
# November 2019

setwd("C:/Users/jwitt/OneDrive/Documents/TBI_files")
IH1_new <- load_perf(load_RData("I_H1_m_new2.RData"), 1)
IH2_new <- load_perf(load_RData("I_H2_m_new2.RData"), 2)
IH3_new <- load_perf(load_RData("I_H1_m_new2.RData"), 3)
IM1_new <- load_perf(load_RData("I_M1_m_new2.RData"), 1)
IM2_new <- load_perf(load_RData("I_M2_m_new2.RData"), 2)
IM3_new <- load_perf(load_RData("I_M3_m_new2.RData"), 3)
IL1_new <- load_perf(load_RData("I_L1_m_new2.RData"), 1)
IL2_new <- load_perf(load_RData("I_L2_m_new2.RData"), 2)
IL3_new <- load_perf(load_RData("I_L3_m_new2.RData"), 3)
CL <- load_perf(load_RData("C_L_m_new2.RData"), "control")[6:21]
CM <- load_perf(load_RData("C_M_m_new2.RData"), "control")[6:21]
CH <- load_perf(load_RData("C_H_m_new2.RData"), "control")[6:21]

alpha <- c(0.025, 0.030, 0.035, 0.040, 0.045, 0.050, 0.055,
           0.060, 0.065, 0.070, 0.075, 0.080, 0.085, 0.090, 0.095, 0.100)
#0.001, 0.005, 0.010, 0.015, 0.020,

dfFPR_I <- data.frame(alpha,
                      IL1=sum_perf(IL1_new, "FPR"),
                      IL2=sum_perf(IL2_new, "FPR"),
                      IL3=sum_perf(IL3_new, "FPR"),
                      #IM1=sum_perf(IM1_new, "FPR"),
                      IM2=sum_perf(IM2_new, "FPR"),
                      IM3=sum_perf(IM3_new, "FPR"),
                      IH1=sum_perf(IH1_new, "FPR"),
                      IH2=sum_perf(IH2_new, "FPR"),
                      IH3=sum_perf(IH3_new, "FPR"),
                      CL=sum_perf(CL, "FPR", TRUE),
                      CM=sum_perf(CM, "FPR", TRUE),
                      CH=sum_perf(CH, "FPR", TRUE)
                      )

dfFNR_I <- data.frame(alpha,
                      IL1=sum_perf(IL1_new, "FNR"),
                      IL2=sum_perf(IL2_new, "FNR"),
                      IL3=sum_perf(IL3_new, "FNR"),
                      #IM1=sum_perf(IM1_new, "FNR"),
                      IM2=sum_perf(IM2_new, "FNR"),
                      IM3=sum_perf(IM3_new, "FNR"),
                      IH1=sum_perf(IH1_new, "FNR"),
                      IH2=sum_perf(IH2_new, "FNR"),#,
                      IH3=sum_perf(IH3_new, "FNR")
                      )

sz <- 3
sz2 <- 1
sz3 <-0.5
widtherr <- 0.001
col1 <- "gray85"
col2 <- "gray50"
col3 <- "black"

ggplot(dfFNR_I, aes(alpha)) + 
  
  geom_line(aes(y = IL1.mean,  color ="col1"), size=sz2) +
  geom_line(aes(y = IL2.mean,  color ="col1"), size=sz2) +
  geom_line(aes(y = IL3.mean,  color ="col1"), size=sz2) +
  #geom_line(aes(y = IM1.mean,  color ="col2"), size=sz2) +
  geom_line(aes(y = IM2.mean,  color ="col2"), size=sz2) +
  geom_line(aes(y = IM3.mean,  color ="col2"), size=sz2) +
  geom_line(aes(y = IH1.mean,  color ="col3"), size=sz2) +
  geom_line(aes(y = IH2.mean,  color ="col3"), size=sz2) +
  geom_line(aes(y = IH3.mean,  color ="col3"), size=sz2) +
  
  geom_point(aes(y = IL1.mean, pch = "pch1"),  color =col1, size=sz) +
  geom_point(aes(y = IL2.mean, pch = "pch2"),  color =col1, size=sz) +
  geom_point(aes(y = IL3.mean, pch = "pch3"),  color =col1, size=sz) +
  #geom_point(aes(y = IM1.mean, pch = "pch1"),  color =col2, size=sz) +
  geom_point(aes(y = IM2.mean, pch = "pch2"),  color =col2, size=sz) +
  geom_point(aes(y = IM3.mean, pch = "pch3"),  color =col2, size=sz) +
  geom_point(aes(y = IH1.mean, pch = "pch1"),  color =col3, size=sz) +
  geom_point(aes(y = IH2.mean, pch = "pch2"),  color =col3, size=sz) +
  geom_point(aes(y = IH3.mean, pch = "pch3"),  color =col3, size=sz) +
  
  geom_errorbar(aes(ymin = IL1.lower, ymax = IL1.upper), width= widtherr ,  color = col1, size=sz3) +
  geom_errorbar(aes(ymin = IL2.lower, ymax = IL2.upper), width= widtherr ,  color =col1, size=sz3) +
  geom_errorbar(aes(ymin = IL3.lower, ymax = IL3.upper), width= widtherr ,  color =col1, size=sz3) +
  #geom_errorbar(aes(ymin = IM1.lower, ymax = IM1.upper), width= widtherr ,  color =col2, size=sz3) +
  geom_errorbar(aes(ymin = IM2.lower, ymax = IM2.upper), width= widtherr ,  color =col2, size=sz3) +
  geom_errorbar(aes(ymin = IM3.lower, ymax = IM3.upper), width= widtherr ,  color =col2, size=sz3) +
  geom_errorbar(aes(ymin = IH1.lower, ymax = IH1.upper), width= widtherr ,  color =col3, size=sz3) +
  geom_errorbar(aes(ymin = IH2.lower, ymax = IH2.upper), width= widtherr ,  color =col3, size=sz3) +
  geom_errorbar(aes(ymin = IH3.lower, ymax = IH3.upper), width= widtherr ,  color =col3,  size=sz3) +
  
  theme(text=element_text(size=12,  family="serif")) +
  labs(fill = "scenario") +
  ylab("FNR") +
  xlab("Threshold") +
  
  scale_color_manual(name = "Dispersal",
                     values = c("col1" = "gray85", "col2" = "gray50", "col3" = "black"),
                     labels = c("low", "moderate", "high")) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(legend.key=element_blank()) +
  
  scale_shape_manual(name = "Number of populations",
                     values = c("pch1" = 17, "pch2" = 15, "pch3" = 19),
                     labels = c("1","2","3"))

##############################################################################################

ggplot(dfFPR_I, aes(alpha)) + 
  
  geom_line(aes(y = IL1.mean,  color ="col1"), size=sz2) +
  geom_line(aes(y = IL2.mean,  color ="col1"), size=sz2) +
  geom_line(aes(y = IL3.mean,  color ="col1"), size=sz2) +
  #geom_line(aes(y = IM1.mean,  color ="col2"), size=sz2) +
  geom_line(aes(y = IM2.mean,  color ="col2"), size=sz2) +
  geom_line(aes(y = IM3.mean,  color ="col2"), size=sz2) +
  geom_line(aes(y = IH1.mean,  color ="col3"), size=sz2) +
  geom_line(aes(y = IH2.mean,  color ="col3"), size=sz2) +
  geom_line(aes(y = IH3.mean,  color ="col3"), size=sz2) +
  geom_line(aes(y = CL.mean),  color =col1, size=sz2, linetype = "dashed") +
  geom_line(aes(y = CM.mean),  color =col2, size=sz2, linetype = "dashed") +
  geom_line(aes(y = CH.mean),  color =col3, size=sz2, linetype = "dashed") +
  # 
  geom_point(aes(y = IL1.mean, pch = "pch1"),  color =col1, size=sz) +
  geom_point(aes(y = IL2.mean, pch = "pch2"),  color =col1, size=sz) +
  geom_point(aes(y = IL3.mean, pch = "pch3"),  color =col1, size=sz) +
  #geom_point(aes(y = IM1.mean, pch = "pch1"),  color =col2, size=sz) +
  geom_point(aes(y = IM2.mean, pch = "pch2"),  color =col2, size=sz) +
  geom_point(aes(y = IM3.mean, pch = "pch3"),  color =col2, size=sz) +
  geom_point(aes(y = IH1.mean, pch = "pch1"),  color =col3, size=sz) +
  geom_point(aes(y = IH2.mean, pch = "pch2"),  color =col3, size=sz) +
  geom_point(aes(y = IH3.mean, pch = "pch3"),  color =col3, size=sz) +
  geom_point(aes(y = CL.mean,  pch = "pch4"),  color =col1, size=sz) + # fill=col1,
  geom_point(aes(y = CM.mean,  pch = "pch4"),  color =col2, size=sz) + # fill=col2,
  geom_point(aes(y = CH.mean,  pch = "pch4"),  color =col3, size=sz) + # fill=col3,
  # 
  geom_errorbar(aes(ymin = IL1.lower, ymax = IL1.upper), width= widtherr ,  color = col1, size=sz3) +
  geom_errorbar(aes(ymin = IL2.lower, ymax = IL2.upper), width= widtherr ,  color =col1, size=sz3) +
  geom_errorbar(aes(ymin = IL3.lower, ymax = IL3.upper), width= widtherr ,  color =col1, size=sz3) +
  #geom_errorbar(aes(ymin = IM1.lower, ymax = IM1.upper), width= widtherr ,  color =col2, size=sz3) +
  geom_errorbar(aes(ymin = IM2.lower, ymax = IM2.upper), width= widtherr ,  color =col2, size=sz3) +
  geom_errorbar(aes(ymin = IM3.lower, ymax = IM3.upper), width= widtherr ,  color =col2, size=sz3) +
  geom_errorbar(aes(ymin = IH1.lower, ymax = IH1.upper), width= widtherr ,  color =col3, size=sz3) +
  geom_errorbar(aes(ymin = IH2.lower, ymax = IH2.upper), width= widtherr ,  color =col3, size=sz3) +
  geom_errorbar(aes(ymin = IH3.lower, ymax = IH3.upper), width= widtherr ,  color =col3,  size=sz3) +
  geom_errorbar(aes(ymin = CL.lower, ymax = CL.upper), width= widtherr ,  color =col1,  size=sz3) +
  geom_errorbar(aes(ymin = CM.lower, ymax = CM.upper), width= widtherr ,  color =col2,  size=sz3) +
  geom_errorbar(aes(ymin = CH.lower, ymax = CH.upper), width= widtherr ,  color =col3,  size=sz3) +
  # 
  theme(text=element_text(size=12,  family="serif")) +
  labs(fill = "scenario") +
  ylab("FPR") +
  xlab("Threshold") +
  
  scale_color_manual(name = "Dispersal",
                     values = c("col1" = "gray85", "col2" = "gray50", "col3" = "black"),
                     labels = c("low", "moderate", "high")) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(legend.key=element_blank()) +
  
  scale_shape_manual(name = "Number of affected populations",
                     values = c("pch1" = 17, "pch2" = 15, "pch3" = 19, "pch4" = 3),
                     labels = c("1","2","3", "0 (with dashed line)"))
##############################################################################################
ggplot(dfFPR_I, aes(alpha)) + 
  
  geom_line(aes(y = IH1.mean,  color ="col3"), size=sz2) +
  geom_line(aes(y = IH2.mean,  color ="col3"), size=sz2) +
  geom_line(aes(y = IH3.mean,  color ="col3"), size=sz2) +
  geom_line(aes(y = CH.mean),  color =col3, size=sz2, linetype = "dashed") +
  
  geom_point(aes(y = IH1.mean, pch = "pch1"),  color =col3, size=sz) +
  geom_point(aes(y = IH2.mean, pch = "pch2"),  color =col3, size=sz) +
  geom_point(aes(y = IH3.mean, pch = "pch3"),  color =col3, size=sz) +
  #geom_point(aes(y = CH.mean,  pch = "pch4"),  color =col3, size=sz) + # fill=col3,
  
  geom_errorbar(aes(ymin = IH1.lower, ymax = IH1.upper), width= widtherr ,  color =col3, size=sz3) +
  geom_errorbar(aes(ymin = IH2.lower, ymax = IH2.upper), width= widtherr ,  color =col3, size=sz3) +
  geom_errorbar(aes(ymin = IH3.lower, ymax = IH3.upper), width= widtherr ,  color =col3,  size=sz3) +
  #geom_errorbar(aes(ymin = CH.lower, ymax = CH.upper), width= widtherr ,  color =col3,  size=sz3) +
  
  theme(text=element_text(size=12,  family="serif")) +
  labs(fill = "scenario") +
  ylab("FPR") +
  xlab("Threshold") +
  
  scale_color_manual(name = "Dispersal",
                     values = c( "col3" = "black"),
                     labels = c("high")) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(legend.key=element_blank()) +
  
  scale_shape_manual(name = "Number of affected populations",
                     values = c("pch1" = 17, "pch2" = 15, "pch3" = 19, "pch4" = 3),
                     labels = c("1","2","3", "0 (with dashed line)"))

# dfFPR_I_TF <- cbind.data.frame(alpha, ifelse(dfFPR_I[,2:10]>dfFPR_I[,1], FALSE, TRUE))
# colnames(dfFPR_I_TF) <- colnames(dfFPR_I)
# dfFPR_I_TF
# 
# dfFPR_I <- data.frame(alpha,
#                       IL1=sum_perf(IL1, "FPR"),
#                       IL2=sum_perf(IL2, "FPR"),
#                       IL3=sum_perf(IL3, "FPR"),
#                       IM1=sum_perf(IM1, "FPR"),
#                       IM2=sum_perf(IM2, "FPR"),
#                       IM3=sum_perf(IM3, "FPR"),
#                       IH1=sum_perf(IH1, "FPR"),
#                       IH2=sum_perf(IH2, "FPR"),
#                       IH3=sum_perf(IH3, "FPR"),
#                       CL=sum_perf(CL, "FPR", TRUE),
#                       CM=sum_perf(CM, "FPR", TRUE),
#                       CH=sum_perf(CH, "FPR", TRUE)) #CHANGER

# dfcontrol <- data.frame(alpha,
#                         CL=sum_perf(CL, "FPR", TRUE),
#                         CM=sum_perf(CM, "FPR", TRUE),
#                         CH=sum_perf(CH, "FPR", TRUE))