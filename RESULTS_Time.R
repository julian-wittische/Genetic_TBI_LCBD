setwd("E:/Globus/RData2")
# setwd("C:/Users/Field/Documents/Glob")

library(Rmisc)

alpha <- c(0.001, 0.0025, 0.005, 0.0075, 0.01, 0.025, 0.05, 0.075, 0.1)

##############################################################################################
dfFNR_before <- data.frame(years=-9:0,
                           IL1=before_FNR_L1,
                           IL2=before_FNR_L2,
                           IL3=before_FNR_L3,
                           IM1=before_FNR_M1,
                           IM2=before_FNR_M2,
                           IM3=before_FNR_M3,
                           IH1=before_FNR_H1,
                           IH2=before_FNR_H2,
                           IH3=before_FNR_H3)

dfFPR_before <- data.frame(years=-9:0,
                           IL1=before_FPR_L1,
                           IL2=before_FPR_L2,
                           IL3=before_FPR_L3,
                           IM1=before_FPR_M1,
                           IM2=before_FPR_M2,
                           IM3=before_FPR_M3,
                           IH1=before_FPR_H1,
                           IH2=before_FPR_H2,
                           IH3=before_FPR_H3)

dfFNR_after <- data.frame(years=0:9,
                          IL1=after_FNR_L1,
                          IL2=after_FNR_L2,
                          IL3=after_FNR_L3,
                          IM1=after_FNR_M1,
                          IM2=after_FNR_M2,
                          IM3=after_FNR_M3,
                          IH1=after_FNR_H1,
                          IH2=after_FNR_H2,
                          IH3=after_FNR_H3)

dfFPR_after <- data.frame(years=0:9,
                          IL1=after_FPR_L1,
                          IL2=after_FPR_L2,
                          IL3=after_FPR_L3,
                          IM1=after_FPR_M1,
                          IM2=after_FPR_M2,
                          IM3=after_FPR_M3,
                          IH1=after_FPR_H1,
                          IH2=after_FPR_H2,
                          IH3=after_FPR_H3)

dfFPR_I_time <- rbind(dfFPR_before, dfFPR_after[-1,])
dfFNR_I_time <- rbind(dfFNR_before, dfFNR_after[-1,])

sz <- 3
sz2 <- 1
sz3 <-0.5
widtherr <- 0.25
col1 <- "gray85"
col2 <- "gray50"
col3 <- "black"

##############################################################################################

ggplot(dfFNR_I_time, aes(years)) + 
  
  geom_line(aes(y = IL1.mean,  color ="col1"), size=sz2) +
  geom_line(aes(y = IL2.mean,  color ="col1"), size=sz2) +
  geom_line(aes(y = IL3.mean,  color ="col1"), size=sz2) +
  geom_line(aes(y = IM1.mean,  color ="col2"), size=sz2) +
  geom_line(aes(y = IM2.mean,  color ="col2"), size=sz2) +
  geom_line(aes(y = IM3.mean,  color ="col2"), size=sz2) +
  geom_line(aes(y = IH1.mean,  color ="col3"), size=sz2) +
  geom_line(aes(y = IH2.mean,  color ="col3"), size=sz2) +
  geom_line(aes(y = IH3.mean,  color ="col3"), size=sz2) +
  
  geom_point(aes(y = IL1.mean, pch = "pch1"),  color =col1, size=sz) +
  geom_point(aes(y = IL2.mean, pch = "pch2"),  color =col1, size=sz) +
  geom_point(aes(y = IL3.mean, pch = "pch3"),  color =col1, size=sz) +
  geom_point(aes(y = IM1.mean, pch = "pch1"),  color =col2, size=sz) +
  geom_point(aes(y = IM2.mean, pch = "pch2"),  color =col2, size=sz) +
  geom_point(aes(y = IM3.mean, pch = "pch3"),  color =col2, size=sz) +
  geom_point(aes(y = IH1.mean, pch = "pch1"),  color =col3, size=sz) +
  geom_point(aes(y = IH2.mean, pch = "pch2"),  color =col3, size=sz) +
  geom_point(aes(y = IH3.mean, pch = "pch3"),  color =col3, size=sz) +
  
  geom_errorbar(aes(ymin = IL1.lower, ymax = IL1.upper), width= widtherr ,  color = col1, size=sz3) +
  geom_errorbar(aes(ymin = IL2.lower, ymax = IL2.upper), width= widtherr ,  color =col1, size=sz3) +
  geom_errorbar(aes(ymin = IL3.lower, ymax = IL3.upper), width= widtherr ,  color =col1, size=sz3) +
  geom_errorbar(aes(ymin = IM1.lower, ymax = IM1.upper), width= widtherr ,  color =col2, size=sz3) +
  geom_errorbar(aes(ymin = IM2.lower, ymax = IM2.upper), width= widtherr ,  color =col2, size=sz3) +
  geom_errorbar(aes(ymin = IM3.lower, ymax = IM3.upper), width= widtherr ,  color =col2, size=sz3) +
  geom_errorbar(aes(ymin = IH1.lower, ymax = IH1.upper), width= widtherr ,  color =col3, size=sz3) +
  geom_errorbar(aes(ymin = IH2.lower, ymax = IH2.upper), width= widtherr ,  color =col3, size=sz3) +
  geom_errorbar(aes(ymin = IH3.lower, ymax = IH3.upper), width= widtherr ,  color =col3,  size=sz3) +
  
  theme(text=element_text(size=12,  family="serif")) +
  labs(fill = "scenario") +
  ylab("FNR") +
  xlab("Time lag relative to the sampling closest to the event (generations)") +
  
  scale_color_manual(name = "Dispersal",
                     values = c("col1" = "gray85", "col2" = "gray50", "col3" = "black"),
                     labels = c("low", "moderate", "high")) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(legend.key=element_blank()) +
  
  scale_shape_manual(name = "Number of populations",
                     values = c("pch1" = 17, "pch2" = 15, "pch3" = 19),
                     labels = c("1","2","3")) +
  geom_vline(xintercept=0, linetype="dashed")
##############################################################################################

ggplot(dfFPR_I_time, aes(years)) + 
  
  geom_line(aes(y = IL1.mean,  color ="col1"), size=sz2) +
  geom_line(aes(y = IL2.mean,  color ="col1"), size=sz2) +
  geom_line(aes(y = IL3.mean,  color ="col1"), size=sz2) +
  geom_line(aes(y = IM1.mean,  color ="col2"), size=sz2) +
  geom_line(aes(y = IM2.mean,  color ="col2"), size=sz2) +
  geom_line(aes(y = IM3.mean,  color ="col2"), size=sz2) +
  geom_line(aes(y = IH1.mean,  color ="col3"), size=sz2) +
  geom_line(aes(y = IH2.mean,  color ="col3"), size=sz2) +
  geom_line(aes(y = IH3.mean,  color ="col3"), size=sz2) +
  
  geom_point(aes(y = IL1.mean, pch = "pch1"),  color =col1, size=sz) +
  geom_point(aes(y = IL2.mean, pch = "pch2"),  color =col1, size=sz) +
  geom_point(aes(y = IL3.mean, pch = "pch3"),  color =col1, size=sz) +
  geom_point(aes(y = IM1.mean, pch = "pch1"),  color =col2, size=sz) +
  geom_point(aes(y = IM2.mean, pch = "pch2"),  color =col2, size=sz) +
  geom_point(aes(y = IM3.mean, pch = "pch3"),  color =col2, size=sz) +
  geom_point(aes(y = IH1.mean, pch = "pch1"),  color =col3, size=sz) +
  geom_point(aes(y = IH2.mean, pch = "pch2"),  color =col3, size=sz) +
  geom_point(aes(y = IH3.mean, pch = "pch3"),  color =col3, size=sz) +
  
  geom_errorbar(aes(ymin = IL1.lower, ymax = IL1.upper), width= widtherr ,  color = col1, size=sz3) +
  geom_errorbar(aes(ymin = IL2.lower, ymax = IL2.upper), width= widtherr ,  color =col1, size=sz3) +
  geom_errorbar(aes(ymin = IL3.lower, ymax = IL3.upper), width= widtherr ,  color =col1, size=sz3) +
  geom_errorbar(aes(ymin = IM1.lower, ymax = IM1.upper), width= widtherr ,  color =col2, size=sz3) +
  geom_errorbar(aes(ymin = IM2.lower, ymax = IM2.upper), width= widtherr ,  color =col2, size=sz3) +
  geom_errorbar(aes(ymin = IM3.lower, ymax = IM3.upper), width= widtherr ,  color =col2, size=sz3) +
  geom_errorbar(aes(ymin = IH1.lower, ymax = IH1.upper), width= widtherr ,  color =col3, size=sz3) +
  geom_errorbar(aes(ymin = IH2.lower, ymax = IH2.upper), width= widtherr ,  color =col3, size=sz3) +
  geom_errorbar(aes(ymin = IH3.lower, ymax = IH3.upper), width= widtherr ,  color =col3,  size=sz3) +
  
  theme(text=element_text(size=12,  family="serif")) +
  labs(fill = "scenario") +
  ylab("FPR") +
  xlab("Time lag relative to the sampling closest to the event (generations)") +
  
  scale_color_manual(name = "Dispersal",
                     values = c("col1" = "gray85", "col2" = "gray50", "col3" = "black"),
                     labels = c("low", "moderate", "high")) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(legend.key=element_blank()) +
  
  scale_shape_manual(name = "Number of populations",
                     values = c("pch1" = 17, "pch2" = 15, "pch3" = 19),
                     labels = c("1","2","3")) +
  geom_vline(xintercept=0, linetype="dashed")
##############################################################################################