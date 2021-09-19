setwd("E:/Globus/RData2")

alpha <- c(0.0001, 0.001, 0.005, 0.01, 0.015, 0.020, 0.025, 0.030, 0.035, 0.040, 0.045,
           0.050, 0.055, 0.060, 0.065, 0.070, 0.075, 0.080, 0.085, 0.090, 0.095, 0.1)

dfFPR_I <- data.frame(alpha,
                      IL1=sum_perf(IL1, "FPR"),
                      IL2=sum_perf(IL2, "FPR"),
                      IL3=sum_perf(IL3, "FPR"),
                      IM1=sum_perf(IM1, "FPR"),
                      IM2=sum_perf(IM2, "FPR"),
                      IM3=sum_perf(IM3, "FPR"),
                      IH1=sum_perf(IH1, "FPR"),
                      IH2=sum_perf(IH2, "FPR"),
                      IH3=sum_perf(IH3, "FPR"))

dfFPR_I_TF <- cbind.data.frame(alpha, ifelse(dfFPR_I[, seq(3, 28, 3)]>dfFPR_I[,1], FALSE, TRUE))
colnames(dfFPR_I_TF) <- colnames(dfFPR_I)
dfFPR_I_TF

dfFPR_I <- data.frame(alpha,
                      IL1=sum_perf(IL1, "FPR"),
                      IL2=sum_perf(IL2, "FPR"),
                      IL3=sum_perf(IL3, "FPR"),
                      IM1=sum_perf(IM1, "FPR"),
                      IM2=sum_perf(IM2, "FPR"),
                      IM3=sum_perf(IM3, "FPR"),
                      IH1=sum_perf(IH1, "FPR"),
                      IH2=sum_perf(IH2, "FPR"),
                      IH3=sum_perf(IH3, "FPR"),
                      CL=sum_perf(CL, "FPR", TRUE),
                      CM=sum_perf(CM, "FPR", TRUE),
                      CH=sum_perf(CH, "FPR", TRUE))
dfFPR_I <- dfFPR_I[-1,]
  
dfFNR_I <- data.frame(alpha,
                      IL1=sum_perf(IL1, "FNR"),
                      IL2=sum_perf(IL2, "FNR"),
                      IL3=sum_perf(IL3, "FNR"),
                      IM1=sum_perf(IM1, "FNR"),
                      IM2=sum_perf(IM2, "FNR"),
                      IM3=sum_perf(IM3, "FNR"),
                      IH1=sum_perf(IH1, "FNR"),
                      IH2=sum_perf(IH2, "FNR"),
                      IH3=sum_perf(IH3, "FNR"))
dfFNR_I <- dfFNR_I[-1,]

# dfcontrol <- data.frame(alpha,
#                         CL=sum_perf(CL, "FPR", TRUE),
#                         CM=sum_perf(CM, "FPR", TRUE),
#                         CH=sum_perf(CH, "FPR", TRUE))

# L005N <- dfFNR_I[12, c(3, 6, 9)]
# M005N <- dfFNR_I[12, c(12, 15, 18)]
# H005N <- dfFNR_I[12, c(21, 24, 27)]
# L005Nlow <- dfFNR_I[12, c(4, 7, 10)]
# M005Nlow <- dfFNR_I[12, c(13, 16, 19)]
# H005Nlow <- dfFNR_I[12, c(22, 25, 28)]
# L005Nup <- dfFNR_I[12, c(2, 5, 8)]
# M005Nup <- dfFNR_I[12, c(11, 14, 17)]
# H005Nup <- dfFNR_I[12, c(20, 23, 26)]
# rowMeans(L005N)
# rowMeans(M005N)
# rowMeans(H005N)
# rowMeans(L005Nlow)
# rowMeans(M005Nlow)
# rowMeans(H005Nlow)
# rowMeans(L005Nup)
# rowMeans(M005Nup)
# rowMeans(H005Nup)
# 
# L005P <- dfFPR_I[7, c(3, 6, 9)]
# M005P <- dfFPR_I[7, c(12, 15, 18)]
# H005P <- dfFPR_I[7, c(21, 24, 27)]
# L005P
# M005P
# H005P
# rowMeans(L005P)
# rowMeans(M005P)
# rowMeans(H005P)

##############################################################################################

sz <- 3
sz2 <- 1
sz3 <-0.5
widtherr <- 0.001
col1 <- "cyan"
col2 <- "red"
col3 <- "black"

# dfFNR_melt <- melt(dfFNR ,  id.vars = 'alpha', variable.name = 'scenario')
# dfFNR_melt_cb <- cbind(dfFNR_melt, disp=rep("L","M","H", each = 27), pops=rep)

##############################################################################################

ggplot(dfFNR_I, aes(alpha)) + 
  
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
  xlab("Threshold") +

  scale_color_manual(name = "Dispersal",
                     values = c("col1" = "lightblue2", "col2" = "dodgerblue", "col3" = "blue4"),
                     labels = c("low (L)", "moderate (M)", "high (H)")) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(legend.key=element_blank()) +
  
  scale_shape_manual(name = "Number of pop.",
                     values = c("pch1" = 17, "pch2" = 15, "pch3" = 19),
                     labels = c("1","2","3"))

# ##############################################################################################
# # To zoom in beyond 1 values
# dfFNR_I_no1s <- dfFNR_I[2:21,]
# ggplot(dfFNR_I_no1s, aes(alpha)) + 
#   
#   geom_line(aes(y = IL1.mean,  color ="col1"), size=sz2) +
#   geom_line(aes(y = IL2.mean,  color ="col1"), size=sz2) +
#   geom_line(aes(y = IL3.mean,  color ="col1"), size=sz2) +
#   geom_line(aes(y = IM1.mean,  color ="col2"), size=sz2) +
#   geom_line(aes(y = IM2.mean,  color ="col2"), size=sz2) +
#   geom_line(aes(y = IM3.mean,  color ="col2"), size=sz2) +
#   geom_line(aes(y = IH1.mean,  color ="col3"), size=sz2) +
#   geom_line(aes(y = IH2.mean,  color ="col3"), size=sz2) +
#   geom_line(aes(y = IH3.mean,  color ="col3"), size=sz2) +
#   
#   geom_point(aes(y = IL1.mean, pch = "pch1"),  color =col1, size=sz) +
#   geom_point(aes(y = IL2.mean, pch = "pch2"),  color =col1, size=sz) +
#   geom_point(aes(y = IL3.mean, pch = "pch3"),  color =col1, size=sz) +
#   geom_point(aes(y = IM1.mean, pch = "pch1"),  color =col2, size=sz) +
#   geom_point(aes(y = IM2.mean, pch = "pch2"),  color =col2, size=sz) +
#   geom_point(aes(y = IM3.mean, pch = "pch3"),  color =col2, size=sz) +
#   geom_point(aes(y = IH1.mean, pch = "pch1"),  color =col3, size=sz) +
#   geom_point(aes(y = IH2.mean, pch = "pch2"),  color =col3, size=sz) +
#   geom_point(aes(y = IH3.mean, pch = "pch3"),  color =col3, size=sz) +
#   
#   geom_errorbar(aes(ymin = IL1.lower, ymax = IL1.upper), width= widtherr ,  color = col1, size=sz3) +
#   geom_errorbar(aes(ymin = IL2.lower, ymax = IL2.upper), width= widtherr ,  color =col1, size=sz3) +
#   geom_errorbar(aes(ymin = IL3.lower, ymax = IL3.upper), width= widtherr ,  color =col1, size=sz3) +
#   geom_errorbar(aes(ymin = IM1.lower, ymax = IM1.upper), width= widtherr ,  color =col2, size=sz3) +
#   geom_errorbar(aes(ymin = IM2.lower, ymax = IM2.upper), width= widtherr ,  color =col2, size=sz3) +
#   geom_errorbar(aes(ymin = IM3.lower, ymax = IM3.upper), width= widtherr ,  color =col2, size=sz3) +
#   geom_errorbar(aes(ymin = IH1.lower, ymax = IH1.upper), width= widtherr ,  color =col3, size=sz3) +
#   geom_errorbar(aes(ymin = IH2.lower, ymax = IH2.upper), width= widtherr ,  color =col3, size=sz3) +
#   geom_errorbar(aes(ymin = IH3.lower, ymax = IH3.upper), width= widtherr ,  color =col3,  size=sz3) +
#   
#   theme(text=element_text(size=12,  family="serif")) +
#   labs(fill = "scenario") +
#   ylab("FNR") +
#   xlab("Threshold") +
#   
#   scale_color_manual(name = "Dispersal",
#                      values = c("col1" = "cyan", "col2" = "red", "col3" = "black"),
#                      labels = c("low (L)", "moderate (M)", "high (H)")) +
#   
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   
#   theme(legend.key=element_blank()) +
#   
#   scale_shape_manual(name = "Number of pop.",
#                      values = c("pch1" = 17, "pch2" = 15, "pch3" = 19),
#                      labels = c("1","2","3"))
# 
# ##############################################################################################

ggplot(dfFPR_I, aes(alpha)) + 
  
  geom_line(aes(y = IL1.mean,  color ="col1"), size=sz2) +
  geom_line(aes(y = IL2.mean,  color ="col1"), size=sz2) +
  geom_line(aes(y = IL3.mean,  color ="col1"), size=sz2) +
  geom_line(aes(y = IM1.mean,  color ="col2"), size=sz2) +
  geom_line(aes(y = IM2.mean,  color ="col2"), size=sz2) +
  geom_line(aes(y = IM3.mean,  color ="col2"), size=sz2) +
  geom_line(aes(y = IH1.mean,  color ="col3"), size=sz2) +
  geom_line(aes(y = IH2.mean,  color ="col3"), size=sz2) +
  geom_line(aes(y = IH3.mean,  color ="col3"), size=sz2) +
  geom_line(aes(y = CL.mean),  color =col1, size=sz2, linetype = "dashed") +
  geom_line(aes(y = CM.mean),  color =col2, size=sz2, linetype = "dashed") +
  geom_line(aes(y = CH.mean),  color =col3, size=sz2, linetype = "dashed") +
  
  geom_point(aes(y = IL1.mean, pch = "pch1"),  color =col1, size=sz) +
  geom_point(aes(y = IL2.mean, pch = "pch2"),  color =col1, size=sz) +
  geom_point(aes(y = IL3.mean, pch = "pch3"),  color =col1, size=sz) +
  geom_point(aes(y = IM1.mean, pch = "pch1"),  color =col2, size=sz) +
  geom_point(aes(y = IM2.mean, pch = "pch2"),  color =col2, size=sz) +
  geom_point(aes(y = IM3.mean, pch = "pch3"),  color =col2, size=sz) +
  geom_point(aes(y = IH1.mean, pch = "pch1"),  color =col3, size=sz) +
  geom_point(aes(y = IH2.mean, pch = "pch2"),  color =col3, size=sz) +
  geom_point(aes(y = IH3.mean, pch = "pch3"),  color =col3, size=sz) +
  geom_point(aes(y = CL.mean,  pch = "pch4"),  color =col1, size=sz) + # fill=col1,
  geom_point(aes(y = CM.mean,  pch = "pch4"),  color =col2, size=sz) + # fill=col2,
  geom_point(aes(y = CH.mean,  pch = "pch4"),  color =col3, size=sz) + # fill=col3,
  
  geom_errorbar(aes(ymin = IL1.lower, ymax = IL1.upper), width= widtherr ,  color = col1, size=sz3) +
  geom_errorbar(aes(ymin = IL2.lower, ymax = IL2.upper), width= widtherr ,  color =col1, size=sz3) +
  geom_errorbar(aes(ymin = IL3.lower, ymax = IL3.upper), width= widtherr ,  color =col1, size=sz3) +
  geom_errorbar(aes(ymin = IM1.lower, ymax = IM1.upper), width= widtherr ,  color =col2, size=sz3) +
  geom_errorbar(aes(ymin = IM2.lower, ymax = IM2.upper), width= widtherr ,  color =col2, size=sz3) +
  geom_errorbar(aes(ymin = IM3.lower, ymax = IM3.upper), width= widtherr ,  color =col2, size=sz3) +
  geom_errorbar(aes(ymin = IH1.lower, ymax = IH1.upper), width= widtherr ,  color =col3, size=sz3) +
  geom_errorbar(aes(ymin = IH2.lower, ymax = IH2.upper), width= widtherr ,  color =col3, size=sz3) +
  geom_errorbar(aes(ymin = IH3.lower, ymax = IH3.upper), width= widtherr ,  color =col3,  size=sz3) +
  geom_errorbar(aes(ymin = CL.lower, ymax = CL.upper), width= widtherr ,  color =col1,  size=sz3) +
  geom_errorbar(aes(ymin = CM.lower, ymax = CM.upper), width= widtherr ,  color =col2,  size=sz3) +
  geom_errorbar(aes(ymin = CH.lower, ymax = CH.upper), width= widtherr ,  color =col3,  size=sz3) +
  
  theme(text=element_text(size=12,  family="serif")) +
  labs(fill = "scenario") +
  ylab("FPR") +
  xlab("Threshold") +
  
  scale_color_manual(name = "Dispersal",
                     values = c("col1" = "lightblue2", "col2" = "dodgerblue", "col3" = "blue4"),
                     labels = c("low (L)", "moderate (M)", "high (H)")) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(legend.key=element_blank()) +
  
  scale_shape_manual(name = "Number of pop.",
                     values = c("pch1" = 17, "pch2" = 15, "pch3" = 19, "pch4" = 3),
                     labels = c("1","2","3", "0 (with dashed line)"))
# ##############################################################################################
# ggplot(dfFPR_I, aes(alpha)) + 
#   
#   geom_line(aes(y = IH1.mean,  color ="col3"), size=sz2) +
#   geom_line(aes(y = IH2.mean,  color ="col3"), size=sz2) +
#   geom_line(aes(y = IH3.mean,  color ="col3"), size=sz2) +
#   geom_line(aes(y = CH.mean),  color =col3, size=sz2, linetype = "dashed") +
#   
#   geom_point(aes(y = IH1.mean, pch = "pch1"),  color =col3, size=sz) +
#   geom_point(aes(y = IH2.mean, pch = "pch2"),  color =col3, size=sz) +
#   geom_point(aes(y = IH3.mean, pch = "pch3"),  color =col3, size=sz) +
#   geom_point(aes(y = CH.mean,  pch = "pch4"),  color =col3, size=sz) + # fill=col3,
#   
#   geom_errorbar(aes(ymin = IH1.lower, ymax = IH1.upper), width= widtherr ,  color =col3, size=sz3) +
#   geom_errorbar(aes(ymin = IH2.lower, ymax = IH2.upper), width= widtherr ,  color =col3, size=sz3) +
#   geom_errorbar(aes(ymin = IH3.lower, ymax = IH3.upper), width= widtherr ,  color =col3,  size=sz3) +
#   geom_errorbar(aes(ymin = CH.lower, ymax = CH.upper), width= widtherr ,  color =col3,  size=sz3) +
#   
#   theme(text=element_text(size=12,  family="serif")) +
#   labs(fill = "scenario") +
#   ylab("FPR") +
#   xlab("Threshold") +
#   
#   scale_color_manual(name = "Dispersal",
#                      values = c( "col3" = "black"),
#                      labels = c("high")) +
#   
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   
#   theme(legend.key=element_blank()) +
#   
#   scale_shape_manual(name = "Number of affected populations",
#                      values = c("pch1" = 17, "pch2" = 15, "pch3" = 19, "pch4" = 3),
#                      labels = c("1","2","3", "0 (with dashed line)"))
##############################################################################################
chisq.test(sum_perf(IL1, "FPR")[,2], sum_perf(IL2, "FPR")[,2], simulate.p.value = TRUE)
chisq.test(sum_perf(IL1, "FPR")[,2], sum_perf(IL3, "FPR")[,2], simulate.p.value = TRUE)
chisq.test(sum_perf(IL2, "FPR")[,2], sum_perf(IL3, "FPR")[,2], simulate.p.value = TRUE)

chisq.test(CH[[9]], IH2[[9]]$FPR)
chisq.test(CH[[9]], IH3[[9]]$FPR)

sum_perf(Map(rbind, IL1, IL2, IL3), "FNR")[,2]
sum_perf(Map(rbind, IM1, IM2, IM3), "FNR")[,2]
sum_perf(Map(rbind, IH1, IH2, IH3), "FNR")[,2]

sum_perf(Map(rbind, IL1, IL2, IL3), "FPR")[,2]
sum_perf(Map(rbind, IM1, IM2, IM3), "FPR")[,2]
sum_perf(Map(rbind, IH1, IH2, IH3), "FPR")[,2]

sum_perf(Map(rbind, IL1, IM1, IH1), "FNR")[,2]
sum_perf(Map(rbind, IL2, IM2, IH2), "FNR")[,2]
sum_perf(Map(rbind, IL3, IM3, IH3), "FNR")[,2]

sum_perf(Map(rbind, IL1, IM1, IH1), "FPR")[,2]
sum_perf(Map(rbind, IL2, IM2, IH2), "FPR")[,2]
sum_perf(Map(rbind, IL3, IM3, IH3), "FPR")[,2]

sum_perf(Map(rbind, IL2, IM2, IH2), "FNR")
sum_perf(Map(rbind, IL2, IM2, IH2), "FPR")

sum_perf(Map(rbind, IL3, IM3, IH3), "FNR")
sum_perf(Map(rbind, IL3, IM3, IH3), "FPR")

sum_perf(Map(rbind, IL1, IM1, IH1), "FPR")
