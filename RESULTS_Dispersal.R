setwd("E:/Globus/RData2")

alpha <- c(0.001, 0.0025, 0.005, 0.0075, 0.01, 0.025, 0.05, 0.075, 0.1)

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

dfcontrol <- data.frame(alpha,
                        CL=sum_perf(load_perf(CL, "control"), "FPR", TRUE),
                        CM=sum_perf(load_perf(CM, "control"), "FPR", TRUE),
                        CH=sum_perf(load_perf(CH, "control"), "FPR", TRUE))



L005 <- dfFNR_I[7,2:4]
M005 <- dfFNR_I[7,5:7]
H005 <- dfFNR_I[7,8:10]

rowMeans(L005)
rowMeans(M005)
rowMeans(H005)

L005 <- dfFPR_I[7,2:4]
M005 <- dfFPR_I[7,5:7]
H005 <- dfFPR_I[7,8:10]

rowMeans(L005)
rowMeans(M005)
rowMeans(H005)

dfFPR_I_TF <- cbind.data.frame(alpha, ifelse(dfFPR_I[,2:10]>dfFPR_I[,1], FALSE, TRUE))
colnames(dfFPR_I_TF) <- colnames(dfFPR_I)
dfFPR_I_TF
###########################################################################

sz <- 3
sz2 <- 1
col1 <- "gray85"
col2 <- "gray50"
col3 <- "black"
col4 <- "red"

dfFNR_melt <- melt(dfFNR ,  id.vars = 'alpha', variable.name = 'scenario')
dfFNR_melt_cb <- cbind(dfFNR_melt, disp=rep("L","M","H", each = 27), pops=rep)

#################################################################################

ggplot(dfFPR_I, aes(alpha, y = value)) + 
  geom_point(aes(y = IL1),  color = col1, pch=17, size=sz) +
  geom_point(aes(y = IL2),  color =col1, pch=15, size=sz) +
  geom_point(aes(y = IL3),  color =col1, pch=19, size=sz) +
  geom_point(aes(y = IM1),  color =col2, pch=17, size=sz) +
  geom_point(aes(y = IM2),  color =col2, pch=15, size=sz) +
  geom_point(aes(y = IM3),  color =col2, pch=19, size=sz) +
  geom_point(aes(y = IH1),  color =col3, pch=17, size=sz) +
  geom_point(aes(y = IH2),  color =col3, pch=15, size=sz) +
  geom_point(aes(y = IH3),  color =col3, pch=19, size=sz) +
  geom_point(aes(y = CL),  color =col4, fill=col1, pch=25, size=sz) +
  geom_point(aes(y = CM),  color =col4, fill=col2, pch=25, size=sz) +
  geom_point(aes(y = CH),  color =col4, fill=col3, pch=25, size=sz) +
  geom_line(aes(y = IL1),  color =col1, size=sz2) +
  geom_line(aes(y = IL2),  color =col1, size=sz2) +
  geom_line(aes(y = IL3),  color =col1, size=sz2) +
  geom_line(aes(y = IM1),  color =col2, size=sz2) +
  geom_line(aes(y = IM2),  color =col2, size=sz2) +
  geom_line(aes(y = IM3),  color =col2, size=sz2) +
  geom_line(aes(y = IH1),  color =col3, size=sz2) +
  geom_line(aes(y = IH2),  color =col3, size=sz2) +
  geom_line(aes(y = IH3),  color =col3, size=sz2) +
  geom_line(aes(y = CL),  color =col4, size=sz2) +
  geom_line(aes(y = CM),  color =col4, size=sz2) +
  geom_line(aes(y = CH),  color =col4, size=sz2) +
  labs(fill = "scenario") +
  ylab("FPR") +
  xlab("Threshold") +
  theme(text=element_text(size=12,  family="serif"))

ggplot(dfFNR_I, aes(alpha, y = value)) + 
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
  xlab("Threshold") +
  theme(text=element_text(size=12,  family="serif"))