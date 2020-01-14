setwd("E:/Globus/RData2")
# setwd("C:/Users/Field/Documents/Glob")

library(Rmisc)

alpha <- c(0.001, 0.0025, 0.005, 0.0075, 0.01, 0.025, 0.05, 0.075, 0.1)

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
####################################################################################################

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
################################################################################################

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
################################################################################################
################################################################################################
################################################################################################

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
###
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
###
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
################################################################################################

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
###
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
###
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
################################################################################################

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
###
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
###
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
################################################################################################

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
widtherr <- 0.001
col1 <- "gray85"
col2 <- "gray50"
col3 <- "black"

#####################################################################################

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