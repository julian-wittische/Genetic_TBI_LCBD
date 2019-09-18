# clval <- sapply(cl, function(x) mean(x))
# cmval <- sapply(cm, function(x) mean(x))
# chval <- sapply(ch, function(x) mean(x))
# 
# kwtlist <- list(cl[[9]], cm[[9]], ch[[9]])
# kruskal.test(kwtlist)

vals9cont <- data.frame(FPR = unlist(cl, cm, ch), 
                    disp = rep(factor(c("L", "M", "H")), each=length(cl[[1]])*13),
                    threshold = rep(c(0.0001, 0.00025, 0.0005, 0.00075,
                                      0.001, 0.0025, 0.005, 0.0075,
                                      0.01, 0.025, 0.05, 0.075,
                                      0.1), each=180)) 
anova(lm(FPR~disp, data=vals9cont))
