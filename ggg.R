########

L <- mapply(rbind, bl1_combine_positions, bl2_combine_positions, bl3_combine_positions,
            il1_combine_positions, il2_combine_positions, il3_combine_positions, SIMPLIFY=FALSE)

M <- mapply(rbind, bm1_combine_positions, bm2_combine_positions, bm3_combine_positions,
            im1_combine_positions, im2_combine_positions, im3_combine_positions, SIMPLIFY=FALSE)

H <- mapply(rbind, bh1_combine_positions, bh2_combine_positions, bh3_combine_positions,
            ih1_combine_positions, ih2_combine_positions, ih3_combine_positions, SIMPLIFY=FALSE)

plotL <- PlotRates(L, cl, ylab = "Rate")
plotM <- PlotRates(M, cm, xlab = "Threshold (p.TBI)")
plotH <- PlotRates(H, ch)
plotlegend <- PlotRates(L, cl, leg = "right")

legend <- get_legend(plotlegend)

pgrid <- plot_grid(plotL, plotM, plotH, ncol = 3)
pdisp <- plot_grid(pgrid, legend, rel_widths = c(1, .1))
pdisp

sapply(L, function(x) mean(x$FNR))[11]
sapply(M, function(x) mean(x$FNR))[11]
sapply(H, function(x) mean(x$FNR))[11]

sapply(L, function(x) mean(x$FPR))[11]
sapply(M, function(x) mean(x$FPR))[11]
sapply(H, function(x) mean(x$FPR))[11]

alpha <- c(0.0001, 0.00025, 0.0005, 0.00075, 0.001, 0.0025, 0.005, 0.0075, 0.01, 0.025, 0.05, 0.075, 0.1)
ml <- sapply(L, function(x) mean(x$FPR))
mm <- sapply(M, function(x) mean(x$FPR))
mh <- sapply(H, function(x) mean(x$FPR))

plot(alpha, ml, col = "red")




# Lval <- sapply(L, function(x) mean(x$FPR))
# Mval <- sapply(M, function(x) mean(x$FPR))
# Hval <- sapply(H, function(x) mean(x$FPR))
# 
# kwtlist <- list(L[[9]]$FPR, M[[9]]$FPR, H[[9]]$FPR)
# kruskal.test(kwtlist)

# vals9 <- data.frame(FPR=c(L[[9]]$FPR, M[[9]]$FPR, H[[9]]$FPR), FNR=c(L[[9]]$FNR, M[[9]]$FNR, H[[9]]$FNR),
#                     group=rep(factor(c("L", "M", "H")), each=length(L[[9]]$FPR))) 
# summary(aov(FPR~group, data=vals9))
# 
# chisq.test(Lval, Mval, simulate.p.value=TRUE, B=1000000)

# do.call('rbind', L)
# 
# vals9cont <- data.frame(FPR = unlist(L, cm, ch), 
#                         disp = rep(factor(c("L", "M", "H")), each=length(cl[[1]])*13),
#                         threshold = rep(c(0.0001, 0.00025, 0.0005, 0.00075,
#                                           0.001, 0.0025, 0.005, 0.0075,
#                                           0.01, 0.025, 0.05, 0.075,
#                                           0.1), each=180)) 
# anova(lm(FPR~disp, data=vals9cont))

