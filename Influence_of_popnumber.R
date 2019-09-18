########

one <- mapply(rbind, bl1_combine_positions, bm1_combine_positions, bh1_combine_positions,
            il1_combine_positions, im1_combine_positions, ih1_combine_positions, SIMPLIFY=FALSE)

two <- mapply(rbind, bl2_combine_positions, bm2_combine_positions, bh2_combine_positions,
            il2_combine_positions, im2_combine_positions, ih2_combine_positions, SIMPLIFY=FALSE)

three <- mapply(rbind, bl3_combine_positions, bm3_combine_positions, bh3_combine_positions,
            il3_combine_positions, im3_combine_positions, ih3_combine_positions, SIMPLIFY=FALSE)

cont <- mapply(rbind, cl, cm, ch, SIMPLIFY=FALSE)

plot1 <- PlotRates(one, cont, ylab = "Rate")
plot2 <- PlotRates(two, cont, xlab = "Threshold (p.TBI)")
plot3 <- PlotRates(three, cont)
plotlegend <- PlotRates(L, cl, leg = "right")

legend <- get_legend(plotlegend)

pgrid <- plot_grid(plot1, plot2, plot3, ncol = 3)
ppop <- plot_grid(pgrid, legend, rel_widths = c(1, .1))
ppop

sapply(one, function(x) mean(x$FNR))[9]
sapply(two, function(x) mean(x$FNR))[9]
sapply(three, function(x) mean(x$FNR))[9]

sapply(one, function(x) mean(x$FPR))[9]
sapply(two, function(x) mean(x$FPR))[9]
sapply(three, function(x) mean(x$FPR))[9]
####
sapply(one, function(x) mean(x$FPR))[10]
sapply(two, function(x) mean(x$FPR))[10]
sapply(three, function(x) mean(x$FPR))[10]

sapply(one, function(x) mean(x$FNR))[10]
sapply(two, function(x) mean(x$FNR))[10]
sapply(three, function(x) mean(x$FNR))[10]

sapply(one, function(x) mean(x$FNR))[11]
sapply(two, function(x) mean(x$FNR))[11]
sapply(three, function(x) mean(x$FNR))[11]

sapply(one, function(x) mean(x$FPR))[11]
sapply(two, function(x) mean(x$FPR))[11]
sapply(three, function(x) mean(x$FPR))[11]
####
alpha <- c(0.0001, 0.00025, 0.0005, 0.00075, 0.001, 0.0025, 0.005, 0.0075, 0.01, 0.025, 0.05, 0.075, 0.1)
mone <- sapply(one, function(x) mean(x$FPR))
mtwo <- sapply(two, function(x) mean(x$FPR))
mthree <- sapply(three, function(x) mean(x$FPR))

plot(alpha, mone, col = "red", pch=19, ylim = c(0,0.052))
points(alpha, mtwo, col = "blue", pch=19)
points(alpha, mthree, col = "green", pch=19)
