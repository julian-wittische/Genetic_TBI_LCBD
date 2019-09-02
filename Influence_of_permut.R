########

plotp1 <- PlotRates(bl1_combine_positions, cl, ylab = "Rate")
plotp2 <- PlotRates(bl1p2_combine_positions, clp2, xlab = "Threshold (p.TBI)")
plotp3 <- PlotRates(bl1p3_combine_positions, clp3)
plotlegend <- PlotRates(bl1_combine_positions, cl, leg = "right")

legend <- get_legend(plotlegend)

pgrid <- plot_grid(plotp1, plotp2, plotp3, ncol = 3)
pperm <- plot_grid(pgrid, legend, rel_widths = c(1, .1))
pperm

sapply(bl1_combine_positions,function(x) mean(x$FPR, na.rm = TRUE))
sapply(bl1p2_combine_positions,function(x) mean(x$FPR, na.rm = TRUE))
sapply(bl1p3_combine_positions,function(x) mean(x$FPR, na.rm = TRUE))

