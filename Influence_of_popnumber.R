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
