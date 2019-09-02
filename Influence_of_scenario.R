########

B <- mapply(rbind, bl1_combine_positions, bm1_combine_positions, bh1_combine_positions,
                   bl2_combine_positions, bm2_combine_positions, bh2_combine_positions,
                   bl3_combine_positions, bm3_combine_positions, bh3_combine_positions, SIMPLIFY=FALSE)

I <- mapply(rbind, il1_combine_positions, im1_combine_positions, ih1_combine_positions,
                   il2_combine_positions, im2_combine_positions, ih2_combine_positions,
                   il3_combine_positions, im3_combine_positions, ih3_combine_positions, SIMPLIFY=FALSE)

cont <- mapply(rbind, cl, cm, ch, SIMPLIFY=FALSE)

plotB <- PlotRates(B, cont, ylab = "Rate", title = "Bottleneck")
plotI <- PlotRates(I, cont, xlab = "Threshold (p.TBI)", title = "Exogeneous migration")
plotlegend <- PlotRates(L, cont, leg = "right")

legend <- get_legend(plotlegend)

pgrid <- plot_grid(plotB, plotI, ncol = 2)
ptyp <- plot_grid(pgrid, legend, rel_widths = c(1, .1))
ptyp
