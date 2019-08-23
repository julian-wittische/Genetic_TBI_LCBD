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
