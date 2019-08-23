
legend <- get_legend(bl1_pl)
pgrid <- plot_grid(bl1_pl, bl2_pl, bl3_pl, bm1_pl, bm2_pl, bm3_pl, bh1_pl, bh2_pl, bh3_pl, ncol = 3)
p <- plot_grid(pgrid, legend, ncol = 3)
#, rel_widths = c(1, .1)

##### Test

# Number of populations x Dispersal for Bottleneck

bl1_pl <- PlotRates(bl1_combine_positions, cl)
bl2_pl <- PlotRates(bl2_combine_positions, cl)
bl3_pl <- PlotRates(bl3_combine_positions, cl)

bm1_pl <- PlotRates(bm1_combine_positions, cm)
bm2_pl <- PlotRates(bm2_combine_positions, cm)
bm3_pl <- PlotRates(bm3_combine_positions, cm)

bh1_pl <- PlotRates(bh1_combine_positions, ch)
bh2_pl <- PlotRates(bh2_combine_positions, ch)
bh3_pl <- PlotRates(bh3_combine_positions, ch)

plot_grid(bl1_pl, bl2_pl, bl3_pl, bm1_pl, bm2_pl, bm3_pl, bh1_pl, bh2_pl, bh3_pl)

# 

PlotRates(bl1_combine_positions, cl)
PlotRates(bm1_combine_positions, cm)
PlotRates(bh1_combine_positions, ch)


