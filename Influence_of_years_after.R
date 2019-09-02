########

plot1l1 <- PlotRates(il1_combine_positions, cl, ylab = "Rate")
plot2l1 <- PlotRates(i2l1_combine_positions, cl)
plot3l1 <- PlotRates(i3l1_combine_positions, cl)
plot4l1 <- PlotRates(i4l1_combine_positions, cl)
plot5l1 <- PlotRates(i5l1_combine_positions, cl, xlab = "Threshold (p.TBI)")
plot6l1 <- PlotRates(i6l1_combine_positions, cl)

plot1h3 <- PlotRates(ih3_combine_positions, ch, ylab = "Rate")
plot2h3 <- PlotRates(i2h3_combine_positions, ch)
plot3h3 <- PlotRates(i3h3_combine_positions, ch)
plot4h3 <- PlotRates(i4h3_combine_positions, ch)
plot5h3 <- PlotRates(i5h3_combine_positions, ch, xlab = "Threshold (p.TBI)")
plot6h3 <- PlotRates(i6h3_combine_positions, ch)

plotlegend <- PlotRates(il1_combine_positions, cl, leg = "right")
legend <- get_legend(plotlegend)

pgridl1 <- plot_grid(plot1l1, plot2l1, plot3l1, plot4l1, plot5l1, plot6l1, ncol = 3)
pyl1 <- plot_grid(pgridl1, legend, rel_widths = c(1, .1))
pyl1

pgridh3 <- plot_grid(plot1h3, plot2h3, plot3h3, plot4h3, plot5h3, plot6h3, ncol = 3)
pyh3 <- plot_grid(pgridh3, legend, rel_widths = c(1, .1))
pyh3
