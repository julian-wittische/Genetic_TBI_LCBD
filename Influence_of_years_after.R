# ########
# 
# plot1l1 <- PlotRates(il1_combine_positions, cl, ylab = "Rate")
# plot2l1 <- PlotRates(i2l1_combine_positions, cl)
# plot3l1 <- PlotRates(i3l1_combine_positions, cl)
# plot4l1 <- PlotRates(i4l1_combine_positions, cl, xlab = "Threshold (p.TBI)")
# plot5l1 <- PlotRates(i5l1_combine_positions, cl)
# plot6l1 <- PlotRates(i6l1_combine_positions, cl)
# 
# plot1h3 <- PlotRates(ih3_combine_positions, ch, ylab = "Rate")
# plot2h3 <- PlotRates(i2h3_combine_positions, ch)
# plot3h3 <- PlotRates(i3h3_combine_positions, ch)
# plot4h3 <- PlotRates(i4h3_combine_positions, ch)
# plot5h3 <- PlotRates(i5h3_combine_positions, ch, xlab = "Threshold (p.TBI)")
# plot6h3 <- PlotRates(i6h3_combine_positions, ch)
# 
# plotlegend <- PlotRates(il1_combine_positions, cl, leg = "right")
# legend <- get_legend(plotlegend)
# 
# pgridl1 <- plot_grid(plot1l1, plot2l1, plot3l1, plot4l1, plot5l1, plot6l1, ncol = 3)
# pyl1 <- plot_grid(pgridl1, legend, rel_widths = c(1, .1))
# pyl1
# 
# pgridh3 <- plot_grid(plot1h3, plot2h3, plot3h3, plot4h3, plot5h3, plot6h3, ncol = 3)
# pyh3 <- plot_grid(pgridh3, legend, rel_widths = c(1, .1))
# pyh3

###########################################################################################################
CI_TBI <- function(x,threshold_number, FPR_or_FNR){
  sapply(x, function(x) CI(x[,paste(FPR_or_FNR)]))[,threshold_number]
}
after_FPR_L1 <- rbind(CI_TBI(il1_combine_positions, 11, "FPR"),
                  CI_TBI(i2l1_combine_positions, 11, "FPR"),   
                  CI_TBI(i3l1_combine_positions, 11, "FPR"),
                  CI_TBI(i4l1_combine_positions, 11, "FPR"),
                  CI_TBI(i5l1_combine_positions, 11, "FPR"))

after_FNR_L1 <- rbind(CI_TBI(il1_combine_positions, 11, "FNR"),
                   CI_TBI(i2l1_combine_positions, 11, "FNR"),
                   CI_TBI(i3l1_combine_positions, 11, "FNR"),
                   CI_TBI(i4l1_combine_positions, 11, "FNR"),
                   CI_TBI(i5l1_combine_positions, 11, "FNR"))

after_FPR_H3 <- rbind(CI_TBI(ih3_combine_positions, 11, "FPR"),
                      CI_TBI(i2h3_combine_positions, 11, "FPR"),   
                      CI_TBI(i3h3_combine_positions, 11, "FPR"),
                      CI_TBI(i4h3_combine_positions, 11, "FPR"),
                      CI_TBI(i5h3_combine_positions, 11, "FPR"))

after_FNR_H3 <- rbind(CI_TBI(ih3_combine_positions, 11, "FNR"),
                      CI_TBI(i2h3_combine_positions, 11, "FNR"),   
                      CI_TBI(i3h3_combine_positions, 11, "FNR"),
                      CI_TBI(i4h3_combine_positions, 11, "FNR"),
                      CI_TBI(i5h3_combine_positions, 11, "FNR"))

after_FPR_L1_d <- data.frame(years=1:5,after_FPR_L1)
after_FNR_L1_d <- data.frame(years=1:5,after_FNR_L1)
after_FPR_H3_d <- data.frame(years=1:5,after_FPR_H3)
after_FNR_H3_d <- data.frame(years=1:5,after_FNR_H3)


dffpr <- cbind(rbind(after_FPR_L1_d, after_FPR_H3_d),
               Scenario=factor(rep(c('"easiest"','"hardest"'), each=5)))
FPRa <- ggplot(data=dffpr, aes(linetype=Scenario)) +
  geom_line(data=dffpr, aes(x=years, y=mean), size=1.15, col="blue") +
  geom_point(data=dffpr, aes(x=years, y=mean), size=4, col="blue") +
  geom_errorbar(data=dffpr, aes(x=years, ymin=lower, ymax=upper), width=.1, size=1.25, col="blue") +
  xlab("") +
  ylab("") +
  ylim(0, 0.075) +
  theme(legend.position="none") +
  theme(text=element_text(size=12,  family="serif")) +
  coord_fixed(ratio=60.67)
  

df <- cbind(rbind(after_FNR_L1_d, after_FNR_H3_d),
            Scenario=factor(rep(c('"easiest"','"hardest"'), each=5)))
FNRa <- ggplot(data=df, aes(linetype=Scenario)) +
  geom_line(data=df, aes(x=years, y=mean), size=1.15) +
  geom_point(data=df, aes(x=years, y=mean), size=4) +
  geom_errorbar(data=df, aes(x=years, ymin=lower, ymax=upper), width=.1, size=1.25) +
  xlab("") +
  ylab("") +
  ylim(-0.01, 0.9) +
  theme(legend.position="none") +
  theme(text=element_text(size=12,  family="serif")) +
  coord_fixed(ratio=5)

pgrid_after <- plot_grid(FPR, FNR, ncol = 2, axis="r", rel_widths = c(1,1), align = "hv")
pgrid_after