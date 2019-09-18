# ########
# 
# plot1l1 <- PlotRates(il1_combine_positions, cl, ylab = "Rate")
# plot2l1 <- PlotRates(i2l1_combine_positions, cl)
# plot3l1 <- PlotRates(i3l1_combine_positions, cl)
# plot4l1 <- PlotRates(i4l1_combine_positions, cl)
# plot5l1 <- PlotRates(i5l1_combine_positions, cl, xlab = "Threshold (p.TBI)")
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

CI_TBI <- function(x,threshold_number, FPR_or_FNR){
  sapply(x, function(x) CI(x[,paste(FPR_or_FNR)]))[,threshold_number]
}
before_FPR_L1 <- rbind(CI_TBI(il1_combine_positions, 11, "FPR"),
                      CI_TBI(i2l1b_combine_positions, 11, "FPR"),   
                      CI_TBI(i3l1b_combine_positions, 11, "FPR"),
                      CI_TBI(i4l1b_combine_positions, 11, "FPR"),
                      CI_TBI(i5l1b_combine_positions, 11, "FPR"))

before_FNR_L1 <- rbind(CI_TBI(il1_combine_positions, 11, "FNR"),
                      CI_TBI(i2l1b_combine_positions, 11, "FNR"),
                      CI_TBI(i3l1b_combine_positions, 11, "FNR"),
                      CI_TBI(i4l1b_combine_positions, 11, "FNR"),
                      CI_TBI(i5l1b_combine_positions, 11, "FNR"))

before_FPR_H3 <- rbind(CI_TBI(ih3_combine_positions, 11, "FPR"),
                      CI_TBI(i2h3b_combine_positions, 11, "FPR"),   
                      CI_TBI(i3h3b_combine_positions, 11, "FPR"),
                      CI_TBI(i4h3b_combine_positions, 11, "FPR"),
                      CI_TBI(i5h3b_combine_positions, 11, "FPR"))

before_FNR_H3 <- rbind(CI_TBI(ih3_combine_positions, 11, "FNR"),
                      CI_TBI(i2h3b_combine_positions, 11, "FNR"),   
                      CI_TBI(i3h3b_combine_positions, 11, "FNR"),
                      CI_TBI(i4h3b_combine_positions, 11, "FNR"),
                      CI_TBI(i5h3b_combine_positions, 11, "FNR"))

before_FPR_L1_d <- data.frame(years=5:1,before_FPR_L1)
before_FNR_L1_d <- data.frame(years=5:1,before_FNR_L1)
before_FPR_H3_d <- data.frame(years=5:1,before_FPR_H3)
before_FNR_H3_d <- data.frame(years=5:1,before_FNR_H3)


dffprb <- cbind(rbind(before_FPR_L1_d, before_FPR_H3_d),
               Scenario=factor(rep(c('"easiest"','"hardest"'), each=5)))
FPRb <- ggplot(data=dffprb, aes(linetype=Scenario)) +
  geom_line(data=dffprb, aes(x=years, y=mean), size=1.15, col="blue") +
  geom_point(data=dffprb, aes(x=years, y=mean), size=4, col="blue") +
  geom_errorbar(data=dffprb, aes(x=years, ymin=lower, ymax=upper), width=.1, size=1.25, col="blue") +
  xlab("") +
  ylab("") +
  ylim(0, 0.075) +
  theme(legend.position="none") +
  theme(text=element_text(size=12,  family="serif")) +
  coord_fixed(ratio=60.67) +
  scale_x_continuous(breaks=1:5, labels=c("5", "4", "3", "2", "1"))

dfb <- cbind(rbind(before_FNR_L1_d, before_FNR_H3_d),
            Scenario=factor(rep(c('"easiest"','"hardest"'), each=5)))
FNRb <- ggplot(data=dfb, aes(linetype=Scenario)) +
  geom_line(data=dfb, aes(x=years, y=mean), size=1.15) +
  geom_point(data=dfb, aes(x=years, y=mean), size=4) +
  geom_errorbar(data=dfb, aes(x=years, ymin=lower, ymax=upper), width=.1, size=1.25) +
  xlab("") +
  ylab("") +
  ylim(-0.01, 0.9) +
  theme(legend.position="none") +
  theme(text=element_text(size=12,  family="serif")) +
  coord_fixed(ratio=5) +
  scale_x_continuous(breaks=1:5, labels=c("5", "4", "3", "2", "1"))

FNRleg <- ggplot(data=dfb, aes(linetype=Scenario)) +
  geom_line(data=dfb, aes(x=years, y=mean), size=1.15) +
  geom_point(data=dfb, aes(x=years, y=mean), size=4) +
  geom_errorbar(data=dfb, aes(x=years, ymin=lower, ymax=upper), width=.1, size=1.25) +
  xlab("Number of years before the event") +
  ylab("Rate") +
  ylim(-0.01, 0.9) +
  theme(legend.position="right") +
  theme(text=element_text(size=12,  family="serif")) +
  coord_fixed(ratio=5) +
  scale_x_continuous(breaks=1:5, labels=c("5", "4", "3", "2", "1"))

legend <- get_legend(FNRleg)

pgrid_before <- plot_grid(FPRb, FNRb, legend, ncol = 2, axis="r", rel_widths = c(1,1))
pgrid_before

pgrid_all <- plot_grid(FPRa, FNRa, FPRb, FNRb, legend, ncol = 2, axis="r")
pgrid_all
