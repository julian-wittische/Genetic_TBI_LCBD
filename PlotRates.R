PlotRates <- function(scenario_pos, control_pos){
  
  library(ggplot2)
  library(reshape2)
  library(cowplot)
  
  meansfpr <- sapply(scenario_pos, function(x) mean(x$FPR))
  sdfpr <- sapply(scenario_pos, function(x) sd(x$FPR))
  
  meansfnr <- sapply(scenario_pos, function(x) mean(x$FNR))
  sdfnr <- sapply(scenario_pos, function(x) sd(x$FNR))
  
  meansfpr_control <- sapply(control_pos, function(x) mean(x))
  sdfpr_control <- sapply(control_pos, function(x) sd(x))
  
  allbl <- data.frame(alpha, meansfpr_control, meansfpr, meansfnr)
  allbl_m <- melt(allbl, id.vars="alpha")
  
  pl <- ggplot(data=allbl_m, aes(x=alpha, y=value, col=variable)) +
    geom_line() +
    geom_point() +
    xlab("Threshold (p.TBI)") +
    ylab("Rate") +
    ggtitle(substr(deparse(substitute(scenario_pos)), 1, 3)) +
    scale_colour_manual(values = c("orange","blue","black"),
                        labels = c("FPR (control)", "FPR", "FNR"),
                        name = "Performance") +
    geom_hline(yintercept = 0.1, linetype = "dashed")
}

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

plot_grid(bl1_pl, bl2_pl, bl3_pl, bm1_pl, bm2_pl, bm3_pl, bh1_pl, bh2_pl)
plot_grid(bl1_pl, bl2_pl, bl3_pl, bm1_pl, bm2_pl, bm3_pl, bh1_pl, bh2_pl, bh3_pl)

# 

PlotRates(bl1_combine_positions, cl)
PlotRates(bm1_combine_positions, cm)
PlotRates(bh1_combine_positions, ch)


