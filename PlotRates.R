PlotRates <- function(scenario_pos, control_pos, leg = "none",
                      xlab = "", ylab ="", title = "",
                      alpha = c(0.0001, 0.00025, 0.0005, 0.00075, 0.001, 0.0025, 0.005,
                                0.0075, 0.01, 0.025, 0.05, 0.075, 0.1)){
  
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
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(title) + #substr(deparse(substitute(scenario_pos)), 1, 3)
    scale_colour_manual(values = c("orange","blue","black"),
                        labels = c("FPR (control)", "FPR", "FNR"),
                        name = "Performance") +
    geom_hline(yintercept = 0.1, linetype = "dashed") +
    theme(legend.position=leg) +
    theme(text=element_text(size=12,  family="serif"))
}