# Display three equally likely intervals as coloured bars, and 
# add the median as a dashed line

makeTertilePlot <- function(lower, t1, m, t2, upper, fontsize=12){
  ggplot()+
    annotate("rect", xmin = lower, 
             xmax = t1, ymin=0.2, ymax = 0.8, fill = "#66c2a5")+
    annotate("rect", xmin = t1, xmax = t2, 
             ymin=0.2, ymax = 0.8, fill = "#fc8d62")+
    annotate("rect", xmin = t2, xmax = upper,
             ymin=0.2, ymax = 0.8, fill = "#8da0cb")+
    xlim(lower, upper)+
    geom_vline(xintercept = m, linetype = "dashed")+
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+
    scale_y_continuous(breaks = NULL, limits = c(0, 1))+
    labs(title = "Tertiles and median", y = expression(f[X](x)),
         x = "x") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_text(colour = "white"),
          text = element_text(size = fontsize))
}