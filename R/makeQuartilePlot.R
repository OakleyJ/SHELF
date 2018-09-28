# Display four equally likely intervals as coloured bars

makeQuartilePlot <- function(lower, Q1, m, Q3, upper, fontsize=12){
  ggplot()+
    annotate("rect", xmin = lower, 
             xmax = Q1, ymin=0.2, ymax = 0.8, fill = "#a6cee3")+
    annotate("rect", xmin = Q1, xmax = m, 
             ymin=0.2, ymax = 0.8, fill = "#1f78b4")+
    annotate("rect", xmin = m, xmax = Q3, ymin=0.2, ymax = 0.8, fill = "#b2df8a")+
    annotate("rect", xmin = Q3, xmax = upper, ymin=0.2, ymax = 0.8, fill = "#33a02c")+
    xlim(lower, upper)+
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+
    scale_y_continuous(breaks = NULL, limits = c(0, 1))+
    labs(title = "Quartiles", y = expression(f[X](x)), x = "x") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_text(colour = "white"),
          text = element_text(size = fontsize))
  
}

