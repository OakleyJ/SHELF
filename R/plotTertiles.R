#' Plot elicted tertiles, median and plausible range for each expert
#'
#' Displays a horizontal bar for each expert, to represent the expert's plausible range.
#' The coloured sections indicate the experts' tertiles: three intervals judged by the expert
#' to be equally likely. The experts' medians are shown as dashed lines.
#'
#'
#' @param vals a matrix of elicited tertiles and medians: one column per expert, first 
#' row is the 33rd percentile, 2nd row is the median, last row is the 66th percentile.
#' @param lower a vector of lower plausible limits: one per expert
#' @param upper a vector of upper plausible limits: one per expert
#' @param fs font size to be used in the plot
#' @param percentages set to \code{TRUE} to use percentages on the x-axis
#'
#

#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' \dontrun{
#' l <- c(-5, 0, 5, -10)
#' u <- c(15, 35, 50, 35)
#' v <- matrix(c(5, 8, 10,
#'  10, 15, 20,
#'  15, 18, 25,
#'  10, 20, 30),
#'  3, 4)
#' plotTertiles(vals = v, lower = l, upper  = u)
#' }
#' @export

plotTertiles <- function(vals, lower, upper, fs = 12, percentages = FALSE){
  
  low <- L <- T1 <- M <- T2 <- U <- enumber <- NULL # hack to pass CRAN check
  
  
  n.experts <- ncol(vals)
  expert <-factor(LETTERS[1 : n.experts], levels = LETTERS[n.experts : 1])
  
  cols <- gg_color_hue(3)
  
  df1 <- data.frame(cbind(lower, t(vals), upper))
  colnames(df1) <- c("L", "T1", "M", "T2", "U")
  df1$expert <- expert
  df1$enumber <- n.experts:1
  p1 <- ggplot(df1, aes(x = low, y = expert)) +
    geom_segment(aes(yend = expert, x=L, xend = T1), lwd = 10, col = cols[1])+
    geom_segment(aes(yend = expert, x=T1, xend = T2), lwd = 10, col = cols[2])+
    geom_segment(aes(yend = expert, x=T2, xend = U), lwd = 10, col = cols[3])+
    geom_segment(aes(y = enumber -0.15, yend = enumber + 0.15, x = M, xend =M),
                 lwd = 1, linetype = "dashed")+
    labs(x = "X") +
    theme(text = element_text(size = fs)) 
  if(percentages){
    p1 <- p1 + scale_x_continuous(labels = scales::percent)
  }
  p1  
}



