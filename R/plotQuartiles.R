#' Plot elicted quartiles, median and plausible range for each expert
#'
#' Displays a horizontal bar for each expert, to represent the expert's plausible range.
#' The coloured sections indicate the experts' quartiles: four intervals judged by the expert
#' to be equally likely. The experts' medians are shown as dashed lines.
#'
#'
#' @param vals a matrix of elicited tertiles and medians: one column per expert, first 
#' row is the 25th percentile, 2nd row is the median, last row is the 75th percentile.
#' @param lower a vector of lower plausible limits: one per expert
#' @param upper a vector of upper plausible limits: one per expert
#'
#

#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' \dontrun{
#' l <- c(2, 1, 5, 1)
#' u <- c(95, 90, 65, 40)
#' v <- matrix(c(15, 25, 40,
#'  10, 20, 40,
#'  10, 15, 25,
#'  5, 10, 20),
#'  3, 4)
#' plotQuartiles(vals = v, lower = l, upper  = u)
#' }
#' @export

plotQuartiles <- function(vals, lower, upper){
  
  low <- L <- Q1 <- M <- Q2 <- U <- enumber <- NULL # hack to pass CRAN check
  
  n.experts <- ncol(vals)
  expert <-factor(LETTERS[1 : n.experts], levels = LETTERS[n.experts : 1])
  
  cols <- gg_color_hue(4)
  
  df1 <- data.frame(cbind(lower, t(vals), upper))
  colnames(df1) <- c("L", "Q1", "M", "Q2", "U")
  df1$expert <- expert
  df1$enumber <- n.experts:1
  theme_set(theme_grey(base_size = 18))
  ggplot(df1, aes(x = low, y = expert)) +
    geom_segment(aes(yend = expert, x=L, xend = Q1), lwd = 10, col = cols[1])+
    geom_segment(aes(yend = expert, x=Q1, xend = M), lwd = 10, col = cols[2])+
    geom_segment(aes(yend = expert, x=M, xend = Q2), lwd = 10, col = cols[3])+
    geom_segment(aes(yend = expert, x=Q2, xend = U), lwd = 10, col = cols[4])+
    geom_segment(aes(y = enumber -0.15, yend = enumber + 0.15, x = M, xend =M),
                 lwd = 1, linetype = "dashed")+
    labs(x = "X")
}


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}
