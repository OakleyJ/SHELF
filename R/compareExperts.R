#' Plot fitted intervals for each expert
#' 
#' Following elicitation of distributions from individual experts, plot fitted probability 
#' intervals for each expert.
#' 
#' @param fit An object of class \code{elicitation}
#' @param interval The probability p for each interval (i.e. the fitted probability for each expert 
#' that the displayed interval contains the uncertain quantity will be p)
#' @param dist The distribution fitted to each expert's probabilities. Options are
#' \code{"normal"}, \code{"t"}, \code{"gamma"}, \code{"lognormal"},
#' \code{"logt"},\code{"beta"}, and
#' \code{"best"} (for best fitting). Can be a vector if different distributions are desired for each expert.
#' @param fs font size used in the plot.
#' 
#' @examples
#' 
#' \dontrun{
#' v <- matrix(c(30, 40, 50, 20, 25, 35, 40, 50, 60, 35, 40, 50), 3, 4)
#' p <- c(0.25, 0.5, 0.75)
#' myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
#' compareIntervals(myfit, interval = 0.5)
#' }
#' @export
compareIntervals <- function(fit, interval = 0.95, dist = "best", fs = 12){
  
  low <- med <- up <- NULL # hack to avoid R CMD check NOTE
  
  n.experts <- nrow(fit$limits)
  fb <- feedback(fit, quantiles = c((1 - interval) / 2, 0.5, 0.5 + interval /2),
                 dist = dist)
  df1<-t(fb$expert.quantiles)
  colnames(df1) <- c("low", "med", "up")
  expert <-factor(LETTERS[1 : n.experts], levels = LETTERS[n.experts : 1])
  df1<-data.frame(df1, expert)
  theme_set(theme_grey(base_size = fs))
  p1<-ggplot(df1, aes(x = low, y = expert)) +
    geom_segment(aes(yend = expert, xend = up)) +
    geom_point(aes(x = med), colour = "red", size = 3) +
    labs(x= "X")
  print(p1)
  print(fb$distributions)  
}




