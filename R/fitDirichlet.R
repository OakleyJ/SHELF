#' Fit a Dirichlet distribution to elicited marginal distributions for proportions 
#' 
#' Takes elicited beta distributions for a set of proportions as inputs,
#' and fits a Dirichlet distribution. The beta parameters are adjusted 
#' so that the expectations sum to 1, and then the sum of the Dirichlet
#' parameters is chosen based on the sums of the beta parameters for each elicited marginal 
#' 
#' 
#' 
#' @param ... Multiple arguments, each an objects of class \code{elicitation}, 
#' one per marginal proportion, separated by commas. The sequence can be 
#' specified as a single argument by containing all the \code{elicitation}
#' objects within a single \code{list} object.
#' @param categories A vector of strings labelling the marginal proportions.
#' @param n.fitted The method used to determine the sum of the Dirichlet parameters.
#' Use \code{"opt"} for best fitting, derived by matching standard deviations from the elicited marginals
#' and the fitted Dirichlet; \code{"min"} for a conservative choice based 
#' on the smallest equivalent sample size (sum of the beta parameters) from the 
#' elicited marginals; \code{"med"} for the median of the smallest and largest largest equivalent sample size
#' from the 
#' elicited marginals; \code{"mean"} for the mean of all the equivalent sample sizes
#' from the 
#' elicited marginals.
#' @param plotBeta logical. Plot the original elicited marginals and the fitted marginals from the 
#' Dirichlet fit. 
#' @param xlab x-axis label on the marginal distribution plot.
#' @param ylab y-axis label on the marginal distribution plot.
#' @param fs The font size used in the plot.
#' @param silent Set to \code{TRUE} to supress printing of results to the console.
#

#' @return The parameters of the fitted Dirichlet distribution.
#' 
#' @references Zapata-Vazquez, R., O'Hagan, A. and Bastos, L. S. (2014). Eliciting expert judgements about a set of proportions. Journal of Applied Statistics 41, 1919-1933.
#' 
#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' \dontrun{
#' p1 <- c(0.25, 0.5, 0.75)
#' v1 <- c(0.5, 0.55, 0.6)
#' v2 <- c(0.22, 0.3, 0.35)
#' v3 <- c(0.11, 0.15, 0.2)
#' myfit1 <- fitdist(v1, p1, 0, 1)
#' myfit2 <- fitdist(v2, p1, 0, 1)
#' myfit3 <- fitdist(v3, p1, 0, 1)
#' d <- fitDirichlet(myfit1, myfit2, myfit3,
#'                   categories = c("A","B","C"),
#'                   n.fitted = "opt")
#' 
#' # Note that this will also work:
#' d <- fitDirichlet(list(myfit1, myfit2, myfit3),
#'                   categories = c("A","B","C"),
#'                   n.fitted = "opt")
#' 
#' }
#' @import ggplot2
#' @importFrom tidyr gather
#' @export
#' 
#'


fitDirichlet <- function(...,
                         categories = NULL,
                         n.fitted = "opt",
                         plotBeta = TRUE,
                         xlab = "x",
                         ylab = expression(f[X](x)),
                         fs = 12,
                         silent = FALSE) {
  
  Category <- x <- fx <- parameters<- NULL # hack to avoid R CMD check NOTE
  
  # Coerce the elictation objects into a single list, if necessary

  argument <- list(...)
  if(inherits(argument[[1]], "list")){
    beta.fits <- (...)
  }
  if(inherits(argument[[1]], "elicitation")){
    beta.fits <- argument
  }

  
  numCategories <- length(beta.fits)
  if (is.null(categories)) {
    categories <- paste("Category", 1:numCategories, sep = " ")
  }
  
  beta.parameters <-
    sapply(beta.fits, function(x)
      c(x$Beta[[1]], x$Beta[[2]]))
  
  # Adjust beta.parameters so that expected values of the
  # marginal proportions sum to 1.
  beta.means <- beta.parameters[1,] / colSums(beta.parameters)
  r <- sum(beta.means)
  adj.beta.parameters <- beta.parameters /r
  adj.beta.parameters[2, ] <- colSums(beta.parameters) - 
    adj.beta.parameters[1, ]
  
  # Get the dirichlet parameter n.d = \sum d_i
  n <- colSums(beta.parameters)
  v <- beta.parameters[1,] * (n - beta.parameters[1,]) /
    (n ^ 2 * (n + 1))
  n.d <- switch(
    n.fitted,
    min = min(n),
    mean = mean(n),
    med = mean(range(n)),
    opt = (sum(v * (n + 1)) / (sum(v * (
      n + 1
    ) ^ 0.5))) ^ 2 - 1
  )
  dirichlet.parameters <- n.d * adj.beta.parameters[1,] /
    colSums(adj.beta.parameters)
  names(dirichlet.parameters) <- categories
  
  # Plot original elicited marginal distributions
  # together with fitted marginal distributions from the Dirichlet
  if (plotBeta) {
    getbetadensity <- function(x) {
      dbeta((0:100) / 100, x[1], x[2])
    }
    
    df1 <- data.frame((0:100) / 100,
                      sapply(data.frame(beta.parameters),
                             getbetadensity))
    
    names(df1) <- c("x", categories)
    df1 <- tidyr::gather(df1,
                         key = Category,
                         value = fx,-x,
                         factor_key = TRUE)
    df1 <- data.frame(df1, parameters = "elicited")
   
    marginal.parameters <- rbind(dirichlet.parameters,
                                 n.d - dirichlet.parameters)
    
    df2 <- data.frame((0:100) / 100,
                      sapply(data.frame(marginal.parameters), getbetadensity))
    
    names(df2) <- c("x", categories)
    df2 <- tidyr::gather(df2,
                         key = Category,
                         value = fx,-x,
                         factor_key = TRUE)
    df2 <- data.frame(df2, parameters = "Dirichlet fit")
    
    df.all <- rbind(df1, df2)
    
    p1 <- ggplot(df.all, aes(x = x, y = fx)) +
      geom_line(aes(colour = parameters)) +
      facet_wrap(~ Category, ncol = 1) +
      labs(colour = " Marginal \n distributions", x = xlab, y = ylab) +
      theme_grey(base_size = fs)
    print(p1)
  }
  
  # diagnostics
  if(!silent){
    
    elic.marginal <- data.frame(beta.parameters)
    elic.marginal <- rbind(elic.marginal,
                           beta.means, 
                           v^0.5,
                           colSums(beta.parameters))
    names(elic.marginal) <- categories
    row.names(elic.marginal) <- c("shape1", "shape2", "mean", "sd",  "sum")
    cat("\nDirectly elicted beta marginal distributions:\n \n" )
    print(signif(elic.marginal, 3))
    
    cat("\nSum of elicited marginal means:",  
        round(sum(beta.means), 3) )
    
    fitted.marginal <- data.frame(matrix(c(dirichlet.parameters,
                                           n.d - dirichlet.parameters,
                                           dirichlet.parameters / n.d,
                                           (dirichlet.parameters * (n.d - dirichlet.parameters) /
                                              (n.d ^ 2 * (n.d + 1)))^0.5, 
                                           rep(n.d, numCategories)),
                                         ncol=numCategories, byrow=T))
    names(fitted.marginal) <- categories
    row.names(fitted.marginal) <- c("shape1", "shape2","mean", "sd", "sum")
    
    cat("\n \nBeta marginal distributions from Dirichlet fit:\n \n" )
    print(signif(fitted.marginal, 3))
  }
  
  dirichlet.parameters
}



#' Calculate quantiles for the marginal distributions of a Dirichlet distribution 
#' 
#' Given a (elicited) Dirichlet distribution, calculate quantiles for each marginal
#' beta distribution corresponding to the elicited quantiles
#' 
#' 
#' @param d A vector of parameters of the Dirichlet distribution
#' @param quantiles The desired quantiles for feedback
#' @param sf The number of significant figures displayed
#

#' @return Quantiles for each marginal distribution
#'  
#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' \dontrun{
#' feedbackDirichlet(d = c(20, 10, 5),
#'                   quantiles = c(0.1, 0.33, 0.66, 0.9))
#' }
#' @export
#' 
#'



feedbackDirichlet <- function(d, quantiles = c(0.1, 0.9), sf = 2) {
  marginal.parameters <- rbind(d, sum(d) - d)
  fittedQuantiles <- sapply(data.frame(marginal.parameters),
              function(x) {
                qbeta(quantiles, x[1], x[2])
              })
  cbind(quantiles, signif(fittedQuantiles, sf))
  
}



