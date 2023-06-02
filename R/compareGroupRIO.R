#' Compare individual elicited distributions with linear pool and RIO distribution
#'
#' Produce one of three plots to compare the individual elicited judgements with the final
#' elicited distribution, chosen to represent the views of a "Rational Impartatial Observer" (RIO)
#' as part of the SHELF process. A linear pool of fitted distributions from the individually elicited
#' judgements is also obtained. The plot choices are a display of the quartiles, a display of the tertiles,
#' and a plot of the various density functions.
#' 
#'
#'
#' @param groupFit either an object of class \code{elicitation}, or the file path
#' for a .csv file exported from the elicitMultiple() app. This should contain 
#' the individually elicited judgements from the experts
#' @param RIOFit an object of class \code{elicitation} containing a single set of 
#' of probability judgements corresponding to the "Rational Impartial Observer (RIO)".
#' @param type the plot used to show the comparison: one of "quartiles", "tertiles" or "density".
#' @param dLP the distribution fitted to each expert's judgements and to the linear pool. Options are 
#' Options are "normal", "t", "gamma", "lognormal", "logt","beta", "mirrorgamma",
#'"mirrorlognormal", "mirrorlogt" "hist" (for a histogram fit), and "best" (for best fitting).
#' @param dRIO the distribution fitted to RIO's judgements. Options are the same as for \code{dLP}.
#' @param xlab x-axis label in plot
#' @param ylab y-axis label in plot
#' @param fs font size used in plot


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
#' p <- c(0.25, 0.5, 0.75)
#' group <- fitdist(vals = v, probs = p, lower = l, upper = u)
#' rio <- fitdist(vals = c(12, 20, 25), probs = p, lower = 1, upper = 100)
#' compareGroupRIO(groupFit = group, RIOFit = rio, dRIO = "gamma")
#' }
#' @export

compareGroupRIO <- function(groupFit, RIOFit, type = "density",
                            dLP = "best", dRIO = "best",
                            xlab = "x",
                            ylab = expression(f[X](x)),
                            fs = 12){
  
  if(inherits(groupFit, "character")){
    groupFit <- readSHELFcsv(groupFit)
  }
  
  expertnames <- rownames(groupFit$vals)
  n.experts <- length(expertnames)
  
  if(dRIO == "best"){
    dRIO <- RIOFit$best.fit[1, 1]
  }
  
  if(type == "quartiles"){
    individualQuartiles <- feedback(groupFit, 
                                    quantiles = c(0.25, 0.5, 0.75))$fitted.quantiles
    RIOQuartiles <- feedback(RIOFit, 
                             quantiles = c(0.25,
                                           0.5,
                                           0.75))$fitted.quantiles[, dRIO]
    LPQuartiles <- qlinearpool(groupFit, q = c(0.25, 0.5, 0.75), d = dLP)
    
    quartileVals <- cbind(individualQuartiles, LPQuartiles,
                          RIOQuartiles)
    lmin <- min(c(groupFit$limits[, "lower"], RIOFit$limits[, "lower"]))
    lAll <- c(groupFit$limits[, "lower"], lmin,  RIOFit$limits[, "lower"])
    umax <- max(c(groupFit$limits[, "upper"], RIOFit$limits[, "upper"]))
    uAll <- c(c(groupFit$limits[, "upper"], umax, RIOFit$limits[, "upper"]))
    
    p1 <- plotQuartiles(vals = quartileVals, 
                  lower = lAll, upper = uAll,
                  expertnames = c(expertnames,  "Linear pool", "RIO"))
    
  }
  if(type == "tertiles"){
    individualTertiles <- feedback(groupFit, 
                                   quantiles = c(0.33, 0.5, 0.67))$fitted.quantiles
    RIOTertiles <- feedback(RIOFit, 
                             quantiles = c(0.33,
                                           0.5,
                                           0.67))$fitted.quantiles[, dRIO]
    LPTertiles <- qlinearpool(groupFit, q = c(0.33, 0.5, 0.67), d = dLP)

    tertileVals <- cbind(individualTertiles, LPTertiles,
                         RIOTertiles)

    lmin <- min(c(groupFit$limits[, "lower"], RIOFit$limits[, "lower"]))
    lAll <- c(groupFit$limits[, "lower"], lmin,  RIOFit$limits[, "lower"])
    umax <- max(c(groupFit$limits[, "upper"], RIOFit$limits[, "upper"]))
    uAll <- c(c(groupFit$limits[, "upper"], umax, RIOFit$limits[, "upper"]))
    
    p1 <- plotTertiles(vals = tertileVals, 
                 lower = lAll, upper = uAll,
                 expertnames = c(expertnames,  "Linear pool", "RIO"))
  }
  if(type == "density"){
    pRIO <- plotfit(RIOFit, returnPlot = TRUE, d = dRIO,
                    showPlot = FALSE)
    pgroup <- plotfit(groupFit, lp = TRUE, returnPlot = TRUE,
                      showPlot = FALSE,
                      d = dLP)
    
    rioData <- ggplot_build(pRIO)$data[[1]]
    groupData <- ggplot_build(pgroup)$data
    
    x <- y <- expert <- NULL # hack to avoid R CMD check NOTE
    
 
    df1 <- NULL
    for(i in 1:(n.experts + 1)){
      df1 <- rbind(df1, data.frame(x = groupData[[i]]$x,
                                   y = groupData[[i]]$y)
      )
    }
    df1 <- rbind(df1, data.frame(x = rioData$x,
                                 y = rioData$y) )
    nxTotal <- length(groupData[[1]]$x)
    df1$expert <- factor(c(rep(expertnames,
                               each = nxTotal),
                           rep("Linear pool", nxTotal),
                           rep("RIO", length(rioData$x))))
    cols <- scales::hue_pal()(n.experts + 2)
    linetypes <- c(rep("dashed", n.experts), "solid", "solid")
    sizes <-  c(rep(0.5, n.experts ), 1,  1.5)
    names(cols) <- names(linetypes) <-
      names(sizes) <- c(expertnames, "Linear pool", "RIO" )
    
    p1 <- ggplot(df1, aes(x = x, y = y, 
                          colour = expert, 
                          linetype = expert, 
                          size = expert)) +
      geom_line() +
      scale_colour_manual(values = cols,
                          breaks = c(expertnames, "Linear pool", "RIO" )) +
      scale_linetype_manual(values = linetypes,
                            breaks = c(expertnames, "Linear pool", "RIO" )) +
      scale_size_manual(values = sizes,
                        breaks = c(expertnames, "Linear pool", "RIO" ))
    
    
  }
  p1 <- p1 + labs(x = xlab, y = ylab) + theme_grey(base_size = fs)
  
  print(p1)       
  
}

# Helper functions ----

readSHELFcsv <- function(filename){
  
  
  # Work out if roulette or general method was used. 
  # If roulette method was used, first row will have bin labels, e.g. (0-10], (10-20]
  # so search for (-] in first row:
  
  methodString <- readLines(filename, n = 1)
  if(grepl("\\(.*-.*\\]", methodString)){
    method <- "roulette"}else{
      method <- "general"
    }
  
  SHELFcsv <- utils::read.csv(filename)
  
  if(method == "roulette"){
    p <- apply(SHELFcsv[, -1], 1, cumsum) /
      matrix(apply(SHELFcsv[, -1], 1, sum), 
             ncol(SHELFcsv[, -1]), nrow(SHELFcsv), byrow = TRUE)
    colnames(p) <- rownames(p) <- NULL
    x <- colnames(SHELFcsv)[-1]
    n <- length(x)
    l <- as.numeric(strsplit(x[1], '[.]')[[1]][2])
    u <- as.numeric(strsplit(x[n], '[.]')[[1]][3])
    v <- matrix(seq(from = l, to = u, length = (n+1))[-1], n, 2)
    expertnames <- SHELFcsv[, 1]
  }
  
  if(method == "general"){
    nr <- nrow(SHELFcsv)
    nc <- ncol(SHELFcsv)
    v <- as.matrix(SHELFcsv[2:(nr-1), 2:nc])
    colnames(v) <- rownames(v) <- NULL
    p <- as.numeric(SHELFcsv[2:(nr-1), 1])
    l <- as.numeric(SHELFcsv[1, 2:nc])
    u <- as.numeric(SHELFcsv[nr, 2:nc])
    expertnames <- colnames(SHELFcsv)[-1]
  }
  
  fitdist(vals = v, probs = p, lower = l, upper = u,
                      expertnames  = expertnames)
  
}

