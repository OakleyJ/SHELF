#' Fit distributions to elicited probabilities 
#' 
#' Takes elicited probabilities as inputs, and fits parametric distributions
#' using least squares on the cumulative distribution function. If separate
#' judgements from multiple experts are specified, the function will fit one
#' set of distributions per expert.
#' 
#' 
#' @param vals A vector of elicited values for one expert, or a matrix of
#' elicited values for multiple experts (one column per expert). Note that the
#' an elicited judgement about X should be of the form P(X<= vals[i,j]) =
#' probs[i,j]
#' @param probs A vector of elicited probabilies for one expert, or a matrix of
#' elicited values for multiple experts (one column per expert). A single
#' vector can be used if the probabilities are the same for each expert. For
#' each expert, the smallest elicited probability must be less than 0.4, and
#' the largest elicited probability must be greater than 0.6.
#' @param lower A single lower limit for the uncertain quantity X, or a vector
#' of different lower limits for each expert. Specifying a lower limit will
#' allow the fitting of distributions bounded below.
#' @param upper A single upper limit for the uncertain quantity X, or a vector
#' of different lower limits for each expert. Specifying both a lower limit and
#' an upper limit will allow the fitting of a Beta distribution.
#' @param weights A vector or matrix of weights corresponding to vals if
#' weighted least squares is to be used in the parameter fitting.
#' @param tdf The number of degrees of freedom to be used when fitting a
#' t-distribution.
#' @param expertnames Vector of names to use for each expert.
#' @param excludelogt Set to TRUE to exclude log-t and mirror log-t when identifying
#' best fitting distribution.
#' 
#' @return An object of class \code{elicitation}. This is a list containing the elements
#' \item{Normal}{Parameters of the fitted normal distributions.}
#' \item{Student.t}{Parameters of the fitted t distributions. Note that (X -
#' location) / scale has a standard t distribution. The degrees of freedom is
#' not fitted; it is specified as an argument to \code{fitdist}.}
#' \item{Gamma}{Parameters of the fitted gamma distributions. Note that E(X - \code{lower}) =
#' shape / rate.} 
#' \item{Log.normal}{Parameters of the fitted log normal
#' distributions: the mean and standard deviation of log (X - \code{lower}).}
#' \item{Log.Student.t}{Parameters of the fitted log student t distributions.
#' Note that (log(X- \code{lower}) - location) / scale has a standard t distribution. The
#' degrees of freedom is not fitted; it is specified as an argument to
#' \code{fitdist}.} 
#' \item{Beta}{Parameters of the fitted beta distributions. X
#' is scaled to the interval [0,1] via Y = (X - \code{lower})/(\code{upper} -
#' \code{lower}), and E(Y) = shape1 / (shape1 + shape2).} 
#' \item{mirrorgamma}{Parameters of ('mirror') gamma distributions fitted to Y = \code{upper} - X. Note that E(Y) =
#' shape / rate.}
#' \item{mirrorlognormal}{Parameters of ('mirror') log normal distributions
#' fitted to Y = \code{upper} - X.} 
#' \item{mirrorlogt}{Parameters of ('mirror') log Student-t distributions fitted to Y = \code{upper} - X. 
#' Note that (log(Y) - location) / scale has a standard t distribution. The
#' degrees of freedom is not fitted; it is specified as an argument to
#' \code{fitdist}.} 
#' \item{ssq}{Sum of
#' squared errors for each fitted distribution and expert. Each error is the
#' difference between an elicited cumulative probability and the corresponding
#' fitted cumulative probability.} 
#' \item{best.fitting}{The best fitting
#' distribution for each expert, determined by the smallest sum of squared
#' errors.} 
#' \item{vals}{The elicited values used to fit the distributions.}
#' \item{probs}{The elicited probabilities used to fit the distributions.}
#' \item{limits}{The lower and upper limits specified by each expert (+/- Inf
#' if not specified).}
#' @note The least squares parameter values are found numerically using the
#' \code{optim} command. Starting values for the distribution parameters are
#' chosen based on a simple normal approximation: linear interpolation is used
#' to estimate the 0.4, 0.5 and 0.6 quantiles, and starting parameter values
#' are chosen by setting E(X) equal to the 0.5th quantile, and Var(X) = (0.6
#' quantile - 0.4 quantile)^2 / 0.25. Note that the arguments \code{lower} and
#' \code{upper} are not included as elicited values on the cumulative
#' distribution function. To include a judgement such as P(X<=a)=0, the values
#' a and 0 must be included in \code{vals} and \code{probs} respectively.
#' @author Jeremy Oakley <j.oakley@@sheffield.ac.uk>
#' @examples
#' \dontrun{
#' # One expert, with elicited probabilities
#' # P(X<20)=0.25, P(X<30)=0.5, P(X<50)=0.75
#' # and X>0.
#' v <- c(20,30,50)
#' p <- c(0.25,0.5,0.75)
#' fitdist(vals=v, probs=p, lower=0)
#' 
#' # Now add a second expert, with elicited probabilities
#' # P(X<55)=0.25, P(X<60=0.5), P(X<70)=0.75
#' v <- matrix(c(20,30,50,55,60,70),3,2)
#' p <- c(0.25,0.5,0.75)
#' fitdist(vals=v, probs=p, lower=0)
#' 
#' # Two experts, different elicited quantiles and limits.
#' # Expert A: P(X<50)=0.25, P(X<60=0.5), P(X<65)=0.75, and provides bounds 10<X<100
#' # Expert B: P(X<40)=0.33, P(X<50=0.5), P(X<60)=0.66, and provides bounds 0<X
#' v <- matrix(c(50,60,65,40,50,60),3,2)
#' p <- matrix(c(.25,.5,.75,.33,.5,.66),3,2)
#' l <- c(10,0)
#' u <- c(100, Inf)
#' fitdist(vals=v, probs=p, lower=l, upper=u)
#' }
#' @import stats
#' @export
#' 
#' 
fitdist <-
  function(vals, probs, lower = -Inf,
           upper = Inf, weights = 1, tdf = 3,
           expertnames = NULL,
           excludelogt = FALSE){
    
    if(is.matrix(vals)==F){vals<-matrix(vals, nrow = length(vals), ncol = 1)}
    if(is.matrix(probs)==F){probs <- matrix(probs, nrow = nrow(vals), ncol = ncol(vals))}
    if(is.matrix(weights)==F){weights <- matrix(weights, nrow = nrow(vals), ncol = ncol(vals))}
    if(length(lower)==1){lower <- rep(lower, ncol(vals))}
    if(length(upper)==1){upper <- rep(upper, ncol(vals))}
    if(length(tdf)==1){tdf <- rep(tdf, ncol(vals))}
    
    
    
    n.experts <- ncol(vals)
    normal.parameters <- matrix(NA, n.experts, 2)
    t.parameters <- matrix(NA, n.experts, 3)
    mirrorgamma.parameters <- gamma.parameters <- 
      matrix(NA, n.experts, 2)
    mirrorlognormal.parameters <- 
      lognormal.parameters <- matrix(NA, n.experts, 2)
    mirrorlogt.parameters <- logt.parameters <-
      matrix(NA, n.experts, 3)
    beta.parameters <- matrix(NA, n.experts, 2)
    ssq<-matrix(NA, n.experts, 9)
    
    colnames(ssq) <- c("normal", "t",
                       "gamma", "lognormal", "logt", "beta",
                       "mirrorgamma",
                       "mirrorlognormal",
                       "mirrorlogt")
    
    
    if(n.experts > 1 & n.experts < 27 & is.null(expertnames)){
      expertnames <- paste("expert.", LETTERS[1:n.experts], sep="")
    }
    
    if(n.experts > 27 & is.null(expertnames)){
      expertnames <- paste("expert.", 1:n.experts, sep="")
    }
    
    limits <- data.frame(lower = lower, upper = upper)
    row.names(limits) <- expertnames
    
    
    for(i in 1:n.experts){
      if (min(probs[,i]) > 0.4 ){stop("smallest elicited probability must be less than 0.4")}
      if (min(probs[,i]) < 0 | max(probs[,i]) > 1 ){stop("probabilities must be between 0 and 1")}
      if (max(probs[,i]) < 0.6 ){stop("largest elicited probability must be greater than 0.6")}
      if (min(vals[,i]) < lower[i]){stop("elicited parameter values cannot be smaller than lower parameter limit")}
      if (max(vals[,i]) > upper[i]){stop("elicited parameter values cannot be greater than upper parameter limit")}
      if (tdf[i] <= 0 ){stop("Student-t degrees of freedom must be greater than 0")}
      if (min(probs[-1,i] - probs[-nrow(probs),i]) < 0 ){stop("probabilities must be specified in ascending order")}
      if (min(vals[-1,i] - vals[-nrow(vals),i]) <= 0 ){stop("parameter values must be specified in ascending order")}
      
      
      # Need to exclude any probability judgements
      # P(X<=x) = 0 or P(X<=x) = 1
      # Should enforce these probabilities via the parameter limits
      
      inc <- (probs[, i] > 0) & (probs[, i] < 1)
      
      minprob <- min(probs[inc, i])
      maxprob <- max(probs[inc, i])
      minvals <- min(vals[inc, i])
      maxvals <- max(vals[inc, i])
      
      q.fit <- approx(x = probs[inc,i], y = vals[inc,i],
                      xout = c(0.4, 0.5, 0.6))$y
      l <- q.fit[1] # estimated 40th percentile on original scale
      u <- q.fit[3] # estimated 60th percentile on original scale
      
     # if(minprob > 0 & maxprob < 1){
        
        minq <- qnorm(minprob)
        maxq <- qnorm(maxprob)
        # Estimate m and v assuming X~N(m,v)
        
        # Obtain m by solving simultaneously:
        # m + Z_l \sqrt{v} = X_l
        # m + Z_u \sqrt{v} = X_u
        # where Z_a is a-th quantile from N(0, 1), X_a is a-th quantile of X
        m <- (minvals * maxq - maxvals * minq) / (maxq - minq)
        v <- ((maxvals - minvals) / (maxq - minq))^2
     # }else{
      #  minq <- qnorm(min(probs[probs[, i] > 0, i]))
      #  maxq <- qnorm(max(probs[probs[, i] < 1, i]))
      #  m <- q.fit[2] # Estimated median on original scale
      #  v<- (u - l)^2 / 0.25 # Estimated variance on original scale
     # } 
      
      # Symmetric distribution fits ----
      
      normal.fit <- optim(c(m, 0.5*log(v)), 
                          normal.error, values = vals[inc,i], 
                          probabilities = probs[inc,i], 
                          weights = weights[inc,i])   
      normal.parameters[i,] <- c(normal.fit$par[1], exp(normal.fit$par[2]))
      ssq[i, "normal"] <- normal.fit$value
      
      # starting values: c(m, log((u - m)/ qt(0.6, tdf[i])))
      
      t.fit <- optim(c(m, 0.5*log(v)), t.error, 
                     values = vals[inc,i], 
                     probabilities = probs[inc,i], 
                     weights = weights[inc,i], 
                     degreesfreedom = tdf[i])
      t.parameters[i, 1:2] <- c(t.fit$par[1], exp(t.fit$par[2]))
      t.parameters[i, 3] <- tdf[i]
      ssq[i, "t"] <- t.fit$value
      
      # Positive skew distribution fits ----
      
      
      if(lower[i] > -Inf){
        vals.scaled1 <- vals[inc,i] - lower[i]
        m.scaled1 <- m - lower[i]
        
        gamma.fit<-optim(c(log(m.scaled1^2/v), log(m.scaled1/v)), 
                         gamma.error, values = vals.scaled1, 
                         probabilities = probs[inc,i], 
                         weights = weights[inc,i])
        gamma.parameters[i,] <- exp(gamma.fit$par)
        ssq[i, "gamma"] <- gamma.fit$value
        
        std<-((log(u - lower[i])-log(l - lower[i]))/1.35)
        
        mlog <- (log(minvals - lower[i]) * 
                   maxq - log(maxvals - lower[i]) * minq) /
          (maxq - minq)
        
        lognormal.fit <- optim(c(mlog,
                                 log(std)), 
                               lognormal.error, 
                               values = vals.scaled1, 
                               probabilities = probs[inc,i], 
                               weights = weights[inc,i])
        lognormal.parameters[i, 1:2] <- c(lognormal.fit$par[1],
                                          exp(lognormal.fit$par[2]))
        ssq[i, "lognormal"] <- lognormal.fit$value
        
        logt.fit <- optim(c(log(m.scaled1), log(std)), 
                          logt.error, 
                          values = vals.scaled1, 
                          probabilities = probs[inc,i], 
                          weights = weights[inc,i], 
                          degreesfreedom = tdf[i])
        logt.parameters[i,1:2] <- c(logt.fit$par[1], exp(logt.fit$par[2]))
        logt.parameters[i,3] <- tdf[i]
        ssq[i, "logt"] <- logt.fit$value
      }
      
      # Beta distribution fits ----
      
      if((lower[i] > -Inf) & (upper[i] < Inf)){
        vals.scaled2 <- (vals[inc,i] - lower[i]) / (upper[i] - lower[i])
        m.scaled2 <- (m - lower[i]) / (upper[i] - lower[i])
        v.scaled2 <- v / (upper[i] - lower[i])^2
        
        alp <- abs(m.scaled2 ^3 / v.scaled2 * (1/m.scaled2-1) - m.scaled2)
        bet <- abs(alp/m.scaled2 - alp)
        if(identical(probs[inc, i], 
                     (vals[inc, i] - lower[i]) / (upper[i] - lower[i]))){
          alp <- bet <- 1
        }
        beta.fit <- optim(c(log(alp), log(bet)), 
                          beta.error, 
                          values = vals.scaled2, 
                          probabilities = probs[inc,i], 
                          weights = weights[inc,i])
        beta.parameters[i,] <- exp(beta.fit$par)
        ssq[i, "beta"] <- beta.fit$value	
        
      }
      
      # Negative skew distribution fits ----
      
      if(upper[i] < Inf){
        
        # Distributions are fitted to Y:= Upper limit - X
        
        valsMirrored <- upper[i] - vals[inc, i]
        probsMirrored <- 1 - probs[inc, i]
        mMirrored <- upper[i] - m
        
        # Mirror gamma
        
        
        
        mirrorgamma.fit<-optim(c(log(mMirrored^2/v), log(mMirrored/v)), 
                               gamma.error, values = valsMirrored, 
                               probabilities = probsMirrored, 
                               weights = weights[inc,i])
        mirrorgamma.parameters[i,] <- exp(mirrorgamma.fit$par)
        ssq[i, "mirrorgamma"] <- mirrorgamma.fit$value
        
        # Mirror log normal
        
        
        # Obtain mlogMirror by solving simultaneously:
        # m + Z_l \sqrt{v} = Y_l
        # m + Z_u \sqrt{v} = Y_u
        # where Z_a is a-th quantile from N(0, 1),
        # Y_a is a-th quantile of Y
        # and we model Y = log(upper - X) ~ N(mlogMirror, stdMirror^2)
        
        
        mlogMirror <- (log(upper[i] - maxvals) * 
                         (1 - minq) -
                         log(upper[i] - minvals) * (1-maxq)) /
          (maxq - minq)
        
        stdMirror <-((log(upper[i] - l)-log(upper[i] - u))/1.35)
        
        
        mirrorlognormal.fit <- optim(c(mlogMirror,
                                       log(stdMirror)), 
                                     lognormal.error, 
                                     values = valsMirrored, 
                                     probabilities = probsMirrored, 
                                     weights = weights[inc,i])
        mirrorlognormal.parameters[i, 1:2] <-
          c(mirrorlognormal.fit$par[1],
            exp(mirrorlognormal.fit$par[2]))
        ssq[i, "mirrorlognormal"] <- mirrorlognormal.fit$value
        
        # Mirror log t
        
        mirrorlogt.fit <- optim(c(log(mMirrored), log(stdMirror)), 
                          logt.error, 
                          values = valsMirrored, 
                          probabilities = probsMirrored, 
                          weights = weights[inc,i], 
                          degreesfreedom = tdf[i])
        mirrorlogt.parameters[i,1:2] <- c(mirrorlogt.fit$par[1],
                                          exp(mirrorlogt.fit$par[2]))
        mirrorlogt.parameters[i,3] <- tdf[i]
        ssq[i, "mirrorlogt"] <- mirrorlogt.fit$value
        
      }
    }
    dfn <- data.frame(normal.parameters)
    names(dfn) <-c ("mean", "sd")
    row.names(dfn) <- expertnames
    
    dft <- data.frame(t.parameters)
    names(dft) <-c ("location", "scale", "df")
    row.names(dft) <- expertnames
    
    dfg <- data.frame(gamma.parameters)
    names(dfg) <-c ("shape", "rate")
    row.names(dfg) <- expertnames
    
    dfmirrorg <- data.frame(mirrorgamma.parameters)
    names(dfmirrorg) <-c ("shape", "rate")
    row.names(dfmirrorg) <- expertnames
    
    dfln <- data.frame(lognormal.parameters)
    names(dfln) <-c ("mean.log.X", "sd.log.X")
    row.names(dfln) <- expertnames
    
    dfmirrorln <- data.frame(mirrorlognormal.parameters)
    names(dfmirrorln) <-c ("mean.log.X", "sd.log.X")
    row.names(dfmirrorln) <- expertnames
    
    dflt <- data.frame(logt.parameters)
    names(dflt) <-c ("location.log.X", "scale.log.X", "df.log.X")
    row.names(dflt) <- expertnames
    
    dfmirrorlt <- data.frame(mirrorlogt.parameters)
    names(dfmirrorlt) <-c ("location.log.X", "scale.log.X", "df.log.X")
    row.names(dfmirrorlt) <- expertnames
    
    dfb <- data.frame(beta.parameters)
    names(dfb) <-c ("shape1", "shape2")
    row.names(dfb) <- expertnames
    
    ssq <- data.frame(ssq)
    row.names(ssq) <- expertnames
    
    if(excludelogt){
      reducedssq <- ssq[, c("normal", "t", "gamma",
                              "lognormal", "beta",
                              "mirrorgamma",
                              "mirrorlognormal")]
      index <- apply(reducedssq, 1, which.min)
      best.fitting <- data.frame(best.fit=
                                   names(reducedssq)[index])}else{
      index <- apply(ssq, 1, which.min)
      best.fitting <- data.frame(best.fit=names(ssq)[index])
      }
      
  
    
    row.names(best.fitting) <- expertnames
    
    vals <- data.frame(vals)
    names(vals) <- expertnames
    
    probs <- data.frame(probs)
    names(probs) <- expertnames
    
    fit <- list(Normal = dfn, Student.t = dft, 
                Gamma = dfg, Log.normal = dfln, 
                Log.Student.t = dflt, Beta = dfb,
                mirrorgamma = dfmirrorg,
                mirrorlognormal = dfmirrorln,
                mirrorlogt = dfmirrorlt,
                ssq = ssq, 
                best.fitting = best.fitting, vals = t(vals), 
                probs = t(probs), limits = limits)
    class(fit) <- "elicitation"
    fit
  }
