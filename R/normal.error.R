normal.error <-
function(parameters, values, probabilities, weights){
	sum(weights * (pnorm(values, parameters[1], exp(parameters[2])) - probabilities)^2)
}

# Optimise for location and scale only
skewnormal.error <- function(parameters, values, probabilities, weights, snAlpha){
  sum(weights * (sn::psn(values, xi = parameters[1],
                     omega = exp(parameters[2]),
                     alpha = snAlpha) - probabilities)^2)
}

# Optimise for location, scale and shape
skewnormal.error.joint <- function(parameters, values, probabilities, weights){
  sum(weights * (sn::psn(values, xi = parameters[1],
                         omega = exp(parameters[2]),
                         alpha = parameters[3]) - probabilities)^2)
}
