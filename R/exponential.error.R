exponential.error <-
function(parameters, values, probabilities, weights){
	sum(weights * (pexp(values, parameters) -probabilities)^2)
}
