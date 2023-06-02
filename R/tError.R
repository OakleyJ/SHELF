tError <-
function(parameters, values, probabilities, weights, degreesfreedom){
	sum(weights * (pt((values-parameters[1]) / exp(parameters[2]),
	                  degreesfreedom) - probabilities)^2)

}
#   sum(weights * ((qt(probabilities, degreesfreedom) * exp(parameters[2]) +
# parameters[1]) - values)^2)