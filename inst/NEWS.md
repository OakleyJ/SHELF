SHELF v1.1.0 (2016-01-29) 
=========================

* Change in fitdist to starting values in optimisation: will now check for exact fits if only two probabilities elicited 

* New functions added for eliciting beliefs about uncertain population distributions:  cdffeedback, cdfplot, fitprecision, pdfplots

SHELF v1.2.0 (2016-08-16) 
==============================

* Roulette elicitation method now implemented using shiny

* New functions fitDirchlet and feedbackDirichlet for eliciting Dirichlet distributions

* New functions copulaSample and elicitConcProb for eliciting dependent distributions using multivariate normal copulas

* New function compareIntervals for comparing fitted intervals for individual distributions from multiple experts

* Change to expert.names from numbers to letters in fitdist

* Vignettes added: overview of SHELF, eliciting a Dirichlet distribution, eliciting a bivariate distribution with a bivariate normal copula 

SHELF v1.2.1 (2016-09-30) 
==============================

* Bug fixed: interactive plots now work for plotting individual distributions for multiple experts

* Bug fixed: plotting best fitting individual distributions for multiple experts