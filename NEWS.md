SHELF v1.11.0 (2024-09-03)
==============================

* New app elicitSurvivalExtrapolation() and supporting functions survivalScenario(), survivalModelExtrapolations() for eliciting extrapolated survival probabilities.

* vignette multivariate-normal-copula removed.

SHELF v1.10.0 (2024-05-07) 
==============================

* Added the skew normal distribution to the set of fitted distributions.

* Bug fixed in feedback - was forcing rounding to 3 s.f. if using feedback for multiple experts.

* elicitQuartiles() and elicitTertiles() are deprecated. Use elicit() instead.

* condDirichlet() is deprecated. Use elicitDirchlet() instead.

* elicitConcProb() is deprecated. Use elicitBivariate() instead.

* non-exported function extractDistributions() removed.

* package test coverage improved.

* elicitation report includes roulette allocation, if used.

* improved error handling in shiny apps.

SHELF v1.9.0 (2023-05-31) 
==============================

* Changed multivariate normal sampling in copulaSample() to use Cholesky decomposition
instead of eigendecomposition (latter can produce different results on different machines).

* new function compareGroupRIO(). Use this to produce plots to compare the the final consensus ("RIO") distribution with the individual elicited judgements, and a linear pool of the individual elicited judgements. Incorporated this feature in the elicit() app.

* can now fit exponential distributions as a special case of the Gamma distribution (or mirror exponential as special case of mirror gamma). Only requires one appropriate limit and a single probability. Mainly intended for the roulette method, if probs are only allocated to two adjacent bins. Updated error reporting when fitdist() is unable to fit distributions.

* bug fixed: output names from feedback() were different depending on whether single or multiple experts. Changed to be consistent with single expert case.

* optional input arguments added to elicit() to allow roulette options
to be specified from command line.

* bug fixed: copulaSample() will now run if fitdist() was used on separate judgements from multiple experts. Extra argument (ex) used to select judgements from a single expert (copulaSample() will not produce judgements for multiple experts simultaneously).

* new package test for copulaSample()

* some internal naming changes in fitdist(): tParameters, fFit, tError. (Sorry, my naming formatting is all over the place...)

SHELF v1.8.0 (2021-06-18) 
==============================

* argument int removed from plotfit(): can no longer launch shiny apps from the
plotfit() command for plotting distributions. Use elicit() and elicitMultiple()
instead for interactive plotting.

* elicitation report files (R Markdown documents) now include plots of fitted distributions.

* Bugs fixed in elicitDirichlet() - code wouldn't run with more than three categories.

* fitdist() has a new argument for excluding log t and mirror log t when identifying best fit (default is FALSE).

* Plots can be downloaded from shiny apps as .png files.

* New (negatively skewed) distributions: mirror gamma, mirror lognormal, and mirror log t. These all fit distributions to (upper - X). 

* Better handling of expert names: elicitMultiple(): can now click on and edit expert names; names are displayed in all plots. plotfit() will now use the expert names, if provided in fitdist(). Can also specify names in plotTertiles() and plotQuartiles(). 

* elicitMultiple(): can now control axes limits in quartile and tertile plot.

* new function rlinearpool() for sampling from a weighted linear pool

* bug fixed in copulaSample. Was rounding samples to 3 s.f. Increased to 8. Mistake in help file corrected, regarding syntax for distribution names.

* elicitExtension(): can now upload a sample from the distribution of the extension variable, instead of eliciting a distribution.

* plotfit(): additional argument returnPlot (default is FALSE) will also return the plot as a ggplot object.

SHELF v1.7.0 (2020-02-06) 
==============================

* new app for the extension method: elicitMixture(), for discrete extension variables.

* bug fixed in elicitExtension(): when plotting conditional densities for the logit link function, x-axis limits now restricted to 0 and 1.

* bug fixed in plotfit(): will now correctly display a single distribution for
a selected expert when requested, if multiple distributions have been elicited.

* plinearpool() and qlinearpool(): can now directly specify different distribution
types for each expert to use in the linear pool.

* fitdist(): extra argument expertnames, for specifying row names in the various
outputs.

* elicit() app: can now report fitted probabilities as well as fitted quantiles,
and can change x-axis label

* bug fixed: switched from class(x) == "foo" to inherits(x, "foo"), to avoid assumption
length(class(x)) == 1

SHELF v1.6.0 (2019-06-12) 
==============================

* new app for the extension method: elicitExtension(). New command line functions
for the extension method are plotConditionalDensities(), 
plotConditionalMedianFunction() and sampleMarginalFit().

* makeCDFPlot() function is now exported: plots the elicited cumulative probabilities, and fitted cumulative distribution functions.

* elicitMultiple() app: can now enter judgements with the roulette method, and save/load judgements 
as .csv files

* column names changed in output of feedback(), fitdist() and sampleFit() to be consistent:
"normal", "t", "gamma", "lognormal", "logt", "beta", "hist"


SHELF v1.5.0 (2019-03-26) 
==============================

* roulette() has been removed, and the roulette method is now available within elicit()

* Extra argument percentages in plotfit() and plotTertiles() for using percentage scale on x-axis

* New function sampleFit(), for generating samples from fitted distributions.

* Minor change to fitDirichlet(), to allow marginal elicitation fits to be 
specified as a single list. 

* Update to fitprecision(): interval used in the proportion method can now be a 
tail area of the population distribution

* New shiny app elicitBivariate() for eliciting bivariate distributions using a Gaussian copula

* Significant update to elicit() shiny app: can now switch between multiple methods within the same app

* New shiny app elicitMultiple() for fitting individual distributions to multiple experts' judgements

* Bugs fixed: plinearpool() now chooses the best fitting distribution for each expert if argument d = "best" is specified. Correctly handles probabilities for log-t, where x is below lower limit.

* Bugs fixed: qlinearpool() could return NA in some cases if argument d = "best" was specified: now fixed. Correctly handles probabilities for log-t, where x is below lower limit. Minor improvement to accuracy in estimated quantiles: finer grid used in linear interpolation of the quantile function.

SHELF v1.4.0 (2018-08-18) 
==============================

* New function: generateReport(): renders an Rmarkdown document to give formulae and parameter values for all the fitted distributions

* New function: condDirichlet(), for viewing conditional distributions from elicited Dirichlet distributions

* New functions: plotQuartiles() and plotTertiles(), for displaying individuals quartiles/tertiles elicited from a group of experts

* New functions: elicitQuartiles() and elicitTertiles(): shiny apps for eliciting with the quartile and tertile methods

* elicit() and roulette() functions now both return the elicited values and results as objects of class "elicitation"


SHELF v1.3.0 (2017-10-31) 
==============================

* Bug fixed: ensure solid line used for linear pool when plotting. Option in plotfit added to plot all individual densities with same colour, to simplify legend.

* New function: linearPoolDensity, for extracting density values from the linear pool.

* Bug fixed: can now accept more than 26 experts.

* Bug fixed: qlinearpool/plinearpool now works with log t distributions.

* New function: elicitHeterogen, for eliciting prior for variance of random effects in meta-analysis

SHELF v1.2.3 (2017-02-10) 
==============================

* Bug fixed: can fit (and plot) distributions bounded below when lower limit is negative

* Bug fixed: roulette method shiny interface works with non-integer bin boundaries

SHELF v1.2.2 (2016-11-14) 
==============================

* Accept non-decreasing probabilities in elicited judgements, rather than only strictly increasing probabilities

* Can specify own axes labels in the plotfit command with arguments xlab and ylab

* Update to Multivariate-normal-copula.Rmd vignette, to match update to GGally


SHELF v1.2.1 (2016-09-06) 
==============================

* Bug fixed: interactive plots now work for plotting individual distributions for multiple experts

* Bug fixed: plotting best fitting individual distributions for multiple experts

SHELF v1.2.0 (2016-08-16) 
==============================

* Roulette elicitation method now implemented using shiny

* New functions fitDirchlet and feedbackDirichlet for eliciting Dirichlet distributions

* New functions copulaSample and elicitConcProb for eliciting dependent distributions using multivariate normal copulas

* New function compareIntervals for comparing fitted intervals for individual distributions from multiple experts

* Change to expert.names from numbers to letters in fitdist

* Vignettes added: overview of SHELF, eliciting a Dirichlet distribution, eliciting a bivariate distribution with a bivariate normal copula 


SHELF v1.1.0 (2016-01-29) 
=========================

* Change in fitdist to starting values in optimisation: will now check for exact fits if only two probabilities elicited 

* New functions added for eliciting beliefs about uncertain population distributions:  cdffeedback, cdfplot, fitprecision, pdfplots