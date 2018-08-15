#' Generate a report to show the fitted distributions
#' 
#' Renders an Rmarkdown document to display the density function of each fitted distribution, 
#' the parameter values, and the R command required to sample from each distribution. 
#' 
#' @param fit An object of class \code{'elicitation'}.
#' @param output_format the output format for the document. One of \code{"html_document"},
#' \code{"pdf_document"} (requires LaTeX to be installed), or \code{"word_document"}
#' (requires Word to be installed).
#' @param sf number of significant figures to be displayed for the fitted parameters.
#' @param expert if the \code{fit} object contains judgements from multiple experts, the 
#' single expert's distributions to be displayed.
#' @param view whether to open the document after it has been compiled
#' @examples
#' \dontrun{
#' # One expert, with elicited probabilities
#' # P(X<20)=0.25, P(X<30)=0.5, P(X<50)=0.75
#' # and X>0.
#' v <- c(20,30,50)
#' p <- c(0.25,0.5,0.75)
#' myfit <- fitdist(vals=v, probs=p, lower=0)
#' 
#' generateReport(myfit)
#' }
#' @export
generateReport <- function(fit, output_format = "html_document",
                           sf = 3, expert = 1, view = TRUE){
    
    path <- rmarkdown::render(input = 
                     system.file("elicitationReportFile", "elicitationSummary.Rmd",
                                 package="SHELF"),
                   output_format = output_format,
                   output_dir = getwd())
    message("File saved to ", path)
    
    if(view){system2("open", shQuote(path))}
    
}