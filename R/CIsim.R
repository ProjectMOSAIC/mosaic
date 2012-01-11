#' Compute confidence intervals from (multiple) simulated data sets
#' 
#' This function automates the calculation of coverage rates for exploring
#' 	the robustness of confidence interval methods.
#' 
#' @param n size of each sample
#' @param samples number of samples to simulate
#' @param rdist function used to draw random samples
#' @param args arguments required by \code{rdist}
#' @param estimand true value of the parameter being estimated
#' @param conf.level confidence level for intervals
#' @param method function used to compute intervals.  Standard functions that 
#' 	  produce an object of class \code{htest} can be used here.
#' @param method.args arguments required by \code{method}
#' @param interval a function that computes a confidence interval from data.  Function
#' 	  should return a vector of length 2.
#' @param estimate a function that computes an estimate from data
#' @param verbose print summary to screen?
#' 
#' 
#' @return A data frame with variables 
#' 	\code{lower},
#' 	\code{upper},
#' 	\code{estimate},
#' 	\code{cover} ('Yes' or 'No'),
#' 	and 
#' 	\code{sample}
#' 	is returned invisibly.  See the examples for a way to use this to display the intervals
#' 	graphically.
#'
#' @author Randall Pruim (\email{rpruim@@calvin.edu})
#' 
#' @export
#' @examples
#' CIsim(10,1000)    # 1000 95% intervals using t.test; population is N(0,1)
#' CIsim(10,1000, rdist=rexp, estimand=1)    # this time population is Exp(1)
#' xYplot(Cbind(estimate,lower,upper) ~ sample, 
#' 		data=CIsim(10,100, rdist=rexp, estimand=1),
#' 		par.settings=col.mosaic(),
#' 		groups=cover)
#' ladd(panel.abline(h=1))
#' 
#' @keywords inference 
#' @keywords simulation 
#' 


# this is borrowed from fastR.  If it stays in mosaic, it should be removed from fastR

CIsim <-
function (n, samples = 100, rdist = rnorm, args = list(), estimand = 0, 
    conf.level = 0.95, method = t.test, method.args = list(), 
    interval = function(x) {
        do.call(method, c(list(x, conf.level = conf.level), method.args))$conf.int
    }, estimate = function(x) {
        do.call(method, c(list(x, conf.level = conf.level), method.args))$estimate
    }, verbose = TRUE) 
{
    sampleData <- replicate(samples, do.call(rdist, c(list(n = n), 
        args)))
    lower <- apply(sampleData, 2, function(x) {
        interval(x)[1]
    })
    upper <- apply(sampleData, 2, function(x) {
        interval(x)[2]
    })
    estimate <- apply(sampleData, 2, function(x) {
        estimate(x)
    })
    cover <- as.integer(estimand >= lower & estimand <= upper)
    cover <- factor(cover, levels = c(0, 1), labels = c("No", 
        "Yes"))
    cis <- data.frame(lower = lower, upper = upper, estimate = estimate, 
        cover = cover, sample = 1:samples)
    if (verbose) {
        cat("Did the interval cover?")
        print(table(cis$cover)/samples)
    }
    invisible(cis)
}


