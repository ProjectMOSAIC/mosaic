utils::globalVariables(c("nlab", "cover"))

#' Compute confidence intervals from (multiple) simulated data sets
#' 
#' This function automates the calculation of coverage rates for exploring
#' 	the robustness of confidence interval methods.
#' 
#' @param n size of each sample
#' @param samples number of samples to simulate
#' @param rdist function used to draw random samples
#' @param args arguments required by `rdist`
#' @param plot one of `"print"`, `"return"`, `"horizontal"`, or `"none"` 
#'   describing whether a plot should be printed, returned, printed with horizontal intervals,
#'   or not generated at all.
#' @param estimand true value of the parameter being estimated
#' @param conf.level confidence level for intervals
#' @param method function used to compute intervals.  Standard functions that 
#' 	  produce an object of class `htest` can be used here.
#' @param method.args arguments required by `method`
#' @param interval a function that computes a confidence interval from data.  Function
#' 	  should return a vector of length 2.
#' @param estimate a function that computes an estimate from data
#' @param verbose print summary to screen?
#' 
#' 
#' @return A data frame with variables 
#' 	`lower`,
#' 	`upper`,
#' 	`estimate`,
#' 	`cover` ('Yes' or 'No'),
#' 	and 
#' 	`sample`
#' 	is returned invisibly.  See the examples for a way to use this to display the intervals
#' 	graphically.
#'
#' 
#' @examples
#' # 1000 95% intervals using t.test; population is N(0,1)
#' CIsim(n=10, samples=1000)    
#' # this time population is Exp(1); fewer samples, so we get a plot 
#' CIsim(n=10, samples=100, rdist=rexp, estimand=1) 
#' # Binomial treats 1 like success, 0 like failure
#' CIsim(n=30, samples=100, rdist=rbinom, args=list(size=1, prob=.7), 
#'        estimand = .7, method = binom.test, method.args=list(ci = "Plus4"))  
#' 
#' @keywords inference 
#' @keywords simulation 
#' 
#' @export


# this is borrowed from fastR.  If it stays in mosaic, it should be removed from fastR

CIsim <-
  function (n, samples = 100, rdist = rnorm, args = list(), 
            plot = if (samples <= 200) "draw" else "none",
            estimand = 0, 
            conf.level = 0.95, method = t.test, method.args = list(),
            interval = function(x) {
              do.call(method, c(list(x, conf.level = conf.level), method.args))$conf.int
            }, estimate = function(x) {
              do.call(method, c(list(x, conf.level = conf.level), method.args))$estimate
            }, verbose = TRUE) 
{
    plot <- match.arg(plot, c("draw", "horizontal", "return", "none"))
    # Grid will have a row for each simulated sample
    Grid <- expand.grid(n = n, sample = 1:samples) %>% mutate(estimand = estimand)
    # a list of data sets, one for each row in Grid
    sampleData <- 
      lapply(1:nrow(Grid),
             function(r) do.call(rdist, c(list(n = Grid[r, "n"]),  args)))
    CIs <- Grid %>% mutate(
      lower    = sapply(sampleData, function(x) { interval(x)[1] }),
      upper    = sapply(sampleData, function(x) { interval(x)[2] }),
      estimate =  sapply(sampleData, function(x) { estimate(x) }),
      cover    = (estimand <= lower) + (estimand < upper),
      cover    = factor(cover, levels = 0L:2L, labels = c("Low", "Yes", "High")),
      nlab     = reorder(factor(paste0("n = ", n)), n)
    )
                    
    if (verbose) {
      message("Interval coverage:")
      message(paste(
        capture.output(t(tally(~ cover | n, data = CIs, format = "prop"))), 
        collapse = "\n"))
    }
    
    plotG <- 
      ggplot(aes(x = sample, y = estimate, ymin = lower, ymax = upper), data = CIs) + 
        geom_errorbar(aes(color = cover)) +
        geom_point(aes(color = cover), size = 0.7) + 
        geom_abline(slope = 0, intercept = estimand, alpha = 0.4) +
        scale_colour_manual(drop  =  FALSE, breaks = c("High", "Yes", "Low"),
                              values = c("red", "navy", "red"), guide = FALSE)
      
    switch(plot,
           return = return(plotG + facet_wrap(~ nlab)),
           horizontal = print(plotG + coord_flip() + facet_wrap(~ nlab)),
           draw = print(plotG + facet_wrap(~ nlab)),
           none = {}
           )
    return(invisible(CIs %>% select(-nlab)))
}


