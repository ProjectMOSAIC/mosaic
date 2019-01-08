#' Central portion of a distribution
#' 
#' This function determines the critical values for isolating 
#' a central portion of a distribution with a specified probability.
#' This is designed to work especially well for symmetric distributions,
#' but it can be used with any distribution.
#' 
#' @inheritParams qdist
#' @param dist a character string naming a distribution family (e.g., "norm").
#'   This will work for any family for which the usual d/p/q functions exist.
#' @param p the proportion to be in the central region, with equal proportions 
#'   in either "tail".
#' @param verbose a logical indicating whether a more verbose output value should be returned.
#' @param pattern One of `"stripes"` or `"rings"`. 
#'   In the latter case, pairs of regions (from the outside to the inside) 
#'   are grouped together for coloring and probability calculation.
#' 
#' @param ... additional arguments passed to the distribution functions.  Typically
#'   these specify the parameters of the particular distribution desired.  See the examples.
# @param tail one of \code{"upper"} or \code{"lower"} specifying whether 
#   the lower or upper critical value is returned.  
# @param warn a logical indicating whether a warning should be given when
#   using a distribution that is not symmetric.
#' @return a pair of numbers indicating the upper and lower bounds, unless `verbose` is 
#'   `TRUE`, in which case a 1-row data frame is returned containing these bounds, 
#'   the central probability, the tail probabilities, and the name of the distribution.
#' @note This function is still experimental and changes the input or output
#'   formats are possible in future versions of the package.
#' 
#' @examples
#' cdist( "norm", .95)
#' cdist( "t", c(.90, .95, .99), df=5)
#' cdist( "t", c(.90, .95, .99), df=50)
#' # plotting doesn't work well when the parameters are not constant
#' cdist( "t", .95, df=c(3,5,10,20), plot = FALSE)
#' cdist( "norm", .95, mean=500, sd=100 )
#' cdist( "chisq", c(.90, .95), df=3 )
#' # CI
#' x <- rnorm(23, mean = 10, sd = 2)
#' cdist("t", p = 0.95, df=22)
#' mean(x) + cdist("t", p = 0.95, df=22) * sd(x) / sqrt(23)
#' confint(t.test(x))
#' cdist("t", p = 0.95, df=22, verbose = TRUE)
#' @export
# # another possible implementation
# cdist <- 
#   function( dist, p, ..., verbose = FALSE ) {
#     alpha <- (1-p)/2
#     lo <- alpha
#     hi <- 1 - alpha
#     qdist <- paste0("q", dist)
#     loQ <- do.call( dpqrdist, c(list(dist = dist, type="q", p=lo), list(...) ) ) 
#     hiQ <- do.call( dpqrdist, c(list(dist = dist, type="q", p=hi), list(...) ) ) 
#     if (verbose) {
#       as.data.frame(
#         c( 
#           list(
#             lo = loQ,
#             hi = hiQ,
#             central_p = p, 
#             tail_p = alpha,
#             dist = dist
#             ),
#           list(...)
#         )
#       )
#     } else {
#        res <- cbind(loQ, hiQ)
#        if (nrow(res) == 1) {
#          res <- res[1, , drop=TRUE]
#          names(res) <- NULL
#        }
#        res
#     }
#   }

cdist <- function (
  dist = "norm", p, plot = TRUE, verbose = FALSE, invisible = FALSE, 
  digits = 3L, 
  xlim, ylim,
  resolution = 500L,
  return = c("values", "plot"),
  pattern = c("rings", "stripes"),
  ...,
  refinements = list())
{
  
  pattern <- match.arg(pattern)
  return <- match.arg(return)
  
  dots <- list(...)
  alpha <- (1-p)/2
  p <- sort(c(alpha, 1-alpha))
  q <- dpqrdist(dist, type = "q", p = p, ...) 
  
  if (verbose) {
    cat("Verbose output not yet implemented.\n")
  }
  
  res_plot <-
    do.call(
      gf_refine,
      c(list(
        plot_multi_dist(
          dist = dist, p = p, 
          xlim = xlim, ylim = ylim, 
          digits = digits, 
          resolution = resolution,
          pattern = pattern,
          ...)),
        refinements)
    )
  
  if (return == "plot") {
    return(res_plot)
  }
  if (plot) {
    print(res_plot)
  }
  if (invisible) { 
    return(invisible(q))
  }
  return(q)
}

# 
# .cdist <- function( dist, p, ... , tail=c("upper","lower"), warn=TRUE) {
#   tail = match.arg(tail)
#   alpha <- (1-p)/2
#   lo <- alpha
#   hi <- 1 - alpha
#   qdist <- paste0("q", dist)
#   loT <- do.call( dpqrdist, c(list(dist = dist, type="q", lo), list(...) ) ) 
#   hiT <- do.call( dpqrdist, c(list(dist = dist, type="q", hi), list(...) ) ) 
#   # hiT <- do.call( qdist, c(list(hi), list(...) ) ) 
#   if ( any( abs(hiT) - abs(loT) > 1e-5 ) ) {
#     if (warn) warning(paste0("It looks like your distribution is not symmetric.  I'm providing ", tail, " tails.") )
#   }
#   
#   return(
#     switch( tail,
#             "lower" = loT,
#             "upper" = hiT
#     )
#   )
# }
# 
