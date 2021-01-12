
#' Extract Chi-squared statistic
#' 
#' Extract Chi-squared statistic
#' 
#' @param x An object of class `"htest"` a coming from a Chi-squared test,
#' an object of class `"table"`, or
#' the inputs to [tally()].
#' @param correct a logical indicating whether a continuity correction should be 
#' applied.
#' @param ... additional arguments passed on to `tally` or `chisq.test`.
#' @seealso [stat()]
#' @examples
#' 
#' if(require(mosaicData)) {
#'   Mites.table <- tally( ~ outcome + treatment, data=Mites )
#'   Mites.table 
#'   chisq.test(Mites.table)
#'   chisq(Mites.table)
#'   chisq(chisq.test(Mites.table))
#'   ## Randomization test.  Increase replications to decrease Monte Carlo error.
#'   do(3) * chisq( tally( ~ outcome + shuffle(treatment),  data=Mites ) )
#'   Mites.rand <- do(1000) * chisq( tally( ~ outcome + shuffle(treatment),  data=Mites ) )
#'   tally( ~(X.squared >= chisq(Mites.table)), data=Mites.rand, format="proportion")
#' }
#'   
#' @export
chisq <- function(x, ...) {
  UseMethod("chisq")
}

#' @rdname chisq
#' @export
chisq.htest <- function(x, ...) {
  if (! grepl ("Chi-squared", x$method) ) {
    stop("This doesn't look like a Chi-sqaured test to me.")
  }
  setNames(x$statistic, "X.squared")
}

#' @rdname chisq
#' @export
chisq.table <- function(x, correct = FALSE, ...) {
  setNames(
  suppressWarnings(chisq.test(x, correct = correct, ...)$statistic),
    "X.squared"
  )
}
  
#' @rdname chisq
#' @export
chisq.default <- function(x, correct = FALSE, ...) {
  dots <- list(...)
  chisq_dots <- dots[intersect(names(dots), names(formals(chisq.test)))]
  # remove chisq.test() ... from tally_dots
  tally_dots <- dots
  tally_dots[names(chisq_dots)] <- NULL
  
  tally_res <- do.call(tally, c(list(x), tally_dots)) 
  setNames(
    suppressWarnings(
    do.call(
      chisq.test, 
      c(list(tally_res, correct = correct), chisq_dots)
    )$statistic),
    "X.squared"
  )
}
