#' @rdname defunct
#'
#' @param \dots Additional arguments; currently ignored.
#'
#' @return `mm` returns an object of class `groupwiseModel`.  The functions
#' `fitted.values`, `residuals`, `coefficients`, and `summary`
#' are useful for extracting various features of the value returned by `mm`
#'
#' @details
#' `gwm` (groupwise model) is a sort of training function for
#' `lm`, meant to provide a basis for discussing inference and introducing
#' resampling in a simple, intuitive setting
#' of groupwise means or proportions.  `lm` provides a better, more general facility.
#' When using `lm` to recreate the results of `gwm`, include all the
#' interaction terms (i.e., use `*` instead of `+`) and remove the
#' intercept term.  See the examples.
#'
#' @seealso
#' [lm()],
#' [do()]
#' @export
#' @examples
#
# gwm( wage ~ sex, data=CPS85 )
# gwm( wage ~ sex + married, data = CPS85 )
# # The same model, fit using lm() instead
# lm( wage ~ sex * married - 1, data = CPS85)
# do(5) * gwm( wage ~ sex + married, data = resample(CPS85))
# mod <- gwm( width ~ domhand, data=KidsFeet)
# summary(mod)
# resid(mod)
# fitted(mod)
#' @export
gwm <- function(...) { # formula, data = parent.frame(), drop = FALSE, ...) {
  .Defunct(msg = "gwm() has been removed from `mosaic'.  \n    It will be replaced by better tools in `mosaicModel'.")
}




