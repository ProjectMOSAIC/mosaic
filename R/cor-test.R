#' Alternative formula interface for cor.test
#' 
#' [stats::cor.test()] in \pkg{stats} accepts formulas of the 
#' shape `~ y + x`.  The \pkg{mosaic} package allows the use 
#' of `y ~ x` as an alternative formula shape.
#' 
#' @param formula a formula
#' @param x,y numeric vectors of data values. x and y must have the same length.
#' @param ... other arguments passed to [stats::cor.test()].
#' @seealso [stats::cor.test()] in the \pkg{stats} package.
#' @examples
#' # This is an example from example(stats::cor.test) done in old and new style
#' require(graphics)
#' cor.test(~ CONT + INTG, data = USJudgeRatings)
#' cor.test(CONT ~ INTG, data = USJudgeRatings)
#' @rdname cor.test
#' @export
cor_test.formula <- function(formula, ...) {
  dots <- list(...)
  if (! is.null(lhs(formula))) {
    f <- substitute(~ y + x, list(y = lhs(formula), x = rhs(formula)))
    environment(f) <- environment(formula)
    formula <- f
  }
  do.call(stats::cor.test, c(list(formula), dots))
}

#' @export
#' @rdname cor.test
cor.test <- function(x, ...) {
  UseMethod("cor_test")
}

#' @export
#' @rdname cor.test
cor_test <- function(x, ...) {
  UseMethod("cor_test")
}
    
#' @export
#' @rdname cor.test
cor_test.default <- function(x, y, ...) {
  stats::cor.test(x, y, ...)
}
