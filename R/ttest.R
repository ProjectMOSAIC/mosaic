#' Student's t-Test
#' 
#' Performs one and two sample t-tests.  
#' The mosaic \code{t.test} provides wrapper functions around the function 
#' of the same name in \pkg{stats}.
#' These wrappers provide an extended interface that allows for a more systematic
#' use of the formula interface.
#' 
#' @rdname ttest
#' 
#' @param x a formula or a non-empty numeric vector
#' @param y an optional non-empty numeric vector or formula
#' @param data a data frame
#' @param \dots  additional arguments, see \code{\link[stats]{t.test}} in the
#'    \pkg{stats} package.
#'   When \code{x} is a formula, \code{groups} can be used to compare groups:  
#'   \code{x = ~ var, groups=g} is equivalent to \code{ x = var ~ g }.
#'   See the examples. 

#' 
#' @return an object of class \code{htest}
#' 
#' @details
#' This is a wrapper around \code{\link[stats]{t.test}} from the \pkg{stats} package
#' to extend the functionality of the formula interface.  In particular, one can 
#' now use the formula interface for a 1-sample t-test.  Before, the formula interface
#' was only permitted for a 2-sample test.  The type of formala that can be used
#' for the 2-sample test has also be broadened.  See the examples.
#'
#' @seealso \code{\link[mosaic]{prop.test}}, \code{\link[mosaic]{binom.test}}, 
#'   \code{\link[stats]{t.test}}
#' 
#' @examples
#' if (require(mosaicData)) {
#'   t.test(~ age, data=HELPrct)
#'   t.test(age ~ sex, data=HELPrct)
#'   t.test(~ age | sex, data=HELPrct)
#'   t.test(~ age, groups=sex, data=HELPrct)
#' }

#' @export 
t_test <- function(x, y = NULL, ..., data=parent.frame()) {
  orig.call <- match.call()
  x_lazy <- lazyeval::f_capture(x)
  y_lazy <- lazyeval::f_capture(y)
  dots_lazy <- lazyeval::dots_capture(...)
  
  x_eval <- tryCatch( lazyeval::f_eval(x_lazy, as.list(data)),
                      error = function(e) lazyeval::f_rhs(x_lazy) )
  y_eval <- tryCatch( lazyeval::f_eval(y_lazy, as.list(data)),
                      error = function(e) lazyeval::f_rhs(y_lazy) )
  
  res <- ttest(x_eval, y_eval, ..., data=data, data.name = orig.call[["data"]]) 
  
  res$data.name <- sub("^x$", first_one(deparse(f_rhs(x_lazy))), res$data.name)
  res$data.name <- sub("^x and y$", 
                       paste(first_one(deparse(f_rhs(x_lazy))), "and", 
			     first_one(deparse(f_rhs(y_lazy)))), 
                       res$data.name)
  res
}

# If x has length > 1, create a character string with
# the first component of x followed by ...
first_one <- function(x) {
  if (length(x) > 1) {
    paste(x[1], "...")
  } else {
    x
  }
}
  
#' @rdname ttest
#' @export t.test
#' @usage t.test(x, y=NULL, ..., data = parent.frame())
t.test <- t_test


