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
#' @param groups \code{x = ~ var, groups=g} is equivalent to \code{ x = var ~ g }.
#' @param \dots  additional arguments, see \code{\link[stats]{t.test}} in the
#'    \pkg{stats} package.
#' 
#' @return an object of class \code{htest}
#' 
#' @details
#' This is a wrapper around \code{\link[stats]{t.test}} from the \pkg{stats} package
#' to extend the functionality of the formula interface.
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
t_test <- function(x, y=NULL, ..., data=parent.frame()) {
  x_lazy <- lazyeval::lazy(x)
  y_lazy <- lazyeval::lazy(y)
  dots_lazy <- lazyeval::lazy_dots(...)
  x_eval <- tryCatch( lazyeval::lazy_eval(x_lazy, data=data),
                      error = function(e) as.name(deparse(x_lazy$expr)))
  y_eval <- tryCatch( lazyeval::lazy_eval(y_lazy, data=data),
                      error = function(e) as.name(deparse(y_lazy$expr)))
  
  res <- ttest(x_eval, y_eval, ..., data=data) 
 
  res$data.name <- sub("^x$", deparse(x_lazy$expr), res$data.name)
  res$data.name <- sub("^x and y$", 
                       paste(deparse(x_lazy$expr), "and", deparse(y_lazy$expr)), 
                       res$data.name)
  res
}

#' @rdname ttest
#' @export t.test
#' @usage t.test(x, y=NULL, ..., data = parent.frame())
t.test <- t_test


