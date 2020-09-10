#' Student's t-Test
#' 
#' Performs one and two sample t-tests.  
#' The mosaic `t.test` provides wrapper functions around the function 
#' of the same name in \pkg{stats}.
#' These wrappers provide an extended interface that allows for a more systematic
#' use of the formula interface.
#' 
#' @rdname ttest
#' 
#' @inheritParams stats::t.test
#' @param groups 
#'   When `x` is a formula, `groups` can be used to compare groups:  
#'   `x = ~ var, groups = g` is equivalent to ` x = var ~ g `.
#'   See the examples. 

#' 
#' @return an object of class `htest`
#' 
#' @details
#' This is a wrapper around [stats::t.test()] from the \pkg{stats} package
#' to extend the functionality of the formula interface.  In particular, one can 
#' now use the formula interface for a 1-sample t-test.  Before, the formula interface
#' was only permitted for a 2-sample test.  The type of formula that can be used
#' for the 2-sample test has also be broadened.  See the examples.
#'
#' @seealso [mosaic::prop.test()], [mosaic::binom.test()], 
#'   [stats::t.test()]
#' 
#' @examples
#'   t.test(HELPrct$age)
#'   # We can now do this with a formula
#'   t.test(~ age, data = HELPrct)
#'   # data = can be omitted, but it is better to use it
#'   t.test(~ age, HELPrct)
#'   # the original 2-sample formula
#'   t.test(age ~ sex, data = HELPrct)
#'   # alternative 2-sample formulas
#'   t.test(~ age | sex, data = HELPrct)
#'   t.test(~ age, groups = sex, data = HELPrct)
#'   # 2-sample t from vectors
#'   with(HELPrct, t.test(age[sex == "male"], age[sex == "female"]))
#'   # just the means
#'   mean(age ~ sex, data = HELPrct)

#' @export
t_test <- function(x, ...) {
  UseMethod("t_test")
}

#' @rdname ttest
#' @export
t.test <- function(x, ...) {
  UseMethod("t_test")
}

#' @rdname ttest
#' @method t_test formula
#' @export
t_test.formula <- 
  function (formula, data, ..., groups = NULL) {
    
    formula <- 
      if (FALSE && is.null(groups)) {
        mosaicCore::mosaic_formula_q(
          formula, max.slots = 2, 
          envir = if (is.environment(data)) data else environment(formula))
      } else {
        mosaicCore::mosaic_formula_q(
          formula, groups = !!rlang::enexpr(groups), max.slots = 2, 
          envir = if (is.environment(data)) data else environment(formula))
      }
    dots <- list(...)
   
    if (length(formula) == 3) {
      if (missing(data)) {
        return( stats::t.test(formula, ...) )
      } else {
        return( stats::t.test(formula, data = data, ...) )
      }
    }
    
    if (missing(data)) {
      data <- parent.frame()
    }
    
    evalF <- mosaicCore::evalFormula(formula, data)
    if (ncol(evalF$right) < 1L) 
      stop("No data specified in rhs of formula.") 
    
    vname <- names(evalF$right)[1L]
    if (ncol(evalF$right) > 1L) {  
      stop("Multiple variables specified in rhs of formula.")
    }
   
    mf <- model.frame(formula, data = data) 
    x <- evalF$right[, 1]
    res <- do.call( stats::t.test, c(list(x = x), dots) ) 
    # res$data.name <- names(mf[1])
    res$data.name <- names(evalF$right)[1]
    return(res)
}

#' @rdname ttest
#' @method t_test default
#' @export
t_test.default <-
  function (x, y = NULL, alternative = c("two.sided", "less", "greater"), 
            mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95, 
            ...) {
    xname <- deparse(substitute(x))
    yname <- deparse(substitute(y))
    res <- 
      stats::t.test( x, y = y, alternative = alternative,
                     mu = mu, paired = paired, var.equal = var.equal, 
                     conf.level = conf.level,  ...) 
    res$data.name <- 
      if (is.null(y)) {
        xname
      } else {
        paste(xname, "and", yname)
      }
    res
  }

