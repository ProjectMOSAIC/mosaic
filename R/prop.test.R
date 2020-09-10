#' Exact and Approximate Tests for Proportions
#' 
#' The mosaic `prop.test` provides wrapper functions around the function of the same name in \pkg{stats}.
#' These wrappers provide an extended interface (including formulas).  
#' `prop.test` performs an approximate test of a simple null hypothesis about the 
#' probability of success in a Bernoulli or multinomial experiment
#' from summarized data or from raw data.
#' 
# @usage prop.test( x, n, p = NULL, alternative = c("two.sided", "less", "greater"), 
#' 					conf.level = 0.95, ...) 
#' 
#' @param x  count of successes, length 2 vector of success and failure counts, a formula,
#'   			or a character, numeric, or factor vector containing raw data.
#'     		
#'
#' 
#' @param n  sample size (successes + failures) or a data frame 
#'   (for the formula interface) 
#' 
#' @param p  a vector of probabilities of success. 
#' The length of p must be the same as the number of groups specified by x, 
#' and its elements must be greater than 0 and less than 1.
#' 
#' @param alternative   character string specifying the alternative hypothesis, must be one of 
#' `"two.sided"` (default), `"greater"` or `"less"`. You can specify just the initial letter. 
#' Only used for testing the null that a single proportion equals a given value, or that two proportions 
#' are equal; ignored otherwise.
#' 
#' @param conf.level confidence level of the returned confidence interval. Must be a single number 
#' between 0 and 1. Only used when testing the null that a single proportion equals a given value, 
#' or that two proportions are equal; ignored otherwise.
#'
#' @param success  level of variable to be considered success.  All other levels are 
#'   	considered failure.
#'
#' @param data a data frame (if missing, `n` may be a data frame)
#' 
#' @param ... additional arguments (often ignored).  
#'   When `x` is a formula, `groups` can be used to compare groups:  
#'   `x = ~ var, groups=g` is equivalent to ` x = var ~ g `. `na.rm` can be a logical
#'   or an integer vector of length 1 or 2 to indicate dimension along which NA's are 
#'   removed before coputing the test.
#'   See the examples. 
#' 
#' @note When `x` is a 0-1 vector, 0 is treated as failure and 1 as success. Similarly,
#' for a logical vector `TRUE` is treated as success and `FALSE` as failure.
#'
#' @return an `htest` object
#' 
#' @details
#' This is a wrapper around [prop.test()] to simplify its use
#' when the raw data are available, in which case 
#' an extended syntax for `prop.test` is provided.  
#' 
#' @seealso [mosaic::binom.test()], [stats::prop.test()]
#' 
#' @examples
#' # Several ways to get a confidence interval for the proportion of Old Faithful
#' # eruptions lasting more than 3 minutes.
#' prop.test( faithful$eruptions > 3 )
#' prop.test(97,272)
#' faithful$long <- faithful$eruptions > 3
#' prop.test( faithful$long )
#' prop.test( ~long , data = faithful )
#' prop.test( homeless ~ sex, data = HELPrct )
#' prop.test( ~ homeless | sex, data = HELPrct )
#' prop.test( ~ homeless, groups = sex, data = HELPrct )
#' prop.test(anysub ~ link, data = HELPrct, na.rm = TRUE)
#' prop.test(link ~ anysub, data = HELPrct, na.rm = 1)
#' prop.test(link ~ anysub, data = HELPrct, na.rm = TRUE)
#' 
#' @keywords stats

#'
#' @rdname prop.test
#' @export

prop.test <- function( x, n, p = NULL, 
                       alternative = c("two.sided", "less", "greater"), 
                       conf.level = 0.95, data = NULL, success=NULL, ...) 
{
  missing_n <- missing(n)
  x_lazy <- rlang::enquo(x)
  x_eval <- 
    tryCatch(
      rlang::eval_tidy(x_lazy, data),
      error = function(e) {
        if (is.null(data) && ! missing_n) {
          stop("prop.test(): Improper `n'; did you forget `data =' perhaps?", call. = FALSE) 
        }
        rlang::f_rhs(x_lazy)
      }
    )
  
  # this list will later be converted to a string using the appropriate information
  # dependent upon which of the prop_test methods is called.  
  
  data.name <- list(x = rlang::enexpr(x), 
                    n = rlang::enexpr(n), 
                    data = substitute(data)) 
  
  if (missing_n) {
    prop_test(x_eval, p = p, alternative = alternative, 
              conf.level = conf.level, data = data, data.name = data.name, 
              success = success, ...)
  } else {
    prop_test(x_eval, n, p = p, alternative = alternative, 
              conf.level = conf.level, data = data, data.name = data.name, 
              success=success, ...)
  }
}

#' Internal function for testing proportion
#' 
#' This function is wrapped by [`prop.test()`], which most users should use instead.
#' 
#' @param x a vector, count, or formula.
#' @param n a vector of counts of trials (not needed when `x` is a table or matrix).
#' @param p a vector of probabilities of success (for the null hypothesis).  
#'   The length must be the same as the number of groups specified by `x`.
#' @param ... additional arguments passed to methods.
# #' @param data a data frame
# #' @param data.name a character string used to label the data in output.
# #' @param groups an expression defining groups when `x` is a formula.
# #' @param success the level to be considered "success".
#' @inheritParams stats::prop.test
#' @export
prop_test <- 
  function(
    x, n, p = NULL, 
    alternative = c("two.sided", "less", "greater"), 
    conf.level = 0.95, ...)  
  {
    UseMethod("prop_test")
  }


#' @export
prop_test.default <-
  function(
    x, n, p=NULL, 
    alternative = c("two.sided", "less", "greater"), 
    conf.level = 0.95, ..., data, data.name) 
  {
    dots <- list(...)
    res <- do.call(stats::prop.test, 
                   c(
                     list(x = x, n = n , p = p, alternative = alternative,
                          conf.level = conf.level), 
                     dots)
    )
    res
  }

#' @export
prop_test.formula <-
  function(
    x, n, p=NULL, 
    alternative = c("two.sided", "less", "greater"), 
    conf.level = 0.95, success = NULL, data.name, data = NULL, groups = NULL, na.rm = FALSE, ...) 
  {
    missing_n <- missing(n)
    if (is.null(data)) {
      if (! missing_n) stop("Improper `n'; did you forget `data = ' perhaps?", call. = FALSE)
      data <- environment(x)
    }
    
    formula <- 
      if (FALSE && is.null(groups)) {
        mosaicCore::mosaic_formula_q(x, max.slots = 2) 
      } else {
        mosaicCore::mosaic_formula_q(x, groups = !!rlang::enexpr(groups), max.slots = 2)
      }
    missing_data.name <- missing(data.name)
    if (is.null(data)) {
      data <- environment(x)
    }
    
    dots <- list(...)
    #    groups <- eval(substitute(groups), data, environment(formula))
    #    subset <- eval(substitute(subset), data, environment(formula))
    if (missing_n) { #  && !missing.data) {
      form <- lattice::latticeParseFormula(formula, data, #subset = subset, #groups = groups,  
                                           subscripts = TRUE, drop = TRUE)
      if (missing_data.name) {
        data.name <- 
          paste(rlang::enexpr(data), "$", form$right.name, sep="")
      } 
      if (is.list(data.name)) {
        data.name <- 
          paste(data.name$data, "$", form$right.name, sep="")
      }
    } else {
      form <- lattice::latticeParseFormula(formula, n, #subset = subset, #groups = groups,  
                                           subscripts = TRUE, drop = TRUE)
      if (missing_data.name) {
        data.name <- 
          paste(rlang::enexpr(n), "$", form$right.name, sep="")
      }
      if (is.list(data.name)) {
        data.name <- 
          paste(data.name$n, "$", form$right.name, sep="")
      }
      data <- n
    }
    # now data.name should be set and data should hold the data
    
    #    groups <- eval(substitute(groups), data, environment(formula))
    #    subset <- eval(substitute(subset), data, environment(formula))
    groups <- form$groups
    subscr <- form$subscr
    cond <- form$condition
    x <- form$right
    
    if (! is.null(form$left) && !is.null(form$condition) )
      stop("Formulas may not have both lhs and condition for prop.test.")
    
    if (! is.null(form$left) || !is.null(form$condition) ) {
      table_from_formula <-  tally( formula, data=data, margin=FALSE, format="count" )
      if (! is.null(success)) {
        key <- names(dimnames(table_from_formula))[1]
        # if (! success %in% data[, key]) {
        #   stop("(in prop_test) `", success, "' is not a value of `", key, "'", 
        #        call. = FALSE)
        # }
        data[, key] <- factor(data[, key] == success, levels = c("TRUE", "FALSE"))
        table_from_formula <-  tally( formula, data=data, margin=FALSE, format="count" )
      }
      
      dims <- length(dim(table_from_formula))
      if (isTRUE(na.rm)) {
        na.rm <- 1:dims
      }
      if (1 %in% na.rm) {
        table_from_formula <-
          table_from_formula[ which(!is.na(dimnames(table_from_formula)[[1]])), ]
      }
      if (2 %in% na.rm) {
        table_from_formula <-
          table_from_formula[,  which(!is.na(dimnames(table_from_formula)[[2]]))]
      }
      
      if (dim(table_from_formula)[1] > 2) {
        if (any(is.na(dimnames(table_from_formula)[[1]]))) {
          stop(paste0(names(dimnames(table_from_formula))[1], ' has ', dim(table_from_formula)[1],
                      ' levels (including NA).  Only 2 are allowed.'), call. = FALSE)
        }
          stop(paste0(dimnames(table_from_formula)[1], ' has ', dim(table_from_formula)[1],
                      ' levels.  Only 2 are allowed.'))
      }
      for (i in 1:dims) {
        if (any(is.na(dimnames(table_from_formula)[[i]]))) {
          warning(
            call. = FALSE,
            paste0("NA is being treated as a category for ", names(dimnames(table_from_formula))[i]))
        }
      }
      res <- stats::prop.test( t(table_from_formula), 
                               p=p,
                               conf.level=conf.level, 
                               alternative=alternative, 
                               ...)  
      res$data.name <- paste0("tally(", deparse(formula), ")")
      return(res)
    }
    
    if (length(cond) == 0) {
      cond <- list(gl(1, length(x)))
    }
    
    prop_test(x, p=p, alternative=alternative, 
              conf.level=conf.level, success=success, data.name=data.name, ...)
  }

#' @export
prop_test.numeric <-
  function(
    x,  n, p=NULL, 
    alternative = c("two.sided", "less", "greater"), 
    conf.level = 0.95, success = NULL, ..., data = NULL, data.name) 
  {
    if (! is.null(data)) 
      stop( "prop.test: If data is not NULL, first argument should be a formula.")
    
    # first handle case when n is provided
    if ( !missing(n) ) {  
      if (missing(data.name)) {
        data.name <- paste(rlang::enexpr(x), "out of", rlang::enexpr(n))
      }
      if (is.list(data.name)) {
        data.name <- paste(data.name$x, "out of", data.name$n)
      }
      result <-  stats::prop.test(x=x, n=n, p=p, alternative=alternative,
                                  conf.level=conf.level,...) 
      result$data.name <- data.name 
      if (!is.null(success)) 
        result$data.name <- 
        paste0(data.name, "  [with success = ", success, "]")
      return(result)
    }
    
    # when n is missing, treat the numbers as raw data rather than counts
    
    if (missing(data.name)) { 
      data.name <- rlang::enexpr(x)
    }
    if (is.list(data.name)) {
      data.name <- data.name$x 
    }
    # set a reasonable value for success if none given
    if (is.null(success)) {
      success <- 
        if (all(x %in% c(0, 1))) 1 else
          if (0 %in% x) 0 else 
            min(x, na.rm=TRUE)
    }
    
    prop_test(x=factor(x), p=p, alternative=alternative, 
              conf.level=conf.level, 
              success=success, 
              data.name=data.name, ...)
  }

#' @export
prop_test.character <-
  function(
    x,  n, p = NULL, 
    alternative = c("two.sided", "less", "greater"), 
    conf.level = 0.95, success = NULL, ..., data = NULL, data.name) 
  {
    if (! is.null(data)) 
      stop( "binom.test: If data is not NULL, first argument should be a formula.")
    
    if (missing(data.name)) { 
      data.name <- rlang::enexpr(x)
    }
    if (is.list(data.name)) { 
      data.name <- data.name$x 
    }
    prop_test(x=factor(x), p=p, alternative=alternative, 
              conf.level=conf.level, 
              success=success, 
              data.name=data.name, ...)
  }

#' @export
prop_test.logical <-
  function(
    x,  n, p=NULL, 
    alternative = c("two.sided", "less", "greater"), 
    conf.level = 0.95, success=NULL, ..., data = NULL, data.name) 
  {
    if (! is.null(data)) 
      stop( "binom.test: If data is not NULL, first argument should be a formula.")
    
    if (missing(data.name)) { 
      data.name <- rlang::enexpr(x)
    }
    if (is.list(data.name)) { 
      data.name <- data.name$x 
    }
    prop_test(x=factor(x, levels=c('TRUE','FALSE')), p=p, alternative=alternative, 
              conf.level=conf.level, 
              success=success, 
              data.name=data.name, ...)
  }

#' @export
prop_test.factor <-
  function(
    x,  n, p = NULL, 
    alternative = c("two.sided", "less", "greater"), 
    conf.level = 0.95, success = NULL, ..., data = NULL, data.name) 
  {
    if (! is.null(data)) 
      stop( "binom.test: If data is not NULL, first argument should be a formula.")
    
    if (missing(data.name)) { 
      data.name <- rlang::enexpr(x)
    }
    if (is.list(data.name)) { 
      data.name <- data.name$x 
    }
    if (is.null(success)) {
      success <- levels(x)[1]
    }
    x <- x [!is.na(x)]
    count <- sum(x==success)
    n <- length(x)
    result <- stats::prop.test( x=count, n=n , p = p,
                                alternative = alternative,
                                conf.level = conf.level, ...) 
    result$data.name <- data.name
    if (!is.null(success)) 
      result$data.name <- 
      paste0(data.name, "  [with success = ", success, "]")
    return(result)
  }
