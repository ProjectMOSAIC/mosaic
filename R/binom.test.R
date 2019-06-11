#' Exact Tests for Proportions
#' 
#' The `binom.test()` function
#' performs an exact test of a simple null hypothesis about the probability of success in a 
#' Bernoulli experiment from summarized data or from raw data.
#' The mosaic `binom.test` provides wrapper functions around the function of the same name in \pkg{stats}.
#' These wrappers provide an extended interface (including formulas).  
#' 
# @usage binom.test( x, n, p = 0.5, alternative = c("two.sided", "less", "greater"), conf.level = 0.95,...) 
#'
#' @param x  count of successes, length 2 vector of success and failure counts, a formula,
#'   			or a character, numeric, or factor vector containing raw data.
#' 
#' @param n  sample size (successes + failures) or a data frame 
#'   (for the formula interface) 
#' @param p probability for null hypothesis 
#' @param alternative  type of alternative hypothesis 
#' @param conf.level  confidence level for confidence interval 
#' @param success  level of variable to be considered success.  All other levels are 
#'   	considered failure.
#' @param data a data frame (if missing, `n` may be a data frame)
#' @param ci.method a method to use for computing the confidence interval 
#'   (case insensitive and may be abbreviated).  See details below.
#' @param ... additional arguments (often ignored) 
#' 
#' @return an object of class `htest`
#' 
#' @note When `x` is a 0-1 vector, 0 is treated as failure and 1 as success. Similarly,
#' for a logical vector `TRUE` is treated as success and `FALSE` as failure.
#' 
#'
#' @seealso [mosaic::prop.test()], [stats::binom.test()]
#' 

#' @details 
#' `binom.test()` is a wrapper around [stats::binom.test()] from the stats 
#' package to simplify its use when the raw data are available, in which case 
#' an extended syntax for `binom.test()` is provided.  See the examples.
#' 
#' Also, five confidence interval methods are provided:
#' * 
#' `"Clopper-Pearson", "binom.test"`: This is the interval produced when using 
#'   [stats::binom.test()]
#'   from the stats package.  It guarantees a coverage rate at least as large as 
#'   the nominal coverage rate, but may produce wider intervals than some of the methods
#'   below, which may either under- or over-cover depending on the data.
#' * `"Score", "Wilson", "prop.test":  This is the usual method used by [stats::prop.test()]
#'   and is computed by inverting p-values from score tests. It is often attributed to 
#'   Edwin Wilson.  If specified with \code{"prop.test"}, the continuity correction is applied
#'   (as is the default in \code{\link{prop.test}()}), else the continuity correction is not 
#'   applied.
#'   * `"Wald"` This is the interval traditionally taught in entry level statistics courses.
#'   It uses the sample proportion to estimate the standard error and uses normal
#'   theory to determine how many standard deviations to add and/or subtract from
#'   the sample proportion to determine an interval.
#'   * \"Agresti-Coull"`
#'   This is the Wald method after setting \eqn{n' = n + z^2} and 
#'   \eqn{p'= (x + z^2/2) / n}' and using \eqn{x' = n' p'} and \eqn{n'}
#'   in place of \eqn{x} and \eqn{n}.
#'   
#'   * `"Plus4"`
#'   This is Wald after adding in two artificial success and two artificial failures.  It 
#'   is nearly the same as the Agresti-Coull method when the confidence level is 95%. since
#'   \eqn{z^2} is approximately 4 and \eqn{z^2/2} is approximately 2.
#'   
#' 
#' @examples
#' # Several ways to get a confidence interval for the proportion of Old Faithful
#' # eruptions lasting more than 3 minutes.
#' data(faithful)
#' binom.test(faithful$eruptions > 3)
#' binom.test(97, 272)
#' binom.test(c(97, 272-97))
#' faithful$long <- faithful$eruptions > 3
#' binom.test(faithful$long)
#' binom.test(resample(1:4, 400), p=.25)
#' binom.test(~ long, data = faithful)
#' binom.test(~ long, data = faithful, ci.method = "Wald")
#' binom.test(~ long, data = faithful, ci.method = "Plus4")
#' with(faithful, binom.test(~long))
#' with(faithful, binom.test(long))
#' 
#' @keywords stats
#' 
#' @rdname binom.test
#' @export
binom.test <- 
  function( 
    x, n = NULL, p = 0.5, 
    alternative = c("two.sided", "less", "greater"), 
    conf.level = 0.95, 
    ci.method = c("Clopper-Pearson", "binom.test", "Score", "Wilson", "prop.test", "Wald", "Agresti-Coull", "Plus4"), 
    data = NULL,
    success = NULL,
    ...) 
  {
    missing_n <- !is.null(n)
    x_lazy <- rlang::enquo(x)
    x_eval <- 
      tryCatch(
        rlang::eval_tidy(x_lazy, data),
        error = function(e) {
          if (is.null(data) && ! missing_n) {
            stop("binom.test(): Improper `n'; did you forget `data =' perhaps?", call. = FALSE) 
          }
          lazyeval::f_rhs(x_lazy)
        }
      )
    
    # this list will later be converted to a string using the appropriate information
    # dependent upon which of the binom_test methods is called.  
    
    data.name <- list(x = lazyeval::expr_text(x), 
                      n = lazyeval::expr_text(n), 
                      data = lazyeval::expr_text(data))
    
    ci.method <- 
      match.arg(
        tolower(ci.method)[1], 
        choices = c("clopper-pearson", "binom.test", "prop.test", "score", "wilson", 
                    "wald", "agresti-coull", "plus4"))
    
    if (ci.method %in% c("wilson")) ci.method <- "score"
    if (ci.method %in% c("binom.test")) ci.method <- "clopper-pearson"
    
    res <- update_ci(
      binom_test( x = x_eval,
                  n = n,
                  p = p,
                  alternative = alternative, 
                  conf.level = conf.level, 
                  data.name = data.name,    # ignored by some methods, used by others
                  data = data,
                  success = success,
                  ...),
      method = ci.method
    )
    
    res
  }

setGeneric(
  "binom_test",
  function( x, n = NULL, p = 0.5, 
            alternative = c("two.sided", "less", "greater"), 
            conf.level = 0.95, ...) 
  {
    standardGeneric('binom_test')
  }
)

## @aliases binom_test,ANY-method
setMethod(
  'binom_test',
  'ANY',
  function(
    x, n, p = 0.5, 
    alternative = c("two.sided", "less", "greater"), 
    conf.level = 0.95, success = NULL, ..., data = NULL, data.name) 
  {
    stats::binom.test( x = x, n = n , p = p,
                       alternative = alternative,
                       conf.level = conf.level,...) 
  }
)

## @aliases binom_test,formula-method
setMethod(
  'binom_test',
  'formula',
  function(
    x, n = NULL, p = 0.5, 
    alternative = c("two.sided", "less", "greater"), 
    conf.level = 0.95, success = NULL, ..., data = NULL, data.name) 
  {
    if (is.null(data)) {
      if (! is.null(n)) stop("Improper `n'; did you forget `data = ' perhaps?", call. = FALSE)
      data <- lazyeval::f_env(x)
    }
    
    formula <- mosaic_formula_q(x, groups = NULL, max.slots = 1)
    dots <- list(...)
    
    form <- lattice::latticeParseFormula(formula, data, 
                                         subscripts = TRUE, drop = TRUE)
    if (missing(data.name)) {
      data.name <- paste( lazyeval::expr_text(data), "$", 
                          form$right.name,  sep="" )
    } 
    if (is.list(data.name)) {
      data.name <- paste( data.name$data, "$", 
                          form$right.name,  sep="" )
    }
    # now data.name should be set and data should hold the data
    subscr <- form$subscr
    cond <- form$condition
    x <- form$right
    if (length(cond) == 0) {
      cond <- list(gl(1, length(x)))
    }
    
    binom_test(x, p = p, alternative = alternative, 
               conf.level = conf.level, success = success, data.name = data.name, ...)
  }
)

##  @aliases binom_test,numeric-method
setMethod(
  'binom_test',
  'numeric',
  function( x,  n = NULL, p = 0.5, 
            alternative = c("two.sided", "less", "greater"), 
            conf.level = 0.95, success = NULL, ..., data = NULL, data.name) 
  {
    if (! is.null(data)) stop( "binom.test: If data is not NULL, first argument should be a formula.")
    
    if ( length(x) == 1 ) {
      result <-  stats::binom.test(x = x, n = n, p = p, alternative = alternative,
                                   conf.level = conf.level) 
      if (is.list(data.name)) {  ### check this VV
        result$data.name <- paste(data.name$x, "out of", data.name$n)
      } else {
        result$data.name <- paste(lazyeval::expr_text(x), "out of", lazyeval::expr_text(n))
      }
      return(result)
    }
    
    if ( length(x) == 2 ) {
      result <-  stats::binom.test(x=x[1], n=base::sum(x), p=p, alternative=alternative,
                                   conf.level=conf.level) 
      if (is.list(data.name)) {
        result$data.name <- data.name$x # deparse(lazyeval::f_rhs(data.name$x))
      } else {
        result$data.name <- lazyeval::expr_text(x)
      }
      return(result)
    }
    
    if (missing(data.name)) { 
      data.name <- lazyeval::expr_text(x)
    }
    if (is.list(data.name)) {
      data.name <- data.name$x  # deparse(lazyeval::f_rhs(data.name$x))
    }
    
    # set a reasonable value for success if none given
    if (is.null(success)) {
      success <- 
        if (all(x %in% c(0, 1))) 1 else
          if (0 %in% x) 0 else 
            min(x, na.rm=TRUE)
    }
    
    binom_test(x = factor(x), p = p, alternative = alternative, 
               conf.level = conf.level, 
               success = success, 
               data.name = data.name, ...)
  }
)

## @aliases binom_test,character-method
setMethod(
  'binom_test',
  'character',
  function(
    x,  n, p = 0.5, 
    alternative = c("two.sided", "less", "greater"), 
    conf.level = 0.95, success=NULL, ..., data = NULL, data.name) 
  {
    if (! is.null(data)) stop( "binom.test: If data is not NULL, first argument should be a formula.")
    
    if (missing(data.name)) { 
      data.name <- lazyeval::expr_text(x)
    }
    if (is.list(data.name)) {
      data.name <- data.name$x  # deparse(lazyeval::f_rhs(data.name$x)) 
    }
    binom_test(x = factor(x), p = p, alternative = alternative, 
               conf.level = conf.level, 
               success = success, 
               data.name = data.name, ...)
  }
)

## @aliases binom_test,logical-method
setMethod(
  'binom_test',
  'logical',
  function(
    x,  n, p = 0.5, 
    alternative = c("two.sided", "less", "greater"), 
    conf.level = 0.95, success = NULL, ..., data = NULL, data.name) 
  {
    if (! is.null(data)) stop( "binom.test: If data is not NULL, first argument should be a formula.")
    
    if (missing(data.name)) { 
      data.name <- lazyeval::expr_text(x)
    }
    if (is.list(data.name)) {
      data.name <- data.name$x  # deparse(lazyeval::f_rhs(data.name$x)) 
    }
    binom_test(x=factor(x, levels=c('TRUE','FALSE')), p=p, alternative=alternative, 
               conf.level=conf.level, 
               success=success, 
               data.name=data.name, ...)
  }
)

## @aliases binom_test,factor-method
setMethod(
  'binom_test',
  'factor',
  function(
    x,  n, p = 0.5, 
    alternative = c("two.sided", "less", "greater"), 
    conf.level = 0.95, success=NULL, ..., data = NULL, data.name) 
  {
    if (! is.null(data)) stop( "binom.test: If data is not NULL, first argument should be a formula.")
    
    if (missing(data.name)) { 
      data.name <- lazyeval::expr_text(x)
    }
    if (is.list(data.name)) { 
      data.name <- data.name$x  # deparse(lazyeval::f_rhs(data.name$x)) 
    }
    if ( missing(success) || is.null(success) ) {
      success <- levels(x)[1]
    }
    x <- x [!is.na(x)]
    count <- base::sum(x==success)
    n <- length(x)
    result <- stats::binom.test( x=count, n=n , p = p,
                                 alternative = alternative,
                                 conf.level = conf.level, ...) 
    result$data.name <- data.name
    if (!is.null(success)) 
      result$data.name <- 
      paste0(data.name, "  [with success = ", success, "]")
    return(result)
  }
)
