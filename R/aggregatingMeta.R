# 
# issues to deal with
#   * var() is hybrid of 1-ary, 2-ary
#     * var( age ~ sex + substance, data=HELPrct ) doesn't work if using aggregatingFunction2()
#     * var( a, b, data=...) doesn't work if using agreggatingFunction1()
#   * var(), cov(), and cor() don't have ... argument
#   * don't currently support var ( ~ x | z )
#   * need to confirm scoping is correct when using programmatically and interactively.
# 
 

#' 1-ary Aggregating functions
#' 
#' \code{aggregatinFuntion1} creates statistical summaries of one numerical vector that are formula aware.
#' 
#' @rdname aggregatingFunction1
#' @aliases aggregatingFunction1 
#' @param fun a function that takes a numeric vector and computes a summary statistic,
#' returning a numeric vector of length 1.
#' @param multiple a boolean indicating whether \code{..fun..} returns multiple values
#' @param envir an environment in which evaluation takes place.
#' @return a function that generalizes \code{fun} to handle a formula/data frame interface.
#' 
#' @export
#' @examples
#' foo <- aggregatingFunction1( base::mean )
#' foo( ~length, data=KidsFeet )
#' base::mean(KidsFeet$length)
#' foo( length ~ sex, data=KidsFeet )
aggregatingFunction1 <- function( fun, multiple=FALSE, envir=parent.frame() ) {
  result <- function( x, ..., data) {
    orig.call <- match.call()
    fun.call <- orig.call 
    fun.call[[1]] <- substitute(..fun..)
    fun.call[[2]] <- substitute(x)
    
    missingData <- FALSE  
    if ( missing(data) ) {
      missingData <- TRUE
      data <- parent.frame()  # redundant?

      result <- tryCatch( eval(fun.call, envir=parent.frame()) , 
                error=function(e) {e} ,
                warning=function(w) {w} ) 
      if ( ! inherits(result, "warning") && ! inherits(result,"error") ) 
        return(result) 
    }
    
    if (! .is.formula(x) ) {
      if (missingData) {
        data <- parent.frame()   # redundant?
      } else {
        fun.call[['data']] <- NULL
      }
      tryCatch( return( eval(fun.call, envir=data, enclos=parent.frame())  ) ,
                error = function(e) { stop(paste(e, "Did you perhaps omit data= ?")) } )
    }

    maggregate.call <- orig.call
    maggregate.call[[1]] <- quote(maggregate)
    maggregate.call$formula <- x
    maggregate.call$data <- data 
    maggregate.call$x <- NULL
    maggregate.call$FUN <- substitute(..fun..)
    maggregate.call$multiple <- multiple
    return( eval(maggregate.call, envir=envir) )
  }
  formals(result) <- c(formals(result), ..fun.. = substitute(fun))
  return(result)
}

#' 2-ary Aggregating functions
#' 
#' \code{aggregatinFuntion2} creates statistical summaries of two numerical vectors that are formula aware.
#' 

#' @rdname aggregatingFunction2
#' @aliases aggregatingFunction2 
#' @param fun a function that takes two numeric vectors and computes a summary statistic,
#' returning a numeric vector of length 1.
#' @return a function that generalizes \code{fun} to handle a formula/data frame interface.
#' 
#' @export
#' @examples
#' foo <- aggregatingFunction2( stats::cor)
#' foo( length ~ width, data=KidsFeet )
#' stats::cor( KidsFeet$length, KidsFeet$width )
aggregatingFunction2 <- function( fun ) {
  result <- function( x, y=NULL, ..., data=parent.frame() ) { # , ..fun.. = fun) {
    orig.call <- match.call()
    mosaic.call <- orig.call 
      mosaic.call[[1]] <- fun
    
    if ( #"data" %in% names(orig.call) && 
         ! .is.formula(eval(orig.call$x, parent.frame())) )  {  
      if (!'data' %in% names(formals(fun)) && ! "..." %in% names(formals(fun)) ) {
        if("data" %in% names(mosaic.call)) mosaic.call[["data"]] <- NULL  # in case original function didn't have ...
      }
      return ( eval( mosaic.call , data, enclos=parent.frame()) )
    }
    
    # message( "Using mosaic super powers!" )
    formula <- eval(orig.call$x, parent.frame())
    mosaic.call[['data']] <- NULL
    # if (is.null( mosaic.call[['data']] ) ) mosaic.call[['data']] <- quote(parent.frame())
    mosaic.call$x <- eval(lhs(formula), envir=data, enclos=parent.frame())
    mosaic.call$y <- eval(rhs(formula), envir=data, enclos=parent.frame())
    if (! "..." %in% names(formals(orig.call))) {
      for (n in setdiff( names(mosaic.call), names(formals(fun))) ) {
        if (n != "") mosaic.call[[n]] <- NULL
      }
    }
    return( eval(mosaic.call) )
  }
  assign("fun", fun, environment(result))
  return(result)
}

#' Aggregating functions
#' 
#' The \code{mosaic} package makes several summary statistic functions (like \code{mean} and \code{sd})
#' formula aware.
#' 
#' @rdname aggregating
#' @aliases sum min max mean median sd var cov cor favstats
#' @param x an object, often a formula
#' @param y an object, often a numeric vector 
#' @param ..fun.. the underlyin function used in the computation
#' @param data a data frame in which to evaluate formulas (or bare names)
#' @param \dots additional arguments
#' @export
mean <- aggregatingFunction1( base::mean )
#' @rdname aggregating
#' @export
median <- aggregatingFunction1( stats::median )
#' @rdname aggregating
#' @export
range <- aggregatingFunction1( base::range )
#' @rdname aggregating
#' @export
sd <- aggregatingFunction1( stats::sd )
#' @rdname aggregating
#' @export
max <- aggregatingFunction1( base::max)
#' @rdname aggregating
#' @export
min <- aggregatingFunction1( base::min)
#' @rdname aggregating
#' @export
sum <- aggregatingFunction1( base::sum)
#' @rdname aggregating
#' @export
IQR <- aggregatingFunction1( stats::IQR )
#' @rdname aggregating
#' @export
fivenum <- aggregatingFunction1( stats::fivenum)
#' @rdname aggregating
#' @export
iqr <- aggregatingFunction1( stats::IQR )
#' @rdname aggregating
#' @export
prod <- aggregatingFunction1( base::prod )
#' @rdname aggregating
#' @export
sum <- aggregatingFunction1( base::sum)
#' @rdname aggregating
#' @export
favstats <- aggregatingFunction1(fav_stats, multiple=TRUE)
#' @rdname aggregating
#' @export
var <- aggregatingFunction1( stats::var )
#' @rdname aggregating
#' @export
cor <- aggregatingFunction2( stats::cor )
#' @rdname aggregating
#' @export
#' 
#' @examples
#' mean( HELPrct$age )
#' mean( ~ age, data=HELPrct )
#' mean( age ~ sex + substance, data=HELPrct )
#' mean( ~ age | sex + substance, data=HELPrct )
#' mean( sqrt(age), data=HELPrct )
#' sum( ~ age, data=HELPrct )
#' sd( HELPrct$age )
#' sd( ~ age, data=HELPrct )
#' sd( age ~ sex + substance, data=HELPrct )
#' var( HELPrct$age )
#' var( ~ age, data=HELPrct )
#' var( age ~ sex + substance, data=HELPrct )
#' IQR( width ~ sex, data=KidsFeet )
#' iqr( width ~ sex, data=KidsFeet )
#' favstats( width ~ sex, data=KidsFeet )
#' 
#' cor( length ~ width, data=KidsFeet )
#' cov ( length ~ width, data=KidsFeet )

cov <- aggregatingFunction2( stats::cov)


