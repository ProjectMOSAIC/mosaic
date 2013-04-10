# 
# issues to deal with
#   * var() is hybrid of 1-ary, 2-ary
#     * var( age ~ sex + substance, data=HELPrct ) doesn't work if using aggregatingFunction2()
#     * var( a, b, data=...) doesn't work if using agreggatingFunction1()
#   * var(), cov(), and cor() don't have ... argument
#   * don't currently support var ( ~ x | z )
#   * need to confirm scoping is correct when using programmatically and interactively.
# 
 
require(mosaic)
require(plr)
myaggregate <- function( formula, data, FUN ) {
  ddply( data, .(all.vars(rhs(formula))), summarise, val=FUN )
}

aggregatingFunction1 <- function( default )
  result <- function( x, ... ) {
    orig.call <- match.call() 
    base.call <- orig.call
    base.call[[1]] <- quote(default)
    mosaic.call <- orig.call 
    mosaic.call[[1]] <- quote(default)
   
    if ( ! .is.formula(eval(orig.call$x) ) && "data" %in% names(orig.call) )  {  
      mosaic.call[[1]] <- default
      return ( eval( mosaic.call , envir=list(...)[["data"]], enclos=parent.frame()) )
    }
    
    if ( ! .is.formula(eval(orig.call$x) ) ) {
      tryCatch( return( eval(base.call)), error=function(e) {})
    }
    message( "Using mosaic super powers!" )
    mosaic.call[[1]] <- quote(maggregate)
    mosaic.call$formula <- orig.call$x
    mosaic.call$x <- NULL
    mosaic.call$FUN <- default
    return( eval(mosaic.call) )
  }


aggregatingFunction2 <- function( default ) {
  result <- function( x, y=NULL, ... ) {
    orig.call <- match.call()
    base.call <- orig.call
    base.call[[1]] <- quote(default)
    if (! is.null(base.call[['data']]) ) base.call[['data']] <- NULL
    mosaic.call <- orig.call 
    mosaic.call[[1]] <- quote(default)
    
    if ( ! .is.formula(eval(orig.call$x) ) && "data" %in% names(orig.call) )  {  
      mosaic.call[[1]] <- default
      return ( eval( mosaic.call , envir=list(...)[["data"]], enclos=parent.frame()) )
    }
    
    if ( ! .is.formula(eval(orig.call$x) ) ) {
      tryCatch( return( eval(base.call)), error=function(e) {}) #  , warning= function(w) {} )
    }
    
    message( "Using mosaic super powers!" )
    mosaic.call[[1]] <- default
    formula <- eval(orig.call$x)
    if (is.null( mosaic.call[['data']] ) ) mosaic.call[['data']] <- quote(parent.frame())
    mosaic.call$x <- eval(rhs(formula), envir=eval(orig.call$data), enclos=parent.frame())
    mosaic.call$y <- eval(lhs(formula), envir=eval(orig.call$data), enclos=parent.frame())
    if (! "..." %in% names(formals(orig.call))) {
      for (n in setdiff( names(mosaic.call), names(formals(default))) ) {
        if (n != "") mosaic.call[[n]] <- NULL
      }
    }
    return( eval(mosaic.call) )
  }
  return(result)
}

mean <- aggregatingFunction1( base::mean )
sd <- aggregatingFunction1( stats::sd )
max <- aggregatingFunction1( base::max)
min <- aggregatingFunction1( base::min)
favstats <- aggregatingFunction1( mosaic::favstats)
var <- aggregatingFunction1( stats::var )
cor <- aggregatingFunction2( stats::cor )
cov <- aggregatingFunction2( stats::cov)

mean( HELPrct$age )
mean( ~ age, data=HELPrct )
mean( age ~ sex + substance, data=HELPrct )
mean( ~ age | sex + substance, data=HELPrct )
mean( sqrt(age), data=HELPrct )
sd( HELPrct$age )
sd( ~ age, data=HELPrct )
sd( age ~ sex + substance, data=HELPrct )
var( HELPrct$age )
var( ~ age, data=HELPrct )
var( age ~ sex + substance, data=HELPrct )

cor( length ~ width, data=KidsFeet )
var ( length ~ width, data=KidsFeet )
cov ( length ~ width, data=KidsFeet )