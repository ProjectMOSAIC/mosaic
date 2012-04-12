#' 
#' Construct a model based on groupwise means
#' 
#' Calculate groupwise means, presenting the result as a model in the style of \code{lm}.
#' 
#' @rdname mm
#' 
#' @param formula A formula.  The left-hand side specifies the variable over
#' which the mean will be taken.  The right-hand side gives the grouping variables, 
#' separated by \code{&}.
#' 
#' @param data A data frame to which the formula variables refer.  If not specified, variables 
#' will be taken from the current environment.
#' 
#' @param fun The function used to calculate the means.  Default: \code{mean}.
#' 
#' @param drop Logical flag indicating whether to drop unoccupied groups.  Default \code{TRUE}.  
#' NOT YET IMPLEMENTED.
#' 
#' @param \dots Additional arguments to be passed to the \code{fun} doing the calculation.
#' 
#' @return \code{mm} returns an object of class \code{groupwiseModel}.  The functions 
#' \code{fitted.values}, \code{residuals}, \code{coefficients}, and \code{summary} 
#' are useful for extracting various features of the value returned by \code{mm}
#' 
#' @details \code{mm} is a sort of training function for \code{lm}, meant to provide a 
#' basis for discussing inference and introducing resampling in a simple, intuitive setting 
#' of groupwise means.  \code{lm} provides a better, more general facility. When using
#' \code{lm} to recreate the results of \code{mm}, include all the interaction terms, 
#' that is, use \code{*} instead of \code{&}.  See the examples.
#' 
#' @examples
#' mm( wage ~ sex, data=CPS )
#' mm( wage ~ sex & married, data=CPS )
#' lm( wage ~ sex*married-1, data=CPS)
#' do(5) * mm( wage ~ sex & married, data=resample(CPS))
#' mod <- mm( width ~ domhand, data=KidsFeet)
#' summary(mod)
#' resid(mod)
#' fitted(mod)
#' 
#' @seealso 
#' \code{\link{lm}}, 
#' \code{\link{do}}
#' 
mm <- function(formula, data=parent.frame(), fun=mean, drop=TRUE, ... ) {
  evalF <- evalFormula(formula, data)
  # placeholder for the response values
  fitted <- vals <- evalF$left[[1]]
  
  # Find the case indices for the members of each group
  if (nrow(evalF$right)==1) { # just one group, so a grand mean
    coefs <- c(all=fun( vals, ...))
    fitted[] <- coefs
    resids <- vals - fitted
    df <- 1
  } else {  #multiple groups
    dups <- duplicated( evalF$right) # find redundant values
    inds <- split( 1:length(vals), evalF$right )
    for (k in 1:length(inds)) {
      fitted[ inds[[k]] ] <- fun( vals[inds[[k]]], ...)
    }
    resids <- vals - fitted
    coefs <- subset(evalF$right,!dups)
    coefs$value <- fitted[!dups]
    df <- sum(!dups)
  }
  res <- list( coefs=coefs, resids=resids, fitted=fitted, call=formula, df=df)
  class(res) <- c("groupwiseModel", "lm")
  return(res)
}
#' @rdname mm
#' @method coefficients groupwiseModel
coefficients.groupwiseModel <- function(x) {
  x <- x$coefs
  if( is.numeric(x)) return(x)
  v <- x$value
  nms <- paste( names(x)[1], x[[1]], sep="")
  if (length(x) > 2) for (k in 2:(length(x)-1)) {
    nms <- paste(nms, paste(names(x)[k],x[[k]],sep=""), sep=":")
  }
  names(v) <- nms
  return(v)
}
# Methods
#' @rdname mm
#' @method print groupwiseModel
print.groupwiseModel <- function(x, ... ) {
  digits = max(3, getOption("digits")-3)
  cat("Groupwise Model.")
  print.lm(x, ..., digits=digits )
}
#' @rdname mm
#' @method residuals groupwiseModel
residuals.groupwiseModel <- function(object, ...) {object$resids}
#' @rdname mm
#' @method fitted groupwiseModel
fitted.groupwiseModel <- function(object, ...) {object$fitted}
#' @rdname mm
#' @method summary groupwiseModel
summary.groupwiseModel <- function(object, ... ){
  resids <- resid(object)
  sigma <- sqrt(sum(resids^2)/(length(resids)-object$df))
  r2 <- var(resids)/var(resids+fitted(object))
  ar2 <- r2*(length(resids)-1)/(length(resids)-object$df)
  res <- list(sigma=sigma,r.squared=1-r2,call=object$call,adj.r.squared=1-ar2,
              df=object$df)
  class(res) <- "summary.groupwiseModel"
  return(res)
}
#' @rdname mm
#' @method print summary.groupwiseModel
print.summary.groupwiseModel <- function(x, ...) {
  digits = max(3, getOption("digits")-3)
  cat("Groupwise Model\n")
  cat(paste("Call: ", deparse(x$call), "\n"))
  cat(paste("sigma: ", signif(x$sigma, digits=digits),"\n"))
  cat(paste("r-squared:", signif(x$r.squared, digits=digits),"\n"))
  cat(paste("Adj. r-squared:", signif(x$adj.r.squared, digits=digits),"\n"))
}
