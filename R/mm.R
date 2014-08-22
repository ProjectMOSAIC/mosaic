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
#' if (require(mosaicData)) {
#' mm( wage ~ sex, data=CPS85 )
#' mm( wage ~ sex & married, data=CPS85 )
#' lm( wage ~ sex*married-1, data=CPS85)
#' do(5) * mm( wage ~ sex & married, data=resample(CPS85))
#' mod <- mm( width ~ domhand, data=KidsFeet)
#' summary(mod)
#' resid(mod)
#' fitted(mod)
#' }
#' 
#' @seealso 
#' \code{\link{lm}}, 
#' \code{\link{do}}
#' @export
 
mm <- function(formula, data=parent.frame(), fun=mean, drop=TRUE, ... ) {
  evalF <- evalFormula(formula, data)
  # placeholder for the response values
  fitted <- vals <- evalF$left[[1]]
  
  # Find the case indices for the members of each group
  if (nrow(evalF$right)==1) { # just one group, so a grand mean
    coefs <- c(all=fun( vals, ...))
    fitted[] <- coefs # replace all of them, that's why the []
    resids <- vals - fitted
    groupsd <- sd(resids)
    ncases <- length(resids)
    sdresids <- sd(resids)
    df <- 1
  } else {  #multiple groups
    dups <- duplicated( evalF$right) # find redundant values
    by.group <- split( 1:length(vals), evalF$right )
    ncases <- groupsd <- coefvals <- rep(0, length(by.group)) # placeholder variables
    for (k in 1:length(by.group)) {
      fitted[ by.group[[k]] ] <- coefvals[k] <- fun( vals[by.group[[k]]], ...)
      ncases[k] <- length(by.group[[k]]) # how many cases in each group
    }
    resids <- vals - fitted
    for (k in 1:length(by.group)) groupsd[k] <- sd(resids[by.group[[k]]])
    # Should we have the names match the output of mean(formula) or the output of lm()?
    coefs <- data.frame(group=names(mean(formula,data=data,...)))
    coefs$value <- coefvals
    df <- sum(!dups)
  }
  res <- structure( list(coefs=coefs, resids=resids, fitted=fitted, 
                         ncases=ncases, groupsd=groupsd, call=formula, df=df),
                    class = c("groupwiseModel")
  )
  return(res)
}
#' @rdname mm
#' @param parm Not used
#' @param level The confidence level (e.g., 0.95)
#' @param pooled Whether to use a pooled variance of residuals to compute the standard error. 
#' (This is what \code{lm} does.)
#' @param margin Whether to present the margin of error rather than the lower and upper bounds
#' @export

confint.groupwiseModel <- function(object, parm, level=0.95, ..., pooled=TRUE, margin=FALSE) {
  n <- length(object$fitted)
  # Find the standard error of each group
  mns <- object$coefs[[2]] # the groupwise means
  margin.of.error <- if (pooled) {
    sqrt( (n-1)/(n-object$df))* sd( object$resids) / sqrt(object$ncases) *
    abs(qt((1-level)/2, df=n-object$df))
  }
  else {
    (object$groupsd / sqrt(object$ncases))*abs(qt((1-level)/2, df=object$ncases-1))
  }
  
  if( margin ) {
    res <- data.frame(center=object$coefs, margin.of.error=margin.of.error)
    colnames(res) <- c("group", "center","margin.of.error")
  }
  else {
    res <- data.frame(name=object$coefs[[1]], L=mns-margin.of.error,R=mns+margin.of.error)
    # Change the names to those given by confint.default
    colnames(res) <- c("group", paste(c((1-level)/2, 1-(1-level)/2)*100, "%" ))
  }
  return(res)
}

#' @rdname mm
#' @export

coef.groupwiseModel <- function(object, ...) {
  x <- object$coefs
  if( is.numeric(x)) return(x)
  v <- x$value
  #nms <- paste( names(x)[1], x[[1]], sep="")
  #if (length(x) > 2) for (k in 2:(length(x)-1)) {
  #  nms <- paste(nms, paste(names(x)[k],x[[k]],sep=""), sep=":")
  #}
  #names(v) <- nms
  names(v) <- x$group
  return(v)
}
# Methods
#' @rdname mm
#' @param x Object to be printed
#' @param digits number of digits to display
#' @export
print.groupwiseModel <- function(x, ..., digits=max(3, getOption("digits") -3) ) {
  # directly copied from print.lm, but since this isn't an lm object, it
  # doesn't make sense to call print.lm on it.
  cat("\nGroupwise Model Call:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  if (length(coef(x))) {
    cat("Coefficients:\n")
    print(format(coef(x), digits = digits), print.gap = 2L, 
                  quote = FALSE)
  }
  else cat("No coefficients\n")
  cat("\n")
  invisible(x)
}
#' @rdname mm
#' @param object groupwiseMean object from which to extract the residuals
#' @export

residuals.groupwiseModel <- function(object, ...) {object$resids}
#' @rdname mm
#' @export

fitted.groupwiseModel <- function(object, ...) {object$fitted}
#' @rdname mm
#' @export
summary.groupwiseModel <- 
          function(object, ... ){
            resids <- resid(object)
            sigma <- sqrt(sum(resids^2)/(length(resids)-object$df))
            r2 <- var(resids)/var(resids+fitted(object))
            ar2 <- r2*(length(resids)-1)/(length(resids)-object$df)
            res <- structure( list(sigma=sigma,r.squared=1-r2,call=object$call,adj.r.squared=1-ar2,
                                   df=object$df,coefs=confint(object)), 
                              class= "summary_groupwiseModel"
            )
            return(res)
          }


#' @rdname mm
#' @export

print.summary_groupwiseModel <- function(x, digits = max(3, getOption("digits")-3), ...) {
  cat("Groupwise Model\n")
  cat(paste("Call: ", deparse(x$call), "\n"))
  cat("\n"); print(x$coefs); cat("\n")
  cat(paste("sigma: ", signif(x$sigma, digits=digits),"\n"))
  cat(paste("r-squared:", signif(x$r.squared, digits=digits),"\n"))
  cat(paste("Adj. r-squared:", signif(x$adj.r.squared, digits=digits),"\n"))
}
