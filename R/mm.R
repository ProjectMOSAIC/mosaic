#' 
#' Construct a model based on groupwise means
#' 
mm <- function(formula, data=parent.frame(), fun=mean, ... ) {
  evalF <- evalFormula(formula, data)
  # placeholder for the response values
  fitted <- vals <- evalF$left[[1]]
  
  # Find the case indices for the members of each group
  if (nrow(evalF$right)==1) { # just one group
    coefs <- c(all=fun( vals, ...))
    fitted[] <- coefs
    resids <- vals - fitted
  } else {  #multiple groups
    dups <- duplicated( evalF$right) # find redundant values
    inds <- split( 1:length(vals), evalF$right )
    for (k in 1:length(inds)) {
      fitted[ inds[[k]] ] <- fun( vals[inds[[k]]], ...)
    }
    resids <- vals - fitted
    coefs <- subset(evalF$right,!dups)
    coefs$value <- fitted[!dups]
  }
  res <- list( coefs=coefs, resids=resids, fitted=fitted, call=formula)
  class(res) <- c("groupwiseModel", "lm")
  return(res)
}

coef.groupwiseModel <- function(x) {
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

residuals.groupwiseModel <- function(object, type, ...) {object$resids}
fitted.groupwiseModel <- function(object, type, ...) {object$fitted}