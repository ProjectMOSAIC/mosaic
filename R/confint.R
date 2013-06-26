#' Confidence interval methods for output of resampling
#' 
#' Methods for \code{confint} to compute confidence intervals
#' on numerical vectors and numerical components of data frames.
#' @rdname confint
#'
#' @method confint numeric
#' @param object The data frame or numerical vector.
#' @param parm not used -- for compatibility with other confint methods
#' @param level confidence level (default 0.95)
#' @param \dots additional arguments (currently ignored)
#' @param method either "stderr" (default) or "quantile"
#' @param margin if true, report intervals as a center and margin of error.
#'
#' @return When applied to a data frame, returns a data frame giving the 
#' confidence interval for each variable in the data frame using 
#' \code{t.test} or \code{binom.test}, unless the data frame was produced using \code{do}, in which case
#' it is assumed that each variable contains resampled statistics that serve as an estimated sampling
#' distribution from which a confidence interval can be computed using either a central proportion
#' of this distribution or using the standard error as estimated by the standard deviation of the 
#' estimated sampling distribution.  When applied to 
#' a numerical vector, returns a vector.
#' 
#' @examples
#' s <- do(500)*mean( age ~ sex, data=resample(HELPrct) )
#' confint(s)
#' confint(s, method="quantile")
#' confint(s, margin=TRUE)
#' confint(s, margin=TRUE, level=0.99 )
#' s2 <- do(500)*mean( resample(1:10) ) 
#' confint(s2)
# ==================
confint.numeric = function(object, parm, level=0.95, ..., method=c("stderr", "quantile"),margin=FALSE) {
  vals = .mosaic.get.ci( object, level, method[1] )
  if( margin ) return( c(center=mean(vals), margin.of.error=diff(vals)/2) )
  else return(vals)
}
# =================
#' @rdname confint
#' @method confint do.data.frame
confint.do.data.frame = function(object, parm, level=0.95, ..., method=c("stderr", "quantile"), margin=FALSE) {
  method <- match.arg(method) # which method was selected
  nms <- names(object)
  n <- length(nms)
  res <- data.frame( name=rep(NA,n), lower=rep(NA,n), upper=rep(NA,n) )
  for (k in 1:n ) {
    if (is.numeric( object[[nms[k]]] )) {
      vals <- .mosaic.get.ci( object[[nms[k]]], level, method)
      res$name[k] <- nms[k]
      res$lower[k] <- vals[1]
      res$upper[k] <- vals[2]
    }
  }
  res <- subset(res, !is.na(res$name) ) # get rid of non-quantitative variables
  if( margin ) {
    res <- data.frame(name=res$name, 
                     point=(res$upper+res$lower)/2, 
                     margin.of.error=(res$upper-res$lower)/2)
  }

  # Change the names to those given by confint.default
  colnames(res) <- 
    if (method=="quantile") 
      c("name", paste(c((1-level)/2, 1-(1-level)/2)*100, "%" ))
    else c("name", "lower", "upper")
  
  if (margin)  # Report as a center and margin of error
    res = .turn.to.margin(res)

  return( res )
}

.turn.to.margin = function(res) {
  data.frame( name=res$name, center=(res[[2]]+res[[3]])/2,
              margin.of.error=(res[[3]]-res[[2]])/2)
}
.mosaic.get.ci = function( vals, level, method ) {
  if( method == "stderr" ) res = mean(vals, na.rm=TRUE) + 
    c(-1,1)*sd(vals, na.rm=TRUE)*
    qt(1-(1-level)/2, sum(!is.na(vals))-1 )
  # the sum(!is.na(vals)) above is to account for NAs in finding the degrees of freedom
  else res = qdata( c((1-level)/2, 1-(1-level)/2), vals )
  return(res)
}

#' @rdname confint
#' @method confint data.frame
#' 
confint.data.frame = function(object, parm, level=0.95, ... )  {
  results <- list()
  for (c in 1:ncol(object)) {
    x <- object[,c]
    if (is.numeric(x)) { 
      newCI <- interval(t.test(x, ...))
      newRow <- data.frame( method="t.test", estimate=newCI[1], lower=newCI[2], upper=newCI[3], level=newCI[4])
      
    } else if ( (is.factor(x) && nlevels(x) <= 2) || (is.character(x) && length(unique(x)) <= 2) || is.logical(x)) { 
      newCI <- interval(binom.test(x, ...)) 
      newRow <- data.frame( method="binom.test", estimate=newCI[1], lower=newCI[2], upper=newCI[3], level=newCI[4])
    } else {
      newRow <- data.frame(method="none", estimate=NA, lower=NA, upper=NA, level=NA)
    }
    results <- rbind(results,newRow)
  }  
  
  row.names(results) <- names(object) 
  
  return(results)
}