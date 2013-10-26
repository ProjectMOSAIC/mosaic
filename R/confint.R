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
#' @param method either "stderr" (default) or "quantile".  ("se" and "percentile" are 
#' allowed as aliases) or a vector containing both.
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
confint.numeric = function(object, parm, level=0.95, ..., method="stderr", 
                           margin="stderr" %in% method=="stderr") {
  method <- match.arg(method, c("stderr","percentile","quantile"), several.ok=TRUE)
  result <- list()
  for (m in method) {
    vals <- .mosaic.get.ci( object, level, m )
    result[[m]] <-  if( margin )  
      c(center=mean(vals), margin.of.error=diff(vals)/2, method=m, level=level)  else 
        vals
  }
  
  
}
# =================
#' @rdname confint
#' @method confint do.data.frame
confint.do.data.frame = function(object, parm, level=0.95, ..., 
                                 method="stderr", margin="stderr" %in% method) {
  method <- match.arg(method, c("se","stderr","percentile","quantile"), several.ok=TRUE) # which method was selected
  method[method=="percentile"] <- "quantile"
  method[method=='se'] <- 'stderr'
  method <- unique(method)
  
  if (missing(parm)) parm <- names(object)
  nms <- intersect(names(object),parm)
  n <- length(nms)
  res <- data.frame( matrix( nrow=0, ncol=5) )
  names(res) <- c("name", "lower","upper","level","method")
  row <- 0
  for (k in 1:n ) {
    for (m in method) {
      for (l in level) {
        if (is.numeric( object[[nms[k]]] )) {
          row <- row + 1
          vals <- .mosaic.get.ci( object[[nms[k]]], l, m)
          res[row,"name"] <- nms[k]
          res[row,"lower"] <- vals[1]
          res[row,"upper"] <- vals[2]
          res[row,"level"] <- l
          res[row,"method"] <- m
          res[row,"estimate"] <- if(m == "stderr") mean(vals) else mean(object[[nms[k]]], na.rm=TRUE)
        }
      }
    }
  }
#  res <- subset(res, !is.na(res$name) ) # get rid of non-quantitative variables
  if( margin ) {
#     res[, "estimate"] <- with( res, (upper+lower)/2 )
    res[, "margin.of.error"] <- with( res,  (res$upper-res$lower)/2 )
    res[ res$method!="stderr", "estimate"] <- NA
    res[ res$method!="stderr", "margin.of.error"] <- NA
  } else {
    res <- subset(res, select=-estimate)
  }

  # Change the names to those given by confint.default
#  colnames(res) <- 
#    if (method=="quantile") 
#      c("name", paste(c((1-level)/2, 1-(1-level)/2)*100, "%" ))
#    else c("name", "lower", "upper")
  
#  if (margin)  # Report as a center and margin of error
#    res = .turn.to.margin(res)

  return( res )
}

.turn.to.margin = function(res) {
  data.frame( name=res$name, center=(res[[2]]+res[[3]])/2,
              margin.of.error=(res[[3]]-res[[2]])/2)
}
.mosaic.get.ci = function( vals, level, method ) {
  alpha <- (1-level)/2
  if( method == "stderr" ) res = mean(vals, na.rm=TRUE) + 
    c(-1,1)*sd(vals, na.rm=TRUE)*
    qt(1-alpha, sum(!is.na(vals))-1 )
  # the sum(!is.na(vals)) above is to account for NAs in finding the degrees of freedom
  else res = qdata( c(alpha, 1-alpha), vals )
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
      newRow <- data.frame( method="t.test", estimate=newCI[1], lower=newCI[2], 
                            upper=newCI[3], level=newCI[4])
      
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