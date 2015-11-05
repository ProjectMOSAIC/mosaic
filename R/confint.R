utils::globalVariables(c("SE.star", "estimate.star", ".index", "SE"))
                       
#' Confidence interval methods for output of resampling
#' 
#' Methods for \code{confint} to compute confidence intervals
#' on numerical vectors and numerical components of data frames.
#' 
#' @rdname confint
#' @name confint
#'
#' @param method a character vector of methods to use for creating confidence
#' intervals.  Choices are "percentile" (or "quantile") which is the default, 
#' "stderr" (or "se"), "bootstrap-t", and
#' "reverse" (or "basic"))
#'
#' @param margin.of.error if true, report intervals as a center and margin of error.
#' @param df degrees for freedom. This is required when \code{object} was produced using
#' \code{link{do}} when 
#' using the standard error to compute the confidence interval since 
#' typically this information is not recorded in these objects.  The default (\code{Inf})
#' uses a normal critical value rather than a one derived from a t-distribution.
#' 
#' @param ... additional arguments
#'
#' @return When applied to a data frame, returns a data frame giving the 
#' confidence interval for each variable in the data frame using 
#' \code{t.test} or \code{binom.test}, unless the data frame was produced using \code{do}, in which case
#' it is assumed that each variable contains resampled statistics that serve as an estimated sampling
#' distribution from which a confidence interval can be computed using either a central proportion
#' of this distribution or using the standard error as estimated by the standard deviation of the 
#' estimated sampling distribution.  For the standard error method, the user must supply the correct
#' degrees of freedom for the t distribution since this information is typically not available in
#' the output of \code{\link{do}}.
#' 
#' When applied to a numerical vector, returns a vector.
#' 
#' @details
#' The methods of producing confidence intervals from bootstrap distributions are currently
#' quite naive.  In particular, when using the standard error, assistance may be required with the 
#' degrees of freedom, and it may not be possible to provide a correct value in all situations.
#' None of the methods include explicit bias correction.
#' Let \eqn{q_a} be the \eqn{a} quantile of the bootstrap distribution,
#' let \eqn{t_a, df} be the \eqn{a} quantile of the t distribution with \eqn{df} 
#' degrees of freedom,
#' let \eqn{SE_b} be the standard deviation of the bootsrap distribution,
#' and let \eqn{\hat{\theta}} be the estimate computed from the original data.  
#' Then the confidence intervals with confidence level \eqn{1 - 2a} are
#' \describe{
#' \item{quantile}{\eqn{(q_a, q_{1-a}) } }
#' \item{reverse}{ \eqn{( 2 \hat{\theta} - q_{1-a}, 2\hat{\theta} - q_{a} )}}
#' \item{stderr}{\eqn{(\hat{\theta} - t_{1-a,df} SE_b, \hat{\theta} + t_{1-a,df} SE_b) }.
#'  When \code{df} is not provided,
#' at attempt is made to determine an appropriate value, but this should be double checked.
#' In particular, missing data an lead to unreliable results. 
#' }
#' The bootstrap-t confidence interval is computed much like the reverse confidence interval
#' but the bootstrap t distribution is used in place of a theoretical t distriubiton.  
#' This interval has much better properties than the reverse (or basic) method, which 
#' is here for comparison purposes only and is not recommended.
#' }
#' @references
#' Tim C. Hesterberg (2015): What Teachers Should Know about the Bootstrap: 
#' Resampling in the Undergraduate Statistics Curriculum, 
#' The American Statistician, 
#' \url{http://dx.doi.org/10.1080/00031305.2015.1089789}.
#' 
#' @examples
#' if (require(mosaicData)) {
#'   bootstrap <- do(500) * diffmean( age ~ sex, data=resample(HELPrct) )
#'   confint(bootstrap)
#'   confint(bootstrap, method = "percentile")
#'   confint(bootstrap, method = "boot")
#'   confint(bootstrap, method = "se", df=nrow(HELPrct) - 1)
#'   confint(bootstrap, margin.of.error = FALSE)
#'   confint(bootstrap, margin.of.error = TRUE, level=0.99, method=c("boot", "se", "perc") )
#'   bootstrap2 <- do(500)*mean( resample(1:10) ) 
#'   confint(bootstrap2)
#' }
#' @export

confint.numeric <- function(object, parm, level=0.95, ..., method="stderr", 
                           margin.of.error="stderr" %in% method=="stderr") {
  method <- match.arg(method, c("stderr","percentile","quantile"), several.ok=TRUE)
  result <- list()
  for (m in method) {
    vals <- .mosaic.get.ci( object, level, m )
    result[[m]] <-  if( margin.of.error ) { 
      c(center=mean(vals), margin.of.error=diff(vals)/2, method=m, level=level)  
    } else {
      vals
    }
  }
  if (length(result) > 1L) {
    result <- result[[1L]]
  }  else {
    result <- as.data.frame(do.call(rbind, result))
  }
  result  
}

dont_randomize_data <- list(
  mean = function(x, ...) x,
  median = function(x, ...) x,
  sd = function(x, ...) x,
  max = function(x, ...) x,
  min = function(x, ...) x,
  sample = function(x, ...) x,
  resample = function(x, ...) 
    if ( inherits(x, "lm") ) eval( x$call[["data"]], environment(formula(x)) ) else x,
  shuffle = function(x, ...) x,
  rflip = function(n, prob = 0.5, quiet, verbose, ...) {
    c( x = round(prob * n), n = n ) 
  }
)

dont_randomize_estimate <- list(
  sample = function(x, ...) x,
  resample = function(x, ...) 
    if ( inherits(x, "lm") ) eval( x$call[["data"]], environment(formula(x)) ) else x,
  shuffle = function(x, ...) x,
  rflip = function(n, prob = 0.5, quiet, verbose, ...) {
    c(prop = prob)
  }
)

extract_data <- function(x) {
  x_lazy <- attr(x, "lazy")
  if (is.null(x_lazy)) return(NULL)
  res <- eval( x_lazy$expr[["data"]], envir = dont_randomize_data, enclos = x_lazy$env )
  if (is.null(res)) {
    res <- eval( x_lazy$expr, envir = dont_randomize_data, enclos = x_lazy$env )
  }
  as.data.frame(res)
}

extract_estimate <- function(x) {
  x_lazy <- attr(x, "lazy")
  if (is.null(x_lazy)) return(NA)
  eval( x_lazy$expr, envir = dont_randomize_estimate, enclos = x_lazy$env )
}


#' @rdname confint
#' @export
confint.do.data.frame <- function(object, parm, level=0.95, ..., 
                                 method="stderr", 
                                 margin.of.error="stderr" %in% method,
                                 df = NULL) {
 
  method <- tolower(method) 
  method <- 
    match.arg(method, 
              c("percentile", "se", "stderr", "basic", "reverse", "quantile", "bootstrap-t"), 
                      several.ok=TRUE) # which method was selected
  method[method=="quantile"] <- "percentile"
  method[method=="basic"] <- "reverse"
  method[method=='se'] <- 'stderr'
  method <- unique(method)
  
  bootT <- ("bootstrap-t" %in% method) & (attr(object, "lazy")$expr[[1]] == "favstats")
  method <- setdiff(method, "bootstrap-t")
  
  compute_t_df <-
    grepl("^diffmean$|^mean$", as.character(attr(object, "lazy")$expr[[1]]))
  
  if ("stderr" %in% method && is.null(df) && compute_t_df) {
    tryCatch({
      orig_data <- extract_data(object) 
      orig_data <- 
        orig_data %>%
        select_(
          .dots = intersect(names(orig_data), 
                            attr(object, "lazy")$expr %>% all.vars())
        )
      df <- nrow(orig_data) - 1
      if ( ! all(complete.cases(orig_data)) ) {
        warning(
          "confint: Some missingness in the data. Check to make sure df is correct.",
          call. = FALSE)
      }
    },
      error = function(e) warning(
        "confint: I can't determine df from the original data.",
        call. = FALSE) 
    )
  }
  
  if (is.null(df) || length(df) != 1) {
    df <- Inf
    if ("stderr" %in% method) warning("confint: Using df=Inf.", call. = FALSE)
  }
  
  if (missing(parm)) parm <- names(object)
  nms <- intersect(names(object),parm)
  res <- data.frame( matrix( nrow=0, ncol=5) )
  names(res) <- c("name", "lower","upper","level","method")
  row <- 0
  culler <- attr(object, "culler")
  estimate <- culler(extract_estimate(object))
  if (is.null(names(estimate))) {
    names(estimate) <- names(object)
  }
  
  for (k in 1:length(nms) ) {
    for (m in method) {
      for (l in level) {
        if (is.numeric( object[[nms[k]]] )) {
          if (! nms[k] %in% names(estimate) ) next
          row <- row + 1
          # vals <- .mosaic.get.ci2( object[[nms[k]]], l, m, df=df)
          vals <- 
            bootstrap_ci( 
              object[[nms[k]]], level = l, method = m, df=df, 
              estimate =  estimate[[ nms[k] ]]
            )
          res[row, "name"] <- nms[k]
          res[row, "estimate"] <- estimate[[ nms[k] ]]
          res[row, "lower"] <- vals[1]
          res[row, "upper"] <- vals[2]
          res[row, "level"] <- l
          res[row, "method"] <- m
        }
      }
    }
  }
  if (bootT) {
    res <- bind_rows(res, boott(object))
  }
  if (prod(dim(res)) == 0L) {
    warning("confint: Unable to compute any of the desired CIs", call. = FALSE)
    return(res)
  }
#  res <- subset(res, !is.na(res$name) ) # get rid of non-quantitative variables
  if( margin.of.error && prod(dim(res)) > 0 ) {
#     res[, "estimate"] <- with( res, (upper+lower)/2 )
    res[, "margin.of.error"] <- with( res,  (res$upper-res$lower)/2 )
#    res[ res$method!="stderr", "estimate"] <- NA
    res[ res$method!="stderr", "margin.of.error"] <- NA
  } 
#  else {
#    res <- res[ , setdiff(names(res), "estimate") ]
#  }

  # Change the names to those given by confint.default
#  colnames(res) <- 
#    if (method=="quantile") 
#      c("name", paste(c((1-level)/2, 1-(1-level)/2)*100, "%" ))
#    else c("name", "lower", "upper")
  
#  if (margin.of.error)  # Report as a center and margin of error
#    res = .turn.to.margin(res)

  if (("stderr" %in% method) && (df < Inf)) {
    res$df = df
    res$df[res$method != "stderr"] <- NA
  }
  res <- res %>% filter( !is.na(lower) & !is.na(upper) & !(lower == upper) )
  return( as.data.frame(res) )
}

.turn.to.margin <- function(res) {
  data.frame( name=res$name, center=(res[[2]]+res[[3]])/2,
              margin.of.error=(res[[3]]-res[[2]])/2)
}

.mosaic.get.ci <- function( vals, level, method, df=NULL) {
  alpha <- (1-level)/2
  if( method == "stderr" ) {
    if (is.null(df)) { df <- sum(!is.na(vals)) - 1 }
    res = mean(vals, na.rm=TRUE) + 
      c(-1,1) * sd(vals, na.rm=TRUE) * qt(1-alpha, df)
  }
  # the sum(!is.na(vals)) above is to account for NAs in finding the degrees of freedom
  else res = stats::quantile(vals, c(alpha, 1-alpha) )
  return(res)
}

.mosaic.get.ci2 <- function( vals, level, method, df=Inf) {
  alpha <- (1 - level) / 2
  if( method == "stderr" ) {
    res = mean(vals, na.rm=TRUE) + 
    c(-1,1) * sd(vals, na.rm=TRUE) * qt(1-alpha, df)
  }
  # the sum(!is.na(vals)) above is to account for NAs in finding the degrees of freedom
  else res = stats::quantile(vals, c(alpha, 1-alpha) )
  return(res)
}

bootstrap_ci <- function( x, level = 0.95, method, 
                          df = Inf, t, ... ) {
  switch(
    method,
    `stderr` = tse_bootstrap_ci(x, level = level, df=df, ...),
    `percentile` = percentile_bootstrap_ci(x, level = level, ...),
    `reverse` = basic_bootstrap_ci(x, level = level, ...),
    `bootstrap-t` = boott_bootstrap_ci(x, level = level, estimate, ...)
  )
}
  
basic_bootstrap_ci <- function( x, estimate, ..., df = Inf, level = 0.95 ) {
  alpha <- (1 - level) / 2
  2 * estimate - stats::quantile(x, c(1-alpha, alpha))
}

percentile_bootstrap_ci <- function( x, ..., level = 0.95 ) {
  alpha <- (1 - level) / 2
  stats::quantile(x, c(alpha, 1-alpha) )
}

tse_bootstrap_ci <- function( x, ..., df = Inf, level = 0.95 ) {
  alpha <- (1 - level) / 2
  mean(x, na.rm=TRUE) + 
    c(-1,1) * sd(x, na.rm=TRUE) * qt(1-alpha, df)
}

boott_bootstrap_ci <- function( x, se, ..., level = 0.95, estimate ) {
  alpha <- (1-level)/2
  t <- (x - mean(x)) / se
  q <- stats::quantile(t, c(hi = 1 - alpha, lo=alpha), na.rm = TRUE)
  as.vector(mean(x, na.rm = TRUE) - q * SE)
}



#' @rdname confint
#' @export
 
confint.data.frame <- function(object, parm, level=0.95, ... )  {
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

boott <- function(object, ...) {
  UseMethod("boott")
}

boott.do.data.frame <- function( object, level = 0.95, ... ) {
  lz <- attr(object, "lazy")
  if ( ! lz$expr[[1]] == "favstats") stop( "Invalid object." )
  if (base::max(object$.row) > 2) stop("Too many groups.")
 
  estimate <- extract_estimate(object)$mean
  
  if (max(object$.row) == 1) {
    parm <- "mean"
    Boot <-
      object %>%
      mutate(estimate.star = mean, SE.star = sd / n, 
             t = (estimate.star - mean(estimate.star)) / SE.star)
  } 
  
  if (max(object$.row) == 2) {
    parm <- "diffmean"
    Boot <-
      object %>%
      group_by(.index) %>%
      summarise(estimate.star = - diff(mean), SE.star = sqrt( sum( sd^2/ n))) %>% 
      mutate(t = (estimate.star - mean(estimate.star)) / SE.star)
    estimate <- - diff(estimate)
  }
  
  q <- cdata( ~ t, level, data = Boot)
  res <-
    data.frame(
      name = parm,
      estimate = estimate,
      lower = estimate - q[2] * sd(~ estimate.star, data = Boot),
      upper = estimate - q[1] * sd(~ estimate.star, data = Boot),
      level = level,
      method = "bootstrap-t"
    )
  row.names(res) <- NULL
  as.data.frame(res)
}