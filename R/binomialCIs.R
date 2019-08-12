
# based on similar function in fastR
wald_ci <- function (x, n = 100, conf.level = 0.95, 
                     alternative = c("two.sided", "less", "greater") ) 
{
  alternative <- match.arg(alternative)
  one_or_two <- if (alternative == "two.sided") 2 else 1
  
  alpha <- 1 - conf.level
  p <- x/n
  zstar <- -qnorm(alpha/one_or_two)
  interval <- p + c(-1, 1) * zstar * sqrt(p * (1 - p)/n)
  if (alternative == "less") interval[1] <- 0
  if (alternative == "greater") interval[2] <- 1
  attr(interval, "conf.level") <- conf.level
  attr(interval, "method") <- "Wald"
  return(interval)
}

# based on similar function in fastR
plus4_ci <- function(x, n, conf.level = 0.95, 
                     alternative = c("two.sided", "less", "greater") ) 
{
  alternative <- match.arg(alternative)
  one_or_two <- if (alternative == "two.sided") 2 else 1
  
  alpha <- (1 - conf.level) 
  p <- (x + 2)/(n + 4)
  zstar <- -qnorm(alpha/one_or_two)
  interval <- p + c(-1, 1) * zstar * sqrt(p * (1 - p)/(n + 4))
  if (alternative == "less" || interval[1] < 0) interval[1] <- 0
  if (alternative == "greater" || interval[2] > 1) interval[2] <- 1
  
  attr(interval, "conf.level") <- conf.level
  attr(interval, "method") <- "plus4"
  return(interval)
}

agresti_ci <- function (x, n, conf.level = 0.95,
                        alternative = c("two.sided", "less", "greater") )
  
{
  alternative <- match.arg(alternative)
  one_or_two <- if (alternative == "two.sided") 2 else 1
  
  alpha <- 1 - conf.level
  p <- (x + 2)/(n + 4)
  zstar <- -qnorm(alpha/one_or_two)
  z2 <- zstar^2
  n_ <- n + z2
  p_ <- (x + z2/2) / n_
  se_ <- sqrt( p_ * (1-p_) / n_ )
  interval <- p_ + c(-1,1) * zstar * se_
  if (alternative == "less" || interval[1] < 0) interval[1] <- 0
  if (alternative == "greater" || interval[2] > 1) interval[2] <- 1
  
  attr(interval, "conf.level") <- conf.level
  attr(interval, "method") <- "Agresti-Coull"
  return(interval)
}

clopper_ci <- function(x, n, conf.level = 0.95, 
                       alternative = c("two.sided", "less", "greater") ) 
{
  interval <- stats::binom.test(x, n, conf.level = conf.level, alternative = alternative)$conf.int
  attr(interval, "method") <- "Score"
  interval
}

score_ci <- function(x, n, conf.level = 0.95, 
                     alternative = c("two.sided", "less", "greater"),
                     correct = TRUE) 
{
  interval <- stats::prop.test(x, n, conf.level = conf.level, alternative = alternative,
                               correct = correct)$conf.int
  attr(interval, "method") <- 
    if (correct) "Score (with continuity correction)" else "Score (without continuity correction)"
  interval
}

#' Update confidence interval
#' 
#' Update the confidence interval portion of an object returned from
#' `binom.test` using one of several alternative methods.
#' 
#' @param object An `"htest"` object produced by [binom.test()]
#' @param method a method for computing a confidence interval for a proportion.
#' @return an `"htest"` object with an updated confidence interval
#' 
#' @export
#' @seealso [mosaic::binom.test()]
#' 
update_ci <- 
  function(
    object, 
    method = c("clopper-pearson", "wald", "agresti-coull", "plus4", "score", "prop.test")
  ) {
    if (! inherits(object, "htest") && !grepl("^Exact binomial test", object$method) )
      stop( "I don't know how to handle that type of object.")
    
    method <- match.arg(method)
    level <- attr(object$conf.int, "conf.level") 
    if ((level < 0.90 || level > .98) && method == "plus4")
      warning(
        paste("Plus 4 confidence intervals are designed for 95% confidence intervals.",
              "Consider using another method when `conf.level` is not close to 0.95.",
              collapse="\n")
      )
    
    
    object$conf.int <- 
      switch(method,
             `clopper-pearson` =  
               clopper_ci(x = object$statistic, 
                          n = object$parameter, 
                          conf.level = level,
                          alternative = object$alternative),
             wald =  
               wald_ci(x = object$statistic, 
                       n = object$parameter, 
                       conf.level = level,
                       alternative = object$alternative),
             plus4 = 
               plus4_ci(x = object$statistic, 
                        n = object$parameter, 
                        conf.level = level,
                        alternative = object$alternative),
             `agresti-coull` = 
               agresti_ci(x = object$statistic, 
                          n = object$parameter, 
                          conf.level = level,
                          alternative = object$alternative),
             score = 
               score_ci(x = object$statistic, 
                        n = object$parameter, 
                        conf.level = level, alternative = object$alternative,
                        correct = FALSE),
             wilson =   # same as score
               score_ci(x = object$statistic, 
                        n = object$parameter, 
                        conf.level = level, alternative = object$alternative,
                        correct = FALSE),
             prop.test = # continuity correction + score
               score_ci(x = object$statistic, 
                        n = object$parameter, 
                        conf.level = level, alternative = object$alternative,
                        correct = TRUE),
      )
    
    object$method <- 
      switch(method,
             wald = "Exact binomial test (Wald CI)",
             plus4 = "Exact binomial test (Plus 4 CI)",
             `agresti-coull` = "Exact binomial test (Agresti-Coull CI)",
             score = "Exact binomial test (Score CI without continuity correction)",
             prop.test = "Exact binomial test (Score CI with continuity correction)"
      )
    
    object
  }
