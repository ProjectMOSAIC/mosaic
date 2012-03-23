# Function to calculate odds ratios and confidence intervals
# on odds ratios.

# Written by Kevin Middleton

# Successes in Column 1
# Treatment of interest in Row 2



#' Odds Ratio for 2X2 Contingency Tables
#' 
#' This function calculates the odds ratio for a 2 X 2 contingency table and a
#' confidence interval (default \code{conf.level} is 95 percent) for the
#' estimated odds ratio. \code{x} should be a matrix, data frame or table. "Successes"
#' should be located in column 1 of \code{x}, and the treatment of interest
#' should be located in row 2. The odds ratio is calculated as (Odds row 2) /
#' (Odds row 1). The confidence interval is calculated from the log(OR) and
#' backtransformed.
#' 
#' 
#' @rdname oddsRatio
#' @param x a 2 X 2 matrix, data frame or table of counts
#' @param conf.level the confidence interval level
#' @return \item{p1, p2}{Proportions for rows 1 and 2} \item{o1, o2}{Odds for
#' rows 1 and 2} \item{OR}{Odds ratio} \item{lower}{the lower bound of the
#' confidence interval} \item{upper}{the upper bound of the confidence
#' interval} \item{conf.level}{the confidence interval level}
#' @author Kevin Middleton (\email{kmm@@csusb.edu})
#' @seealso \code{\link{chisq.test}}
#' @keywords univar
#' @export
#' @examples
#' M1 <- matrix(c(14, 38, 51, 11), nrow = 2)
#' M1
#' oddsRatio(M1)
#' 
#' M2 <- matrix(c(18515, 18496, 1427, 1438), nrow = 2)
#' rownames(M2) <- c("Placebo", "Aspirin")
#' colnames(M2) <- c("No", "Yes")
#' M2
#' oddsRatio(M2)
#' 
oddsRatio <- function(x, conf.level = 0.95){
  rowsums <- rowSums(x)
  p1 <- x[1, 1] / rowsums[1]
  p2 <- x[2, 1] / rowsums[2]
  o1 <- p1 / (1 - p1)
  o2 <- p2 / (1 - p2)
  OR <- o2 / o1
  log.OR <- log(OR)
  SE.log.OR <- sqrt(sum(1/x))
  crit <- qnorm((1 - conf.level)/2, lower.tail = FALSE)
  log.lower <- log.OR - crit * SE.log.OR
  log.upper <- log.OR + crit * SE.log.OR
  lower <- exp(log.lower)
  upper <- exp(log.upper)
  zz <- list(p1 = p1, p2 = p2, o1 = o1, o2 = o2, OR = OR, 
    lower = lower, upper = upper, conf.level = conf.level)
  class(zz) <- "oddsRatio"
  zz
}

#' @rdname oddsRatio
#' @method print oddsRatio
#' @param digits number of digits to display
#' @param \dots additional arguments
#' @export
print.oddsRatio <- function(x, digits = 4, ...){
  cat("\n")
  cat("Odds Ratio\n")
  cat("\n")
  cat("Proportions\n")
  cat("\tProp. 1:\t", format(x$p1, digits = digits), "\n")
  cat("\tProp. 2:\t", format(x$p2, digits = digits), "\n\n")
  cat("Odds\n")
  cat("\tOdds 1:\t\t", format(x$o1, digits = digits), "\n")
  cat("\tOdds 2:\t\t", format(x$o2, digits = digits), "\n\n")
  cat("Odds Ratio\n")
  cat("\tOdds Ratio:\t", format(x$OR, digits = digits), "\n\n")
  cat(format(100 * x$conf.level), "percent confidence interval:\n\t")
  cat(format(x$lower, digits = digits), "< OR <", format(x$upper, digits = digits), "\n")
}
