# Function to calculate odds ratios and confidence intervals
# on odds ratios.

# Written by Kevin Middleton; largely redone by R. Pruim

# Successes in Column 1
# Treatment of interest in Row 2



#' Odds Ratio and Relative Risk for 2 x 2 Contingency Tables
#' 
#' This function calculates the odds ratio and relative risk for a 2 x 2 
#' contingency table and a
#' confidence interval (default \code{conf.level} is 95 percent) for the
#' each estimate. \code{x} should be a matrix, data frame or table. "Successes"
#' should be located in column 1 of \code{x}, and the treatment of interest
#' should be located in row 2. The odds ratio is calculated as (Odds row 2) /
#' (Odds row 1). The confidence interval is calculated from the log(OR) and
#' backtransformed.
#' 
#' 
#' @rdname oddsRatio
#' @param x a 2 X 2 matrix, data frame or table of counts
#' @param object an R object to print or summarise.  Here an object of class
#' \code{"oddsRatio"} or \code{"relrisk"}.
#' @param conf.level the confidence interval level
#' @param verbose a logical indicating whether verbose output should be displayed
#' @param quiet a logical indicating whether verbose outoput should be supressed
#' @param relrisk a logical indicating whether the relative risk should be returned
#' instead of the odds ratio
#' @param digits number of digits to display

#' @param ... additional arguments
#' @return an odds ratio or relative risk.  If \code{verpose} is true,
#' more details and the confidence intervals are displayed.
#' @author Kevin Middleton (\email{kmm@@csusb.edu}); modified by 
#' R Pruim.
#' @seealso \code{\link{chisq.test}}, \code{\link{fisher.test}}
#' @keywords stats
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
#' oddsRatio(M2, verbose=TRUE)
#' relrisk(M2, verbose=TRUE)
#' relrisk(tally(~ homeless + sex, data=HELPrct) )
#' do(3) * relrisk( tally( ~ homeless + shuffle(sex), data=HELPrct) )
#' @export

orrr <- function(x, conf.level = 0.95, verbose=!quiet, quiet=TRUE, digits=3,
                      relrisk=FALSE){
  if (any(dim(x) != c(2,2))) {
    stop("expecting something 2 x 2")
  }
  names(x) <- NULL
  row.names(x) <- NULL
  colnames(x) <- NULL
  rowsums <- rowSums(x)
  p1 <- x[1, 1] / rowsums[1]
  p2 <- x[2, 1] / rowsums[2]
  o1 <- p1 / (1 - p1)
  o2 <- p2 / (1 - p2)
  RR <- p2 / p1
  OR <- o2 / o1
  crit <- qnorm((1 - conf.level)/2, lower.tail = FALSE)
  
  names(RR) <- "RR"
  log.RR <- log(RR)
  SE.log.RR <- sqrt( sum( x[,2]/x[,1]/rowsums) )
  log.lower.RR <- log.RR - crit * SE.log.RR
  log.upper.RR <- log.RR + crit * SE.log.RR
  lower.RR <- exp(log.lower.RR)
  upper.RR <- exp(log.upper.RR)
  
  names(OR) <- "OR"
  log.OR <- log(OR)
  SE.log.OR <- sqrt(sum(1/x))
  log.lower.OR <- log.OR - crit * SE.log.OR
  log.upper.OR <- log.OR + crit * SE.log.OR
  lower.OR <- exp(log.lower.OR)
  upper.OR <- exp(log.upper.OR)

  res <- if (relrisk) {
    structure(RR,
              p1 = p1, 
              p2 = p2, 
              o1 = o1, 
              o2 = o2, 
              OR = OR, 
              lower.OR = lower.OR, 
              upper.OR = upper.OR, 
              RR = RR,
              lower.RR = lower.RR, 
              upper.RR = upper.RR, 
              conf.level = conf.level,
              class=c("relrisk", "numeric"))
  } else {  
    structure(OR,
              p1 = p1, 
              p2 = p2, 
              o1 = o1, 
              o2 = o2, 
              OR = OR, 
              lower.OR = lower.OR, 
              upper.OR = upper.OR, 
              RR = RR,
              lower.RR = lower.RR, 
              upper.RR = upper.RR, 
              conf.level = conf.level,
              class=c("oddsRatio", "numeric"))
  }
  if (verbose) print(summary(res))
  res
}

#' @rdname oddsRatio
#' @export
oddsRatio <- function(x, conf.level = 0.95, verbose=!quiet, quiet=TRUE, digits=3) {
  orrr(x, conf.level=conf.level, verbose=verbose, digits=digits, relrisk=FALSE)
}

#' @rdname oddsRatio
#' @export
relrisk <- function(x, conf.level = 0.95, verbose=!quiet, quiet=TRUE, digits=3) {
  orrr(x, conf.level=conf.level, verbose=verbose, digits=digits, relrisk=TRUE)
}

#' @rdname oddsRatio
#' @export
print.oddsRatio <- function(x, digits  = 4, ...) {
  print(as.numeric(x))
}

#' @rdname oddsRatio
#' @export
print.relrisk <- function(x, digits  = 4, ...) {
  print(as.numeric(x))
}

#' @rdname oddsRatio
#' @export
setMethod("summary", "oddsRatio",
          function(object, digits = 4, ...){
            summary_relrisk_oddsratio(object, digits=digits, ...) 
          }
)

#' @rdname oddsRatio
#' @export
setMethod("summary","relrisk",
          function(object, digits = 4, ...){
            summary_relrisk_oddsratio(object, digits=digits, ...) 
          }
)

summary_relrisk_oddsratio <- function(x, digits = 4, ...){
  cat("\n")
  cat("Odds Ratio\n")
  cat("\n")
  cat("Proportions\n")
  cat("\t   Prop. 1:\t", format(attr(x,"p1"), digits = digits), "\n")
  cat("\t   Prop. 2:\t", format(attr(x,"p2"), digits = digits), "\n")
  cat("\t Rel. Risk:\t", format(attr(x,"RR"), digits = digits), "\n\n")
  cat("Odds\n")
  cat("\t    Odds 1:\t", format(attr(x,"o1"), digits = digits), "\n")
  cat("\t    Odds 2:\t", format(attr(x,"o2"), digits = digits), "\n")
  cat("\tOdds Ratio:\t", format(attr(x,"OR"), digits = digits), "\n\n")
  cat(format(100 * attr(x,"conf.level")), "percent confidence interval:\n")
  cat("\t", format(attr(x,"lower.RR"), digits = digits), "< RR <", 
      format(attr(x,"upper.RR"), digits = digits), "\n")
  cat("\t", format(attr(x,"lower.OR"), digits = digits), "< OR <", 
      format(attr(x,"upper.OR"), digits = digits), "\n")
}
