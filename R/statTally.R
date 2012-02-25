#' Tally test statistics
#' 
#' Tally test statistics from data and from multiple draws
#' from a simluated null distribution
#' @param sample sample data
#' 
#' @param rdata a matrix of randomly generated data under 
#'   null hypothesis.  
#' 
#' @param FUN a function that computes the test statistic from
#'   a data set.
#' 
#' @param direction 1 or 2 indicating whether samples in \code{rdata}
#'   are in rows (1) or columns (2).
#' 
#' @param stemplot 
#' indicates whether a stem plot should be displayed
#' @param q quantiles of sampling distribution to display
#' 
#' @param fun same as \code{FUN} so you don't have to remember if it
#'   should be capitalized
#' 
#' @param \dots additional arguments passed to \code{\link{xhistogram}}
#' 
#' @return A lattice plot is returned invisibly but can be \code{print}ed. 
#' 
#' As side effects, information
#' about the empirical sampling distribution and (optionally) a stem plot are
#' printed to the screen.
#'
#' @author Randall Pruim (\email{rpruim@@calvin.edu})
#' @export
#' @examples
#' # is my spinner fair?
#' x <- c(10, 18, 9, 15)   # counts in four cells
#' rdata <- rmultinom(1000, sum(x), prob=rep(.25, 4))
#' print(statTally( x, rdata, fun=max ))  # unusual test statistic
#' print(statTally( x, rdata, fun=var ))  # equivalent to chi-squared test
#' 
#' @keywords inference 
#' @keywords teaching 
#' 
statTally <-
function (sample, rdata, FUN, direction = NULL, 
	stemplot = dim(rdata)[direction] < 201, q = c(0.5, 0.9, 0.95, 0.99), fun, ...) 
{
	if (missing(FUN)) {
		FUN = fun
	}
	if ( is.null(direction) ) {
		if ( dim(rdata) [1] == length(sample) ) {
			direction <- 2
		} else if ( dim(rdata) [2] == length(sample) ) {
			direction <- 1
		} else {
			stop("sample and rdata have incompatible dimensions")
		}
	}
    dstat <- FUN(sample)
#    cat("Test Stat function: ")
#	  cat(deparse(substitute(FUN)))
#    cat("\n\n")
    stats <- apply(rdata, direction, FUN)
    message("\nTest Stat applied to sample data = ")
    message(signif(dstat, 4))
    message("\n\n")
    message("Test Stat applied to random data:\n\n")
    print(quantile(stats, q))
    if (stemplot) {
        stem(stats)
    }
	results <- data.frame(stat=stats)
    plot1 <- xhistogram(~stat, data=results,  groups=stat >= dstat, ...) 
    message("\nOf the random samples")
    message("\n\t", paste(sum(stats < dstat), "(", round(100 * 
        sum(stats < dstat)/length(stats), 2), "% )", "had test stats <", 
        signif(dstat, 4)))
    message("\n\t", paste(sum(stats == dstat), "(", round(100 * 
        sum(stats == dstat)/length(stats), 2), "% )", "had test stats =", 
        signif(dstat, 4)))
    message("\n\t", paste(sum(stats > dstat), "(", round(100 * 
        sum(stats > dstat)/length(stats), 2), "% )", "had test stats >", 
        signif(dstat, 4)))
    message("\n")
    return(invisible(plot1))
}
