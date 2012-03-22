#' Tally test statistics
#' 
#' Tally test statistics from data and from multiple draws
#' from a simulated null distribution
#' @param sample sample data
#' 
#' @param rdata a matrix of randomly generated data under 
#'   null hypothesis.  
#' 
#' @param FUN a function that computes the test statistic from
#'   a data set.  The default value does nothing, making it easy to 
#'   use this to tabulate precomputed statistics into a null distribution.
#'   See the examples.
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
#' @param xlim limits for the horizontal axis of the plot.
#'
#' @param center center of null distribution
#'
#' @param alternative one of \code{default}, \code{two.sided}, \code{less}, or \code{greater}
#' 
#' @param sig.level  significance threshold for \code{wilcox.test} used to detect lack of symmetry
#' 
#' @param \dots additional arguments passed to \code{\link{xhistogram}}
#' 
#' @return A lattice plot showing the sampling distribution. 
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
#' statTally( x, rdata, fun=max )  # unusual test statistic
#' statTally( x, rdata, fun=var )  # equivalent to chi-squared test
#' # Can also be used with test stats that are precomputed.
#' D <- diff(mean( age ~ sex, HELPrct)); D
#' nullDist <- do(1000) * diff( mean( age ~ shuffle(sex), HELPrct))
#' statTally( D, nullDist)
#' 
#' @keywords inference 
#' @keywords teaching 
#' 
statTally <-
function (sample, rdata, FUN, direction = NULL, alternative=c('default','two.sided','less','greater'),
		  sig.level=0.1, center=NULL,
	stemplot = dim(rdata)[direction] < 201, q = c(0.5, 0.9, 0.95, 0.99), fun=function(x) x, xlim, ...) 
{

	alternative = match.arg(alternative) 
	#rdata <- matrix(rdata)

	if (missing(FUN)) {
		FUN = fun
	}
	if ( is.null(direction) ) {
		size <- max(NROW(sample), NCOL(sample))
		if ( NROW (rdata) == size ) {
			direction <- 2
		} else if ( NCOL(rdata) == size ) {
			direction <- 1
		} else {
			stop( paste( "sample and rdata have incompatible dimensions:", c(size, NROW(rdata), NCOL(rdata))) )
		}
	}

    dstat <- FUN(sample)
    stats <- apply(rdata, direction, FUN)
	if (alternative == 'default') {
		if (is.null(center)) center <- median(stats, na.rm=TRUE)
		pv <- pval(wilcox.test(stats, mu=center, exact=FALSE)) 
		if (is.na(pv) || pv > sig.level) {
           alternative <- "two.sided"
		   message(paste('Null distribution appears to be symmetric. (p = ', signif(pv,3),')'))
		} else {
           alternative <- if (dstat < center) 'less' else 'greater'
		   message(paste('Null distribution appears to be asymmetric. (p = ', signif(pv,3),')', sep=""))
		}
	}
	if (is.null(center)) center <- 0

    message(paste("\nTest statistic applied to sample data = ", signif(dstat, 4)))
    message("\nQuantiles of test statistic applied to random data:")
    print(quantile(stats, q))
    if (stemplot) {
        stem(stats)
    }
	results <- data.frame(stat=stats)
	if (missing(xlim)) xlim <- range(pretty(c(stats,dstat)))

	hi <- center + abs(dstat - center)
	lo <- center - abs(dstat - center)
	if (alternative == 'greater') lo <- -Inf
	if (alternative == 'less')    hi <-  Inf

    plot1 <- tryCatch( xhistogram(~stat, data=results,  #groups=stat >= dstat, 
						xlim = xlim, ...,
						panel = function(x,...){
							panel.xhistogram(x,...)
							grid.rect( x=unit(lo,'native'), y=0.5, hjust=1,
									  gp=gpar(fill='navy',col='navy', alpha=.05))
							grid.rect( x=unit(hi,'native'), y=0.5, hjust=0,
									  gp=gpar(fill='navy',col='navy', alpha=.05))
						}
						) , error = function(e) NULL
	)

    message("\nOf the random samples")
    message("\n\t", paste(sum(stats == dstat), "(", round(100 * 
        sum(stats == dstat)/length(stats), 2), "% )", "had test stats =", 
        signif(dstat, 4)))
	if (alternative != 'greater') {
    	message("\n\t", paste(sum(stats < lo), "(", round(100 * sum(stats < lo)/length(stats), 2), 
							  "% )", "had test stats <", 
   	     signif(lo, 4)))
	}
	if (alternative != 'less') {
    message("\n\t", paste(sum(stats > hi), "(", round(100 * sum(stats > hi)/length(stats), 2), 
						  "% )", "had test stats >", signif(hi, 4)))
	}
    message("\n")
	
    return(plot1)
}
