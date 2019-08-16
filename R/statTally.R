utils::globalVariables(c("xleft", "xright"))

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
#' @param direction 1 or 2 indicating whether samples in `rdata`
#'   are in rows (1) or columns (2).
#'  
#' @param system graphics system to use for the plot
#' 
#' @param shade a color to use for shading. 
#' 
#' @param alpha opacity of shading.
#' 
#' @param binwidth bin width for histogram.
#' 
#' @param fill fill color for histogram.
#' 
#' @param color border color for histogram.
#' 
#' @param stemplot 
#' indicates whether a stem plot should be displayed
#' @param q quantiles of sampling distribution to display
#' 
#' @param fun same as `FUN` so you don't have to remember if it
#'   should be capitalized
#'
#' @param xlim limits for the horizontal axis of the plot.
#'
#' @param center center of null distribution
#'
#' @param alternative one of `default`, `two.sided`, `less`, or `greater`
#' 
#' @param sig.level  significance threshold for `wilcox.test` used to detect lack of symmetry
#' 
#' @param quiet a logicial indicating whether the text output should be suppressed
#' 
#' @param \dots additional arguments passed to [lattice::histogram()] or [ggplot2::geom_histogram()]
#' 
#' @return A lattice or ggplot showing the sampling distribution. 
#' 
#' As side effects, information
#' about the empirical sampling distribution and (optionally) a stem plot are
#' printed to the screen.
#'
#' @examples
#' # is my spinner fair?
#' x <- c(10, 18, 9, 15)   # counts in four cells
#' rdata <- rmultinom(999, sum(x), prob = rep(.25, 4))
#' statTally(x, rdata, fun = max, binwidth = 1)  # unusual test statistic
#' statTally(x, rdata, fun = var, shade = "red", binwidth = 2)  # equivalent to chi-squared test
#' # Can also be used with test stats that are precomputed.
#' if (require(mosaicData)) {
#' D <- diffmean( age ~ sex, data = HELPrct); D
#' nullDist <- do(999) * diffmean( age ~ shuffle(sex), data = HELPrct)
#' statTally(D, nullDist)
#' statTally(D, nullDist, system = "lattice")
#' }
#' 
#' @keywords inference 
#' @export
 
statTally <-
function (sample, rdata, FUN, direction = NULL, 
          alternative=c('default', 'two.sided', 'less', 'greater'), 
          sig.level = 0.1, 
          system = c("gg", "lattice"),
          shade = "navy",
          alpha = 0.10,
          binwidth = NULL, bins = NULL, fill = "gray80", color = "black",
          center = NULL, stemplot = dim(rdata)[direction] < 201, 
          q = c(0.5, 0.9, 0.95, 0.99), fun = function(x) x, xlim, 
          quiet = FALSE,
          ...) 
{

  mymessage <- 
    if (quiet) {
      function(...) {}
    } else {
      function(...) {cat(..., "\n")}
    }
  
  system <- match.arg(system)
	alternative <- match.arg(alternative) 
	
	dots <- list(...)

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
			stop(paste("sample and rdata have incompatible dimensions:", 
			           c(size, NROW(rdata), NCOL(rdata))))
		}
	}

    dstat <- FUN(sample)
    stats <- apply(rdata, direction, FUN)
	if (alternative == 'default') {
		if (is.null(center)) center <- median(stats, na.rm=TRUE)
		pv <- pval(wilcox.test(stats, mu=center, exact=FALSE)) 
		if (is.na(pv) || pv > sig.level) {
           alternative <- "two.sided"
		   mymessage(paste('\nNull distribution appears to be symmetric. (p = ', signif(pv,3),')'))
		} else {
           alternative <- if (dstat < center) 'less' else 'greater'
		   mymessage(paste('\nNull distribution appears to be asymmetric. (p = ', signif(pv,3),')', sep=""))
		}
	}
	if (is.null(center)) center <- 0

    mymessage(paste("\nTest statistic applied to sample data = ", signif(dstat, 4)))
    mymessage("\nQuantiles of test statistic applied to random data:")
    capture.output(quantile(stats, q, na.rm = TRUE)) %>%
      paste(collapse = "\n") %>% 
      mymessage()
    if (any( ! is.finite(stats))) {
      mymessage("** Note:  ", table(is.finite(stats))["FALSE"], " non-finite or missing values excluded.")
      stats <- stats[is.finite(stats)]
    }
    if (stemplot) {
        stem(stats)
    }
	results <- data.frame(stat=stats)
	if (missing(xlim)) xlim <- range(pretty(c(stats,dstat)))

	if (alternative == 'two.sided') {
	  hi <- center + abs(dstat - center)
	  lo <- center - abs(dstat - center)
	}
	if (alternative == 'greater') {
	  hi <- dstat
	  lo <- -Inf
	}
	if (alternative == 'less') {
	  hi <-  Inf
	  lo <- dstat
	}

	Rect_Data <- data.frame(
	  xleft = c(-Inf, hi),
	  xright = c(lo, Inf)
	)

	plot1 <- 
	  switch(
	    system,
	    gg = 
	      tryCatch( 
	        ggplot() +
	          geom_histogram(
	            aes(y = stat(density), x = stat),
	            data = results,
	            fill = fill, color = color, binwidth = binwidth, bins = bins,
	            ...) +
	          geom_rect(
	            aes(ymin = 0, ymax = Inf, xmin = xleft, xmax = xright),
	            data = Rect_Data,
	            fill = shade, color = "transparent", alpha = alpha, inherit.aes = FALSE) +
	          lims(x = xlim),
	        #             
	        #                  
	        # gf_dhistogram( ~ stat, data = results, fill = fill, color = color, binwidth = binwidth) %>%
	        #   gf_rect(0 + Inf ~ xleft + xright, fill = shade, alpha = alpha, data = Rect_Data,
	        #           inherit = FALSE) %>%
	        #   gf_lims(x = xlim), 
	        error = function(e) NULL
	      ),
	    lattice = 
	      tryCatch( histogram(~stat, data=results,  #groups=stat >= dstat, 
	                          xlim = xlim, width = binwidth, col = fill, border = color, 
	                          ...,
	                          panel = function(x,...){
	                            panel.xhistogram(x, ...)
	                            grid.rect( x=grid::unit(lo, "native"), y = grid::unit(0, "native"),
	                                       hjust = 1,
	                                       height = unit(1, "npc"), vjust = 0,
	                                       gp=gpar(fill = shade, col = shade, alpha = alpha))
	                            grid.rect( x=grid::unit(hi, "native"), y = grid::unit(0, "native"),
	                                       hjust = 0,
	                                       height = unit(1, "npc"), vjust = 0,
	                                       gp=gpar(fill = shade, col = shade, alpha = alpha))
	                          }
	      ) , error = function(e) NULL
	      )
	  )
	
	# for backward compatibility with a code chunk in fast 2e
	if (system == "gg" && !is.null(dots$xlab)) {
	  plot1 <- plot1 %>% gf_labs(x = dots$xlab)
	}
	
	
	# add in observed statistic for the remaining summaries.
	stats <- c(dstat, stats)
	mymessage("\nOf the ", length(stats), " samples (1 original + ", length(stats) -1, " random),")
    mymessage("\t", paste(sum(stats == dstat), "(", round(100 * 
        sum(stats == dstat)/length(stats), 2), "% )", "had test stats =", 
        signif(dstat, 4)))
	if (alternative != 'greater') {
    	mymessage("\t", paste(sum(stats <= lo), "(", round(100 * sum(stats <= lo)/length(stats), 2), 
							  "% )", "had test stats <=", 
   	     signif(lo, 4)))
	}
	if (alternative != 'less') {
    mymessage("\t", paste(sum(stats >= hi), "(", round(100 * sum(stats >= hi)/length(stats), 2), 
						  "% )", "had test stats >=", signif(hi, 4)))
	}
	
    return(plot1)
}
