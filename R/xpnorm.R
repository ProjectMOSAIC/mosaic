utils::globalVariables( 
  c('button','picker','slider','checkbox','SD','Q','MEAN','slider','manipulate',
    '.plot_one_norm', '.plot_multi_norm'
  )
)

#' Augmented versions of pnorm and qnorm
#' 
#' These functions behave similarly to the functions with the initial \code{x}
#' removed from their names but add more verbose output and graphics.
#'
#' @param p probability
#' @param q quantile
#' @param mean,sd parameters of normal distribution.
#' @param plot logical.  If TRUE, show an illustrative plot.
#' @param verbose logical.  If TRUE, display verbose output.
#' @param invisible logical.  If TRUE, return value invisibly.
#' @param digits number of digits to display in output.
#' @param lower.tail logical.  If FALSE, use upper tail probabilities.
#' @param log.p logical.  If TRUE, uses the log of probabilities.
#' @param xlim,ylim limits for plotting.
#' @param vlwd,vcol line width and color for vertical lines.
#' @param rot angle of rotation for text labels.
#' @param manipulate logical.  If TRUE and in RStudio,
#' 	then sliders are added for interactivity.
#' @param \dots additional arguments.
#' 
#' 
#' @seealso \code{\link{histogram}}, 
#' \code{\link{chisq.test}}, 
#' \code{\link{pnorm}}, 
#' \code{\link{qnorm}}, 
#' \code{\link{qqmath}}, and
#' \code{\link{plot}}. 
#' 
#' 
#' @examples
#' xpnorm(650, 500, 100)
#' xqnorm(.75, 500, 100)
#' \dontrun{
#' if (rstudio_is_available() & require(manipulate)) {
#'   manipulate(xpnorm(score, 500, 100, verbose=verbose),
#'     score = slider(200,800),
#' 	   verbose = checkbox(TRUE, label="Verbose Output")
#'   )
#' }
#' }
#' @export

xpnorm <-
function (q, mean = 0, sd = 1, plot = TRUE, verbose = TRUE, invisible=FALSE, digits = 4, 
    lower.tail = TRUE, log.p = FALSE, xlim = mean + c(-4,4) * sd, ylim = c(0, 1.4 * dnorm(mean,mean,sd)), 
    vlwd=2, vcol=trellis.par.get('add.line')$col,
	rot=45, manipulate=FALSE, ..., return = c("value", "plot")) 
{
  return <- match.arg(return)
  
	if (manipulate && rstudio_is_available() && requireNamespace("manipulate")) {
		return(manipulate::manipulate( 
			xpnorm(q=Q, mean=MEAN, sd=SD, plot=TRUE, verbose=FALSE, invisible=invisible,
						   digits=digits, lower.tail=lower.tail, log.p=log.p, xlim=xlim,
						   ylim=ylim, vlwd=vlwd, vcol=vcol, rot=rot, manipulate=FALSE,
						   ...),
				   Q = manipulate::slider(mean-4*sd, mean+4*sd, initial=q, step=8*sd/200, label='q'),
				   MEAN = manipulate::slider(mean-4*sd, mean+4*sd, initial=mean, step=8*sd/200, label='mean'),
				   SD = manipulate::slider(0, 4*sd, initial=sd, step=4*sd/100, label='st dev')
				   )
		)
	}
    p = pnorm(q, mean = mean, sd = sd) 
    z = (q - mean)/sd
    if (verbose) {
		message("\n")
		message(paste("If X ~ N(",format(mean, digits = digits),", ", 
		          format(sd, digits = digits),"), then \n",sep=""))
        message(paste("\tP(X <= ", format(q, digits = digits), ") = P(Z <= ", 
                  format(z, digits = digits), 
            ") = ", format(p, digits = digits), "", sep = ""))
        message(paste("\tP(X >  ", format(q, digits = digits), ") = P(Z >  ", 
                  format(z, digits = digits), 
                  ") = ", format(1 - p, digits = digits), "", sep = ""))
        message("\n")
    }
    if (plot & length(q) == 1) {
		  res_plot <- .plot_one_norm(p=p, q=q, mean, sd, xlim=xlim, ylim=ylim, digits=digits, 
		      vlwd=vlwd, vcol=vcol, rot=rot, ...)
    }
    if (plot & length(q) > 1) {
      res_plot <- 
        .plot_multi_norm(p=p, q=q, mean, sd, xlim=xlim, ylim=ylim, digits=digits, 
                         vlwd=vlwd, vcol=vcol, rot=rot, ...)
    }
    if (return == "plot") {
      if (invisible) {
        return(invisible(res_plot))
      } else {
        return(res_plot)
      }
    } else {
      if (invisible) { 
        return(invisible(pnorm(q, mean = mean, sd = sd, lower.tail = lower.tail, log.p = log.p)))
      }
      return(pnorm(q, mean = mean, sd = sd, lower.tail = lower.tail, log.p = log.p))
    }
}

#' @rdname xpnorm
#' @export

xqnorm <-
function (p, mean = 0, sd = 1, plot = TRUE, verbose = TRUE, digits = getOption("digits"), 
    lower.tail = TRUE, log.p = FALSE, xlim, ylim, invisible=FALSE, 
    vlwd=2, vcol=trellis.par.get('add.line')$col,
	rot=45, ...) 
{
    q = qnorm(p, mean = mean, sd = sd, lower.tail = lower.tail, 
        log.p = log.p)
    z = (q - mean)/sd
    p <- pnorm(z)
    if (verbose) {
      message("\n")
      message(paste("If X ~ N(",format(mean, digits = digits),", ", 
                format(sd, digits = digits),"), then \n",sep=""))
      message(paste("\tP(X", " <= ", format(q, digits = digits), ") = ", p, "", sep = ""))
      message(paste("\tP(X", " >  ", format(q, digits = digits), ") = ", 1 - p, "", sep = ""))
      message("\n")
    }
    if (! lower.tail) { # make sure we have lower tail probs from here on
      p <- 1 - p
    }
    
    if (plot & length(p) == 1) {
		print(.plot_one_norm(p=p, q=q, mean, sd, xlim=xlim, ylim=ylim, digits=digits, 
		      vlwd=vlwd, vcol=vcol, rot=rot, ...))
    }
    if (plot & length(p) > 1) {
		print(.plot_multi_norm(p=sort(p), q=sort(q), mean, sd, xlim=xlim, ylim=ylim, digits=digits, 
		      vlwd=vlwd, vcol=vcol, rot=rot, ...))
    }
    
    if (return == "plot") {
      if (invisible) {
        return(invisible(res_plot))
      } else {
        return(res_plot)
      }
    } else {
      if (invisible) { 
        return(invisible(q))
      }
      return(q)
    }
}

#' midpoints along a sequence
#' 
#' Compute a vector of midpoints between values in a numeric vector

#' @param x a numeric vector
#' @return a vector of length 1 less than \code{x}
#' @export
#' @examples
#' mid(1:5)
#' mid((1:5)^2)

mid <- function(x) { 
	if (!is.numeric(x) || length(x) < 2) {
		stop( "`x' must be a numeric vector of length at least 2")
	}
	x[-1] - .5 * diff(x)
}


.plot_multi_norm <- function(p, q, mean, sd, xlim, ylim, digits=4, dlwd=2, 
    vlwd=2, vcol=trellis.par.get('add.line')$col,
	rot=0, ...) 
{
	dots <- list(...)

	if (! 'lty' %in% names(dots)) { dots$lty <- 1 }

	z <- (q - mean) / sd
			z <- (q - mean) / sd 
	zmax = max(4, abs(z) * 1.6)
	if (missing(xlim)) {
		xlim = mean + c(-1, 1) * abs(zmax) * sd
	}
	ymax = dnorm(mean, mean = mean, sd = sd)
	if (missing(ylim)) {
		ylim = c(0, 1.4 * ymax)
	}
	xdata = seq(xlim[1], xlim[2], length.out=800)
	ydata = dnorm(xdata, mean=mean, sd=sd)
	groups = apply(sapply(xdata, function(x) {x < q}), 2, sum)

	p <- c(0, p, 1)
	q <- c(xlim[1], q, xlim[2])

	plot <- do.call("xyplot", c(list(
		ydata ~ xdata, 
		xlim = xlim, ylim = ylim, 
		groups = groups, type='h',
		xlab = "", ylab = "density", 
		panel = function(x, y, ...) {
			panel.xyplot(x,y,...)
			panel.segments(q, 0, q, grid::unit(ymax,'native') + grid::unit(.2,'lines'), 
			  col = vcol, lwd=vlwd)
			grid.text(x=mid(q), y=grid::unit(ymax,'native') + grid::unit(1.0,'lines'), 
                default.units='native', rot=rot, check.overlap=TRUE,
			  	paste("", round(diff(p), 3), sep = ""), 
				just = c('center','center'),  gp=gpar(cex = 1.0))
			panel.mathdensity(dmath = dnorm, args = list(mean = mean, 
			  sd = sd), lwd = dlwd, n = 100, col = "navy")
		} 
		), dots)
	)

	D <- 
	  data.frame(x = xdata, density = ydata) %>%
	  mutate(group = apply(sapply(x, function(x) {x < q}), 2, sum))
	Q <- 
	  data.frame(q = mid(q), m = 1.1 * max(D$density)) %>%
	  mutate(group = apply(sapply(q, function(x) {x < q}), 2, sum))
	
	plot <- 
	  ggplot() +
	  geom_area(aes(y = density, x = x, fill = factor(group)), data = D, show.legend = FALSE) +
	  geom_text(aes(y = m,  x = q, color = factor(group)), data = Q, 
	            label = paste("", round(diff(p), 3), sep = ""),
	            angle = 45, show.legend = FALSE) +
	  geom_point(aes(y = 1.1 * m, x = q), color = "transparent", data = Q)
	  
	return(plot)
}


.plot_one_norm <- function(p, q, mean, sd, xlim, ylim, digits=4, 
    vlwd=2, vcol=trellis.par.get('add.line')$col,
	...) 
{
	z <- (q - mean) / sd
	zmax = max(4, abs(z) * 1.6)
	if (missing(xlim)) {
		xlim = mean + c(-1, 1) * zmax * sd
	}
	
	Ddensity <-
	   data_frame(
	     x = seq(xlim[1], xlim[2], length.out = 500),
	     density = dnorm(x, mean, sd),
	     tail = ifelse(x <= q, 
	                   paste0("P(X <= ", format(q, digits = 2), ") = ", round(pnorm(z), 3)),
	                   paste0("P(X >  ", format(q, digits = 2), ") = ", round(1 - pnorm(z), 3))
	     )
	   )
	
	Dtext <- 
	  data_frame(
	    x = q,
	    z = (q - mean)/sd,
	    y1 = dnorm(mean, mean, sd),
	    y2 = 1.1 * dnorm(mean, mean, sd),
	    label = paste("z = ", round(z, 2))
	  )
	  

	res_plot <-
	  ggplot(data = Ddensity, aes(x = x, y = density, group = tail, fill = tail)) +
	    geom_area() + 
	    geom_text(aes(x = x, y = y2, label = label), data = Dtext, 
	              vjust = 1, hjust = 0.5, inherit.aes = FALSE) +
	    geom_segment(aes(x = x, xend = x, y = 0, yend = y1), data = Dtext, inherit.aes = FALSE) +
	    guides(fill = guide_legend(title = "")) +
	    theme(legend.position = "top")

	return(res_plot)
	
}
