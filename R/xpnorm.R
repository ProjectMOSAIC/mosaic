
utils::globalVariables( 
  c('button','picker','slider','checkbox','SD','Q','MEAN','slider','manipulate',
    '.plot_one_norm', '.plot_multi_norm', 'y1', 'y2', 'density', 'm'
  )
)

#' Augmented versions of pnorm and qnorm
#' 
#' These functions behave similarly to the functions with the initial `x`
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
# #' @param vlwd,vcol line width and color for vertical lines.
# #' @param rot angle of rotation for text labels.
#' @param manipulate logical.  If TRUE and in RStudio,
#'  	then sliders are added for interactivity.
#' @param return If `"plot"`, return a plot.  If `"values"`, return a vector of numerical values.
#' @param \dots additional arguments.
#' 
#' 
#' @seealso [histogram()], 
#' [chisq.test()], 
#' [pnorm()], 
#' [qnorm()], 
#' [qqmath()], and
#' [plot()]. 
#' 
#' 
#' @examples
#' xpnorm(650, 500, 100)
#' xqnorm(.75, 500, 100)
#' xpnorm(-3:3, return = "plot", system = "gg") %>% 
#'   gf_labs(title = "My Plot", x = "") %>% 
#'   gf_theme(theme_bw())
#' 
#' \dontrun{
#' if (rstudio_is_available() & require(manipulate)) {
#'   manipulate(xpnorm(score, 500, 100, verbose = verbose),
#'     score = slider(200, 800),
#' 	   verbose = checkbox(TRUE, label = "Verbose Output")
#'   )
#' }
#' }
#' @export

xpnorm <-
function (q, mean = 0, sd = 1, plot = TRUE, verbose = TRUE, invisible=FALSE, digits = 4, 
    lower.tail = TRUE, log.p = FALSE, xlim = mean + c(-4,4) * sd, ylim = c(0, 1.4 * dnorm(mean,mean,sd)), 
    # vlwd=2, vcol=trellis.par.get('add.line')$col, rot=45, 
    manipulate=FALSE, ..., return = c("value", "plot")) 
{
  return <- match.arg(return)
  
	if (manipulate && rstudio_is_available() && requireNamespace("manipulate")) {
		return(manipulate::manipulate( 
			xpnorm(q=Q, mean=MEAN, sd=SD, plot=TRUE, verbose=FALSE, invisible=invisible,
						   digits=digits, lower.tail=lower.tail, log.p=log.p, 
						   xlim=xlim, ylim=ylim, 
						   # vlwd=vlwd, vcol=vcol, rot=rot, 
						   manipulate=FALSE,
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
    if (length(q) == 1) {
		  res_plot <- .plot_one_norm(p=p, q=q, mean, sd, xlim=xlim, ylim=ylim, digits=digits, 
		      # vlwd=vlwd, vcol=vcol, rot=rot, 
		      ...)
    }
    if (length(q) > 1) {
      res_plot <- 
        .plot_multi_norm(p=p, q=q, mean, sd, xlim=xlim, ylim=ylim, digits=digits, 
                         # vlwd=vlwd, vcol=vcol, rot=rot, 
                         ...)
    }
    if (return == "plot") {
      if (invisible) {
        return(invisible(res_plot))
      } else {
        return(res_plot)
      }
    } else {
      if(plot) print(res_plot)
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
            # vlwd=2, vcol=trellis.par.get('add.line')$col, rot=45, 
            ..., return = c("value", "plot")) 
{
    return <- match.arg(return)
    q = qnorm(p, mean = mean, sd = sd, lower.tail = lower.tail, 
        log.p = log.p)
    z = (q - mean)/sd
    p <- pnorm(z)
    if (verbose) {
      message("\n")
      message(paste("If X ~ N(",format(mean, digits = digits),", ", 
                format(sd, digits = digits),"), then \n",sep=""))
      message(paste("\tP(X", " <= ", format(q, digits = digits), ") = ", 
                    format(p, digits = digits), "", sep = ""))
      message(paste("\tP(X", " >  ", format(q, digits = digits), ") = ", 
                    format(1 - p, digits = digits), "", sep = ""))
      message("\n")
    }
    if (! lower.tail) { # make sure we have lower tail probs from here on
      p <- 1 - p
    }
    
    if (length(p) == 1) {
		  res_plot <- .plot_one_norm(p=p, q=q, mean, sd, xlim=xlim, ylim=ylim, digits=digits, 
		      # vlwd=vlwd, vcol=vcol, rot=rot, 
		      ...)
    }
    if (length(p) > 1) {
		  res_plot <- .plot_multi_norm(p=sort(p), q=sort(q), mean, sd, xlim=xlim, ylim=ylim, digits=digits, 
		      # vlwd=vlwd, vcol=vcol, rot=rot, 
		      ...)
    }
    
    if (return == "plot") {
      if (invisible) {
        return(invisible(res_plot))
      } else {
        return(res_plot)
      }
    } else {
      if (plot) print(res_plot)
      if (invisible) { 
        return(invisible(q))
      }
      return(q)
    }
}

#' @rdname xpnorm
#' @export
#' 
xcnorm <-
  function (
    p, mean = 0, sd = 1, plot = TRUE, verbose = TRUE, digits = getOption("digits"), 
    lower.tail = TRUE, log.p = FALSE, xlim, ylim, invisible=FALSE, 
    ..., return = c("value", "plot")) {
    p <- c((1-p)/2, p + (1-p)/2)
    xqnorm(
      p, mean = mean, sd = sd, verbose = verbose, digits = digits, 
      lower.tail = lower.tail, log.p = log.p, xlim = xlim, ylim = ylim, 
      invisible = invisible, ..., return = return)
  }
    

#' Central Probability in a Normal or T Distribution
#' 
#' These versions of the quantile functions take a vector of 
#' *central* probabilities as its first argument.
#' 
#' @seealso [stats::qnorm()], [cdist()]
#' 
#' @inheritParams stats::qnorm
#' 
#' @param side One of "upper", "lower", or "both" indicating
#' whether a vector of upper or lower quantiles or a matrix of 
#' both should be returned.
#' 
#' @export
#' 
#' @examples
#' cnorm(.95)
#' qnorm(.975)
#' cnorm(.95, mean = 100, sd = 10)
#' xcnorm(.95)
#' 
cnorm <- 
  function(p, mean = 0, sd = 1, log.p = FALSE,
           side = c("both", "upper", "lower")
  ) {
    side <- match.arg(side)
    upper <- qnorm(p + (1-p)/2, mean = mean, sd = sd, log.p = log.p) 
    lower <- qnorm((1-p)/2,     mean = mean, sd = sd, log.p = log.p) 
    switch(
      side,
      upper = upper,
      lower = lower,
      both  = cbind(lower, upper)
    )
}

#' @rdname cnorm
#' @inheritParams stats::qt
#' 
#' @export
ct <- 
  function(p, df, ncp, log.p = FALSE,
           side = c("upper", "lower", "both")
  ) {
    side <- match.arg(side)
    upper <- qt(p + (1-p)/2, df = df, ncp = ncp, log.p = log.p) 
    lower <- qt((1-p)/2,     df = df, ncp = ncp, log.p = log.p) 
    switch(
      side,
      upper = upper,
      lower = lower,
      both  = cbind(lower, upper)
    )
}

#' midpoints along a sequence
#' 
#' Compute a vector of midpoints between values in a numeric vector

#' @param x a numeric vector
#' @return a vector of length 1 less than `x`
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
    # vlwd=2, vcol=trellis.par.get('add.line')$col, rot=0, 
    ...) 
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

	D <- 
	  data.frame(x = xdata, density = ydata) %>%
	  mutate(group = apply(sapply(x, function(x) {x >= q}), 2, sum)) %>%
	  head(-1) %>% tail(-1)
	
	Q <- 
	  data.frame(q = mid(q), m = 1.1 * max(D$density)) %>%
	  mutate(group = apply(sapply(q, function(x) {x >= q}), 2, sum))
	
	# plot <- 
	#   ggplot() +
	#   geom_area(aes(y = density, x = x, fill = factor(group)), data = D, show.legend = FALSE) +
	#   geom_text(aes(y = m,  x = q, color = factor(group)), data = Q, 
	#             label = paste("", round(diff(p), 3), sep = ""),
	#             angle = 45, show.legend = FALSE) +
	#   geom_point(aes(y = 1.1 * m, x = q), color = "transparent", data = Q)
	  
	plot <- 
	  gf_area(density ~ x, fill = ~as.character(group %% 2), data = D, 
	          group = ~group,
	          show.legend = FALSE, ...) %>%
	  gf_text(m ~ q, color = ~ as.character(group %% 2), data = Q, 
	            label = paste("", round(diff(p), 3), sep = ""),
	            angle = 45, show.legend = FALSE) %>%
	  gf_point((1.1 * m) ~ q, color = "transparent", data = Q, 
	           inherit = FALSE, show.legend = FALSE) %>%
	  gf_refine(
	    scale_fill_brewer(type = "qual", palette = 6),
	    scale_color_brewer(type = "qual", palette = 6)
	  )
	
	return(plot)
}


.plot_one_norm <- function(p, q, mean, sd, xlim, ylim, digits=4, 
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
	  dplyr::data_frame(
	    x = q,
	    z = (q - mean)/sd,
	    y1 = stats::dnorm(mean, mean, sd),
	    y2 = 1.1 * stats::dnorm(mean, mean, sd),
	    label = paste("z = ", round(z, 2))
	  )
	  

	# res_plot <-
	#   ggplot(data = Ddensity, aes(x = x, y = density, group = tail, fill = tail)) +
	#     geom_area() + 
	#     geom_text(aes(x = x, y = y2, label = label), data = Dtext, 
	#               vjust = 1, hjust = 0.5, inherit.aes = FALSE) +
	#     geom_segment(aes(x = x, xend = x, y = 0, yend = y1), data = Dtext, inherit.aes = FALSE) +
	#     guides(fill = guide_legend(title = "")) +
	#     theme(legend.position = "top")

	res_plot <-
	  gf_area(density ~ x, data = Ddensity, group = ~ tail, fill = ~ tail, 
	          show.legend = FALSE, ...) %>%
	  gf_text(y2 ~ x, label = ~label, data = Dtext, 
	          vjust = 1, hjust = 0.5, inherit = FALSE,
	          show.legend = FALSE) %>%
	  gf_segment(0 + y1 ~ x + x, data = Dtext, inherit = FALSE)  %>%
	  gf_refine(
	    scale_fill_brewer(type = "qual", palette = 2),
	    scale_color_brewer(type = "qual", palette = 2)
	  )
	  
	    # gf_refine(guides(fill = guide_legend(title = ""))) %>%
	
	return(res_plot)
	
}
