
# Note: one of d, p, q, or n must be a **named** argument.  This allows other things to 
# be in ... without causing issues for the d/p/q/r function called.
#' Distribution wrapper
#' 
#' Utility function wrapping up the d/p/q/r distribution functions
#' 
#' @param dist a character description of a distribution, for example 
#'   `"norm"`, `"t"`, or `"chisq"`
#' @param type one of `"x"`, `"p"`, `"q"`, or `"r"`
#' @param ... additional arguments passed on to underlying distribution function.
#'   Note that one of `d`, `p`, `q`, or `n` must 
#'   be a named argument in ...
#' @export
#' @examples
#' # 3 random draws from N(1,2)
#' dpqrdist("norm", "r", n = 3, mean = 1, sd = 2)
#' # These should all be the same
#' dpqrdist("norm", "d", x = 0) == dnorm(x = 0)
#' dpqrdist("norm", "p", q = 0, mean = 1, sd = 2) == pnorm(q = 0, mean = 1, sd = 2)
#' dpqrdist("norm", "q", p = 0.5, mean = 1, sd = 2) == qnorm(p = 0.5, mean = 1, sd = 2)
#' 
dpqrdist <- function( dist, type = c("d","p","q","r"), ... ) {
  type <- match.arg(type)
  dots <- list(...)
  distFunName <- paste0(type, dist)
  dist_dots <- dots[names(dots) %in% names(formals(distFunName))]

  do.call(distFunName, dist_dots)
}

#' Illustrated probability calculations from distributions
#' 
#' Illustrated probability calculations from distributions
#' 
#' @param dist a character description of a distribution, for example 
#'   `"norm"`, `"t"`, or `"chisq"`
#' @param q a vector of quantiles
#' @param plot a logical indicating whether a plot should be created
#' @param verbose a logical
#' @param invisible a logical
#' @param digits the number of digits desired
#' @param xlim x limits
#' @param ylim y limits
# #' @param vlwd width of vertical lines
# #' @param vcol color of vertical lines
# #' @param rot angle for rotating text indicating probability
#' @param resolution Number of points used for detecting discreteness and generating plots.  
#'        The default value of 5000 should work well except for discrete distributions
#'        that have many distinct values, especially if these values are not evenly spaced.
#' @param ... Additional arguments, including parameters of the distribution
#' and additional options for the plot
#' @param return If `"plot"`, return a plot.  If `"values"`, return a vector of numerical values.
#' @param refinements A list of refinements to the plot.  See [ggformula::gf_refine()].
#' @details The most general function is `pdist` which can work with 
#' any distribution for which a p-function exists.  As a convenience, wrappers are 
#' provided for several common distributions.
#' @return A vector of probabilities; a plot is printed as a side effect.
#' @examples
#' pdist("norm", -2:2)
#' pdist("norm", seq(80,120, by = 10), mean = 100, sd = 10)
#' pdist("chisq", 2:4, df = 3)
#' pdist("f", 1, df1 = 2, df2 = 10)
#' pdist("gamma", 2, shape = 3, rate = 4)
#' @export
 
pdist <- function (dist = "norm", q, plot = TRUE, verbose = FALSE, invisible = FALSE, 
                   digits = 3L, 
                   xlim, ylim,
                   resolution = 500L,
                   return = c("values", "plot"),
                   ...,
                   refinements = list())
{
 
  return <- match.arg(return)
  
  dots <- list(...)
  
  # if (is.null(vlwd)) {
  #   vlwd <- if (is_discrete_dist( dist, ... )) 0 else 2
  # }
  
  p <- dpqrdist(dist, type = "p", q = q, ...) 
  
  if (verbose) {
    cat("Verbose output not yet implemented.\n")
  }
 
  res_plot <-
    do.call(
      gf_refine,
      c(list(
        plot_multi_dist(
          dist = dist, p = p, 
          xlim = xlim, ylim = ylim, 
          digits = digits, 
          resolution = resolution,
          ...)),
        refinements)
    )
    
  if (return == "plot") {
    return(res_plot)
  }
  if (plot) {
    print(res_plot)
  }
  if (invisible) { 
    return(invisible(p))
  }
  return(p)
}

#' Illustrated quantile calculations from distributions
#' 
#' Illustrated quantile calculations from distributions
#' 
#' @param dist a character description of a distribution, for example 
#'   `"norm"`, `"t"`, or `"chisq"`
#' @param p a vector of probabilities
#' @param plot a logical indicating whether a plot should be created
#' @param verbose a logical
#' @param invisible a logical
#' @param digits the number of digits desired
#' @param xlim x limits.  By default, these are chosen to show the central 99.8\% 
#'   of the distribution.
#' @param ylim y limits
# #' @param vlwd width of vertical lines
# #' @param vcol color of vertical lines
# #' @param rot angle for rotating text indicating probability
#' @param resolution number of points used for detecting discreteness and generating plots.  
#'   The default value of 5000 should work well except for discrete distributions
#'   that have many distinct values, especially if these values are not evenly spaced.
#' @param refinements A list of refinements to the plot.  See [ggformula::gf_refine()].
#' @param ... additional arguments, including parameters of the distribution
#'   and additional options for the plot.  To help with name collisions (eg `size` for binomial 
#'   distributions and `shape` for gamma distributions), argument names beginning `plot_` will
#'   be renamed to remove `plot_` and passed only to the plot.  The unprefixed version will 
#'   used as a paramter for the the distribution.
#' @param return If `"plot"`, return a plot.  If `"values"`, return a vector of numerical values.
#' @details The most general function is `qdist` which can work with 
#' any distribution for which a q-function exists.  As a convenience, wrappers are 
#' provided for several common distributions.
#' 
#' @return a vector of quantiles; a plot is printed as a side effect
#' @examples
#' qdist("norm", seq(.1, .9, by = 0.10), 
#'       title = "Deciles of a normal distribution", show.legend = FALSE,
#'       pattern = "rings")
#' xqnorm(seq(.2, .8, by = 0.20), mean = 100, sd = 10)
#' qdist("unif", .5)
#' xqgamma(.5, shape = 3, scale = 4)
#' xqgamma(.5, shape = 3, scale = 4, color = "black")
#' xqbeta(.5, shape1 = .9, shape2 = 1.4, dlwd = 1)
#' xqchisq(c(.25,.5,.75), df = 3)
#' xcbinom(c(0.80, 0.90), size = 1000, prob = 0.40)
#' # displayed as if continuous
#' xcbinom(c(0.80, 0.90), size = 5000, prob = 0.40)
#' xpbinom(c(480, 500, 520), size = 1000, prob = 0.48)
#' xpbinom(c(40, 60), size = 100, prob = 0.5)
#' xqpois(c(0.25, 0.5, 0.75), lambda = 12)
#' xcpois(0.50, lambda = 12)
#' xcpois(0.50, lambda = 12, refinements = list(scale_color_brewer(type = "qual", palette = 5)))
#' @export 

qdist <- function (dist = "norm", p, plot = TRUE, verbose = FALSE, invisible = FALSE, 
                   resolution = 500L,
                   digits = 3L, 
                   xlim, ylim,
                   return = c("values", "plot"),
                   refinements = list(),
                   ...
                   )
{
  return = match.arg(return)
  dots <- list(...)
  
  # if (is.null(vlwd)) {
  #   vlwd <- if (is_discrete_dist( dist, ... )) 0 else 2
  # }
  # 
  q <- dpqrdist(dist, type = "q", p = p, ...) 
  # adjust probs for discrete dists where requested p's might not be (exactly) possible
  p <- dpqrdist(dist, type = "p", q = q, ...)  
  if (verbose) {
    cat("Verbose output not yet implemented.\n")
  }
 
  res_plot <-  
    do.call(
      ggformula::gf_refine,
      c(list(
        plot_multi_dist(
          dist = dist, p = p, 
          xlim = xlim, ylim = ylim, 
          digits = digits, 
          resolution = resolution,
          ...)
      ), refinements)
    )
      
  if (return == "plot") return (res_plot)

  if (plot) {
    print(res_plot)
  }
  if (invisible) { 
    return(invisible(q))
  }
  return(q)
}


utils::globalVariables(c("i", "name", "p_all"))

plot_multi_dist <- 
  function(dist, p, q, xlim, ylim, digits = 3L, resolution = 500L,
           lower.tail = TRUE,
           dlwd = 0, 
           pattern = c("stripes", "rings"),
           ...) 
{
  dots <- list(...)
  pattern <- match.arg(pattern)
  
  plot_dots <- dots[ ! names(dots) %in% names(formals(paste0("p",dist))) ]
  dist_dots <- dots[   names(dots) %in% names(formals(paste0("p",dist))) ]
  
  discrete <- do.call(is_discrete_dist, c(list(dist), dist_dots))
  
  if (missing(p)) {
    if (missing(q)) { stop( "one of p or q must be specified") }
    q <- sort(q)
    p <- dpqrdist(dist, type = "p", q = q, lower.tail = lower.tail, ...)
    p_less <- dpqrdist(dist, type = "p", q = q, lower.tail = TRUE, ...)
  } else {
    if (!missing(q)) {
      warning("Both p and q were specified.  I'm using p")
    }
    p <- sort(p)
    q <- sort(dpqrdist(dist, type = "q", p = p, lower.tail = lower.tail, ...))
    p_less <- dpqrdist(dist, type = "p", q = q, ...)
  }
 
  names(plot_dots) <- gsub("plot_", "", names(plot_dots)) 
  if ('lty' %in% names(plot_dots)) { plot_dots$linetype <- plot_dots$lty; plot_dots$lty <- NULL }
  if ('pch' %in% names(plot_dots)) { plot_dots$shape <- plot_dots$pch;    plot_dots$pch <- NULL }
  if ('cex' %in% names(plot_dots)) { plot_dots$size <- plot_dots$cex;     plot_dots$cex <- NULL }
  
  if (! 'xlab' %in% names(plot_dots)) { plot_dots$xlab <- "" }
  
  if (missing(xlim) || is.null(xlim)) {
    xlim_opts <- dpqrdist(dist, type = "q", p = c(0, 0.001, 0.999, 1), ...)
    dxlim_opts <- diff(xlim_opts)
    xlim <- xlim_opts[2:3]
    if (dxlim_opts[1] < dxlim_opts[2] && is.finite(dxlim_opts[1])) {xlim[1] <- xlim_opts[1]}
    if (dxlim_opts[3] < dxlim_opts[2] && is.finite(dxlim_opts[4])) {xlim[2] <- xlim_opts[4]}
  }
  
  if (discrete) {
    xdata <- unique(dpqrdist(dist, type = "q", p = ppoints(resolution), ...))
    step = min(diff(xdata))
    xlim = c(-step/2, step/2) + xlim  # widen by half step in each direction
    fill <- seq( min(xdata) -1.5 * step , max(xdata) + 1.5*step, length.out = resolution)
    xdata <- c(xdata, fill) 
    xdata <- dpqrdist(dist, type = "q", p = dpqrdist(dist, type = "p", q = xdata, ...), ...)
    xdata <- sort(unique(xdata))
  } else {
    xdata <- seq(xlim[1], xlim[2], length.out = resolution)
    xdata <- dpqrdist(dist, type = "q", p = dpqrdist(dist, type = "p", q = xdata, ...), ...)
    xdata <- sort(unique(xdata))
  }
 
  # not a fix: xdata <- sort(xdata) 
  ydata <- dpqrdist(dist, type = "d", x = xdata, ...)
  ymax <- 1.1 * quantile(ydata, 0.95, na.rm = TRUE)
  if (missing(ylim)) {
    ylim = c(0, 1.4 * ymax)  # quantile(ydata, 0.95, na.rm = TRUE))
    # print(ylim)
  }
  
  # this could be funny if limits don't span q
  p_less <- sort(unique(c(0, p_less, 1)))
  q <- dpqrdist(dist, type = "q", p = p_less, ...) #  c(xlim[1], q, xlim[2])
  
  # if (discrete) {
  #   if (is.null(plot_dots$pch)) plot_dots$pch = 16
  # }
  
 
  
  # args <- c(
  #   list(
  #     ydata ~ xdata, 
  #     xlim = xlim, ylim = ylim, 
  #     xlab = "", 
  #     ylab = if (discrete) "probability" else "density" 
  #   ),
  #   plot_dots
  # )
  # 
  # res_plot <- do.call("xyplot", args)
  
  Groups <- 
    data_frame(p = diff(p_less)) %>%
    mutate(
      i = 1 : n(),
      name = if (pattern == "stripes") LETTERS[i] else LETTERS[ceiling(1 + abs(i - mean(i)))]
    ) %>% 
    group_by(name) %>%
    mutate(
      p_all = sum(p),
      label = paste(first(name), ":", formatC(p_all, format = "f", digits = digits), sep = "")
    )
  
  if (discrete) { 
    # q <- tail(q, -1)
    Ddensity <- 
      data_frame(
        x = xdata, density = ydata, 
        group = sapply(xdata, function(x) {sum(x > q, na.rm = TRUE)})
      ) %>%
      mutate(
        group = pmax(1, group),
        probability = Groups$label[group] 
      ) 
    
    # print(Ddensity)
    
    plot <- 
      do.call(gf_point, c(list(density ~ x, color = ~ probability, group = ~ group, fill = ~ probability,
               data = Ddensity), plot_dots)) %>%
      gf_segment(density + 0 ~ x + x) %>%
      gf_labs(x = "", y = "probability")
    
  } else {
    Ddensity <- 
      data_frame(
        x = xdata, density = ydata, 
        group = sapply(xdata, function(x) {sum(x > q, na.rm = TRUE)}),
      ) %>%
      mutate(
        group = pmax(1, group),
        probability = Groups$label[group] # factor(group, labels = labels[sort(unique(group))])
      ) %>%
      dplyr::filter(!is.na(probability))  # avoids issues when xlim is wider than support
    
    plot <- 
      do.call(gf_area, c(list(density ~ x, fill = ~ probability, 
                              group = ~ group, data = Ddensity), plot_dots)) 
  }  
  return(plot)
}


#' @rdname pdist
#' @seealso [qdist()], [xpnorm()], [xqnorm()].
#' @export
xpgamma <- function(...)  pdist("gamma", ...)
#' @rdname qdist
#' @export
xqgamma <- function(...)  qdist("gamma", ...)
#' @rdname cdist
#' @export
xcgamma <- function(...)  cdist("gamma", ...)

#' @rdname pdist
#' @export
xpt <- function(...)  pdist("t", ...)
#' @rdname qdist
#' @export
xqt <- function(...)  qdist("t", ...)
#' @rdname cdist
#' @export
xct <- function(...)  cdist("t", ...)

#' @rdname pdist
#' @export
xpchisq <- function(...)  pdist("chisq", ...)
#' @rdname qdist
#' @export
xqchisq <- function(...)  qdist("chisq", ...)
#' @rdname cdist
#' @export
xcchisq <- function(...)  cdist("chisq", ...)

#' @rdname pdist
#' @export
xpf <- function(...)  pdist("f", ...)
#' @rdname qdist
#' @export
xqf <- function(...)  qdist("f", ...)
#' @rdname cdist
#' @export
xcf <- function(...)  cdist("f", ...)


#' @rdname pdist
#' @export
xpbinom <- function(...)  pdist("binom", ...)
#' @rdname qdist
#' @export
xqbinom <- function(...)  qdist("binom", ...)
#' @rdname cdist
#' @export
xcbinom <- function(...)  cdist("binom", ...)

#' @rdname pdist
#' @export
xppois <- function(...)  pdist("pois", ...)
#' @rdname qdist
#' @export
xqpois <- function(...)  qdist("pois", ...)
#' @rdname cdist
#' @export
xcpois <- function(...)  cdist("pois", ...)


#' @rdname pdist
#' @export
xpgeom <- function(...)  pdist("geom", ...)
#' @rdname qdist
#' @export
xqgeom <- function(...)  qdist("geom", ...)
#' @rdname cdist
#' @export
xcgeom <- function(...)  cdist("geom", ...)

#' @rdname pdist
#' @export
xpnbinom <- function(...)  pdist("nbinom", ...)
#' @rdname qdist
#' @export
xqnbinom <- function(...)  qdist("nbinom", ...)
#' @rdname cdist
#' @export
xcnbinom <- function(...)  qdist("cbinom", ...)

#' @rdname pdist
#' @export
xpbeta <- function(...)  pdist("beta", ...)
#' @rdname qdist
#' @export
xqbeta <- function(...)  qdist("beta", ...)
#' @rdname cdist
#' @export
xcbeta <- function(...)  cdist("beta", ...)


is_discrete_dist <- function(dist, n = 100L, ... ) {
  q <- dpqrdist(dist, type = "q", p = ppoints(n), ...) 
  # continuous dists should have all unique values
  # discrete dists will typically have many ties
  # discrete dists with many values may look continuous
  length(unique(q)) / length(q) <= 0.95
} 
