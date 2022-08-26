
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
#' @importFrom purrr possibly map_dbl
 
dpqrdist <- function( dist, type = c("d","p","q","r"), ...) {
  type <- match.arg(type)
  dots <- list(...)
  distFunName <- paste0(type, dist)
  distFun <- rlang::as_function(distFunName)
  dist_dots <- dots[names(dots) %in% names(formals(distFunName))]
  first_name <- as.name(names(formals(distFunName))[[1]])
  first_val <- dist_dots[[first_name]]
  dist_dots[[first_name]] <- NULL
  # for the sake of some distribution functions that throw errors, for example when p = 0 or 1
  # we convert errors into NaNs [Note: is.na(NaN) is TRUE]
  dpqrfun <- purrr::possibly(distFun, otherwise = NaN)
  # res <- do.call(purrr::map_dbl, c(list(.f = dpqrfun, .x = first_val), dist_dots))
  res <- do.call(dpqrfun,  c(list(first_val), dist_dots))
  if (length(res) > 0 & all(is.na(res))) {
    stop("No values could be computed. Did you specify all the required parameters?")
  }
  res
  # do.call(distFunName, dist_dots)
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
#' @param ... Additional arguments, typically for fine tuning the plot.
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
#'   used as a parameter for the the distribution.
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
      xlim_opts <- dpqrdist(dist, type = "q", p = c(0, 0.0001, 0.001, 0.01, 0.99, 0.999, 0.9999, 1), ...)
      xlim <- xlim_opts[4:5]
      
      # extend range if it doesn't extend too far
      mid_width <- diff(xlim)
      xlim[1] <- xlim_opts[min(which(xlim_opts[5] - xlim_opts < 1.8 * mid_width), na.rm = TRUE)]
      xlim[2] <- xlim_opts[max(which(xlim_opts - xlim_opts[4] < 1.8 * mid_width), na.rm = TRUE)]
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
      dplyr::tibble(p = diff(p_less)) %>%
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
        dplyr::tibble(
          x = xdata, density = ydata, 
          group = sapply(xdata, function(x) {sum(x > q, na.rm = TRUE)})
        ) %>%
        mutate(
          group = pmax(1, group),
          probability = Groups$label[group] 
        ) 
      
      # print(Ddensity)
      
      plot <- 
        do.call(
          gf_point, 
          c(list(density ~ x, color = ~ probability, group = ~ group, fill = ~ probability,
                 data = Ddensity), plot_dots)) %>%
        gf_segment(density + 0 ~ x + x) %>%
        gf_labs(x = "", y = "probability") %>%
        gf_refine(
        scale_fill_viridis_d(end = 0.9),
        scale_color_viridis_d(end = 0.9)
      )
    
  } else {
    Ddensity <- 
      dplyr::tibble(
        x = xdata, density = ydata, 
        group = sapply(xdata, function(x) {sum(x > q, na.rm = TRUE)})
      ) %>%
      mutate(
        group = pmax(1, group),
        probability = Groups$label[group] # factor(group, labels = labels[sort(unique(group))])
      ) %>%
      dplyr::filter(!is.na(probability))  # avoids issues when xlim is wider than support
    
    plot <- 
      do.call(gf_area, 
              c(list(density ~ x, fill = ~ probability, 
                     group = ~ group, data = Ddensity), plot_dots)) %>%
      gf_refine(
        scale_fill_viridis_d(end = 0.9),
        scale_color_viridis_d(end = 0.9)
      )
  }  
  return(plot)
}


#' @rdname pdist
#' @inheritParams stats::pgamma 
#' @seealso [qdist()], [xpnorm()], [xqnorm()].
#' @export
xpgamma <- function(q, shape, rate = 1, scale = 1/rate, 
                    lower.tail = TRUE, log.p = FALSE, ...) {
  if (!missing(rate) && !missing(scale)) {
    if (abs(rate * scale - 1) < 1e-15) 
      warning("specify 'rate' or 'scale' but not both")
    else stop("specify 'rate' or 'scale' but not both")
  }
  pdist("gamma", q = q, shape = shape, scale = scale,
        lower.tail = lower.tail, log.p = log.p, ...)
}
#' @rdname qdist
#' @inheritParams stats::qgamma
#' @export
xqgamma <- function(p, shape, rate = 1, scale = 1/rate, 
                    lower.tail = TRUE, log.p = FALSE, ...) {
  if (!missing(rate) && !missing(scale)) {
    if (abs(rate * scale - 1) < 1e-15) 
      warning("specify 'rate' or 'scale' but not both")
    else stop("specify 'rate' or 'scale' but not both")
  }
  qdist("gamma", p = p, shape = shape, scale = scale,
        lower.tail = lower.tail, log.p = log.p,
        ...)
}
#' @rdname cdist
#' @inheritParams stats::qgamma
#' @export
xcgamma <- function(p, shape, rate = 1, scale = 1/rate, 
                    lower.tail = TRUE, log.p = FALSE, ...) {
  if (!missing(rate) && !missing(scale)) {
    if (abs(rate * scale - 1) < 1e-15) 
      warning("specify 'rate' or 'scale' but not both")
    else stop("specify 'rate' or 'scale' but not both")
  }
  cdist("gamma", q = q, shape = shape, scale = scale,
        lower.tail = lower.tail, log.p = log.p,
        ...)
}

#' @rdname pdist
#' @inheritParams stats::pt
#' @export
xpt <- function(q, df , ncp, lower.tail = TRUE, log.p = FALSE, ...)  
  if (missing(ncp)) {
    pdist("t", q = q, df = df, lower.tail = lower.tail, 
          log.p = log.p, ...)
  } else {
    pdist("t", q = q, df = df, ncp = ncp, lower.tail = lower.tail, 
          log.p = log.p, ...)
  }
#' @rdname qdist
#' @inheritParams stats::qt
#' @export
xqt <- function(p, df , ncp, lower.tail = TRUE, log.p = FALSE, ...)  
  if (missing(ncp)) {
    qdist("t", p = p, df = df, lower.tail = lower.tail, 
          log.p = log.p, ...)
  } else {
    qdist("t", p = p, df = df, ncp = ncp, lower.tail = lower.tail, 
          log.p = log.p, ...)
  }



#' @rdname cdist
#' @inheritParams stats::pt
#' @export
xct <- function(p, df , ncp, lower.tail = TRUE, log.p = FALSE, ...)  
  if (missing(ncp)) {
    cdist("t", p = p, df = df, lower.tail = lower.tail, 
          log.p = log.p, ...)
  } else {
    cdist("t", p = p, df = df, ncp = ncp, lower.tail = lower.tail, 
          log.p = log.p, ...)
  }

#' @rdname pdist
#' @export
xpchisq <- function(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE, ...)   
  pdist("chisq", q = q, df = df, ncp = ncp, lower.tail = lower.tail, 
        log.p = log.p, ...)
#' @rdname qdist
#' @export
xqchisq <- function(p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE, ...)   
  qdist("chisq", p = p, df = df, ncp = ncp, lower.tail = lower.tail, 
        log.p = log.p, ...)
#' @rdname cdist
#' @export
xcchisq <- function(p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE, ...)   
  cdist("chisq", p = p, df = df, ncp = ncp, lower.tail = lower.tail, 
        log.p = log.p, ...)

#' @rdname pdist
#' @inheritParams stats::pf
#' @export
xpf <- function(q, df1, df2, lower.tail = TRUE, log.p = FALSE, ...)  
  pdist("f", q = q, df1 = df1, df2 =df2, 
        lower.tail = lower.tail, log.p = log.p, ...)

#' @rdname qdist
#' @inheritParams stats::qf
#' @export
xqf <- function(p, df1, df2, lower.tail = TRUE, log.p = FALSE, ...)  
  qdist("f", p = p, df1 = df1, df2 =df2, 
        lower.tail = lower.tail, log.p = log.p, ...)

#' @rdname cdist
#' @inheritParams stats::qf
#' @export
xcf <- function(p, df1, df2, lower.tail = TRUE, log.p = FALSE, ...)  
  cdist("f", p = p, df1 = df1, df2 =df2, 
        lower.tail = lower.tail, log.p = log.p, ...)


#' @rdname pdist
#' @inheritParams stats::pbinom
#' @export
xpbinom <- function(q, size, prob, lower.tail = TRUE, log.p = FALSE, ...)  
  pdist("binom", q = q, size = size, prob = prob, 
        lower.tail = lower.tail, log.p = log.p, ...)

#' @rdname qdist
#' @inheritParams stats::qbinom
#' @export
xqbinom <- function(p, size, prob, lower.tail = TRUE, log.p = FALSE, ...)  
  qdist("binom", p = p, size = size, prob = prob, 
        lower.tail = lower.tail, log.p = log.p, ...)

#' @rdname cdist
#' @inheritParams stats::qbinom
#' @export
xcbinom <- function(p, size, prob, lower.tail = TRUE, log.p = FALSE, ...)  
  cdist("binom", p = p, size = size, prob = prob, 
        lower.tail = lower.tail, log.p = log.p, ...)

#' @rdname pdist
#' @inheritParams stats::ppois
#' @export
xppois <- function(q, lambda, lower.tail = TRUE, log.p = FALSE, ...)  
  pdist("pois", q = q, lambda = lambda, 
        lower.tail = lower.tail, log.p = log.p, ...)

#' @rdname qdist
#' @inheritParams stats::qpois
#' @export
xqpois <- function(p, lambda, lower.tail = TRUE, log.p = FALSE, ...)  
  qdist("pois", p = p, lambda = lambda, 
        lower.tail = lower.tail, log.p = log.p, ...)


#' @rdname cdist
#' @inheritParams stats::qpois
#' @export
xcpois <- function(p, lambda, lower.tail = TRUE, log.p = FALSE, ...)  
  cdist("pois", p = p, lambda = lambda, 
        lower.tail = lower.tail, log.p = log.p, ...)


#' @rdname pdist
#' @inheritParams stats::pgeom
#' @export
xpgeom <- function(q, prob, lower.tail = TRUE, log.p = FALSE, ...)  
  pdist("geom", q = q, prob = prob, 
        lower.tail = lower.tail, log.p = log.p, ...)

#' @rdname qdist
#' @inheritParams stats::qgeom
#' @export
xqgeom <- function(p, prob, lower.tail = TRUE, log.p = FALSE, ...)  
  qdist("geom", p = p, prob = prob, 
        lower.tail = lower.tail, log.p = log.p, ...)


#' @rdname cdist
#' @inheritParams stats::qgeom
#' @export
xcgeom <- function(p, prob, lower.tail = TRUE, log.p = FALSE, ...)  
  cdist("geom", p = p, prob = prob, 
        lower.tail = lower.tail, log.p = log.p, ...)

#' @rdname pdist
#' @inheritParams stats::pnbinom
#' @export
xpnbinom <- function(q, size, prob, mu, lower.tail = TRUE, log.p = FALSE, ...)  
  pdist("nbinom", q = q, size = size, prob =prob, mu = mu, 
        lower.tail = lower.tail, log.p = log.p, ...)

#' @rdname qdist
#' @inheritParams stats::qnbinom
#' @export
xqnbinom <- function(p, size, prob, mu, lower.tail = TRUE, log.p = FALSE, ...)  
  qdist("nbinom", p = p, size = size, prob =prob, mu = mu, 
        lower.tail = lower.tail, log.p = log.p, ...)

#' @rdname cdist
#' @inheritParams stats::qnbinom
#' @export
xcnbinom <- function(p, size, prob, mu, lower.tail = TRUE, log.p = FALSE, ...)  
  cdist("nbinom", p = p, size = size, prob =prob, mu = mu, 
        lower.tail = lower.tail, log.p = log.p, ...)

#' @rdname pdist
#' @inheritParams stats::pbeta
#' @export
xpbeta <- function(q, shape1, shape2, ncp = 0, 
                   lower.tail = TRUE, log.p = FALSE, ...)  
  pdist("beta", q = q, shape1 = shape1, shape2 = shape2, ncp = 0, 
        lower.tail = lower.tail, log.p = log.p, ...)

#' @rdname qdist
#' @inheritParams stats::qbeta
#' @export
xqbeta <- function(p, shape1, shape2, ncp = 0, 
                   lower.tail = TRUE, log.p = FALSE, ...)  
  qdist("beta", p = p, shape1 = shape1, shape2 = shape2, ncp = 0, 
        lower.tail = lower.tail, log.p = log.p, ...)

#' @rdname cdist
#' @inheritParams stats::qbeta
#' @export
xcbeta <- function(p, shape1, shape2, ncp = 0, 
                   lower.tail = TRUE, log.p = FALSE, ...)  
  cdist("beta", p = p, shape1 = shape1, shape2 = shape2, ncp = 0, 
        lower.tail = lower.tail, log.p = log.p, ...)


is_discrete_dist <- function(dist, resolution = 100L, ... ) {
  q <- dpqrdist(dist, type = "q", p = ppoints(resolution), ...) 
  # continuous dists should have all unique values
  # discrete dists will typically have many ties
  # discrete dists with many values may look continuous
  length(unique(q)) / length(q) <= 0.95
} 
