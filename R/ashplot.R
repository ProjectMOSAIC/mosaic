
#' Average Shifted Histograms
#' 
#' An ASH plot is the average over all histograms of a fixed bin width. 
#' 
#' @param x a formula or numeric vector
#' @param width the historam bin width.
#' @param adjust a numeric adjustment to \code{width}.  Primarily useful when \code{width} is 
#'   not specified.  Increasing \code{adjust} makes the plot smoother.
#' @param panel a panel funtion
#' @param prepanel a prepanel function
#' @param ... additional arguments passed to panel and prepanel functions or \code{data}, a 
#'   data frame in which to find the variales used for the plot.
#' @param data a data frame
#' @export
#' @examples
#' ashplot( ~age | substance, groups = sex, data = HELPrct)
ashplot <- function(x, ..., width = NULL, adjust = NULL, 
                    panel = panel.ashplot, prepanel = prepanel.default.ashplot) 
{
  densityplot(x, panel = panel, width = width, ..., prepanel = prepanel)
}

#' @rdname ashplot
#' @param darg a list of arguments for the function computing the ASH.
#' @param groups as in other lattice plots
#' @param subscripts as in other lattice prepanel functions
#' 
#' @export
prepanel.default.ashplot <- function(
  x, darg, groups = NULL, subscripts = TRUE, 
  ...) 
{
  if (!is.numeric(x)) 
    x <- as.numeric(x)
  if (sum(!is.na(x)) < 1) 
    prepanel.null()
  else if (sum(!is.na(x)) == 1) {
    list(xlim = rep(x, 2), ylim = rep(0, 2), dx = 1, dy = 1)
  }
  else if (is.null(groups)) {
    h <- ash_points(x, binwidth = darg$width, adjust = darg$adjust)
    quants <- quantile(x, c(0.15, 0.85), names = FALSE, na.rm = TRUE)
    ok <- h$x > quants[1] & h$x < quants[2]
    list(xlim = range(h$x), ylim = range(h$y), dx = diff(h$x[ok]), 
         dy = diff(h$y[ok]))
  }
  else {
    vals <- sort(unique(groups))
    xl <- range(x, finite = TRUE)
    yl <- 0
    dxl <- numeric(0)
    dyl <- numeric(0)
    for (i in seq_along(vals)) {
      id <- (groups[subscripts] == vals[i])
      if (sum(id, na.rm = TRUE) > 1) {
        h <- do.call(ash_points, c(list(x = x[id], binwidth = darg$width, darg$adjust))) 
        xl <- c(xl, h$x)
        yl <- c(yl, h$y)
        quants <- quantile(x[id], c(0.15, 0.85), names = FALSE, na.rm = TRUE)
        ok <- h$x > quants[1] & h$x < quants[2]
        dxl <- c(dxl, diff(h$x[ok]))
        dyl <- c(dyl, diff(h$y[ok]))
      }
    }
    list(xlim = range(xl, finite = TRUE), 
         ylim = range(yl, finite = TRUE), dx = dxl, dy = dyl)
  }
}
 
#' @rdname ashplot
#' @param plot.points One of \code{TRUE}, \code{FALSE}, \code{"jitter"}, or \code{"rug"} 
#' @param ref a logical indicating whether a reference line should be displayed
#' @param jitter.amount when \code{plot.points="jitter"}, the value to use as the amount 
#' argument to \code{\link{jitter}}.
#' @param type type argument used to plot points, if requested. 
#'   This is not expected to be useful, it is available mostly to protect a \code{type} argument, 
#'   if specified, from affecting the display of the ASH.
#' @param identifier A character string that is prepended to the names of i
#'   grobs that are created by this panel function.
#' @export
panel.ashplot <- 
  function (
    x, 
    darg = list(),
    plot.points = FALSE,
    ref = FALSE, 
    groups = NULL, 
    jitter.amount = 0.01 * diff(current.panel.limits()$ylim), 
    type = "p", 
    ..., 
    identifier = "ash") 
{
  if (ref) {
    reference.line <- trellis.par.get("reference.line")
    panel.abline(h = 0, col = reference.line$col, lty = reference.line$lty, 
                 lwd = reference.line$lwd, identifier = paste(identifier, 
                                                              "abline"))
  }
  if (!is.null(groups)) {
    panel.superpose(x, darg = darg, plot.points = plot.points, 
                    ref = FALSE, groups = groups, 
                    panel.groups = panel.ashplot, 
                    jitter.amount = jitter.amount, 
                    type = type, ...)
  }
  else {
    switch(
      as.character(plot.points), 
        `TRUE` = panel.xyplot(x = x, y = rep(0, length(x)), type = type, ..., identifier = identifier), 
           rug = panel.rug(x = x, start = 0, end = 0, 
                         x.units = c("npc", "native"), 
                         type = type, ..., 
                         identifier = paste(identifier, "rug")), 
        jitter = panel.xyplot(x = x, y = jitter(rep(0, length(x)), amount = jitter.amount), 
                            type = type, ..., identifier = identifier)
      )

    ash <- ash_points(x, darg$width, adjust=darg$adjust)    
    if (sum(!is.na(x)) > 1) {
      panel.xyplot(ash$x, ash$y, type="l", ...)
    }
  }
  }

#' Compute knot points of an average shifted histogram
#' 
#' Mainly a utility for the \pkg{lattice} and \pkg{ggplot2} plotting 
#' functions, \code{ash_points()} returns the points to be plotted.
#' 
#' @param binwidth the width of the histogram bins.  If \code{NULL} (the default) the 
#'   binwidth will be chosen so that approximately 10 bins cover the data.  \code{adjust}
#'   can be used to to increase or decrease \code{binwidth}.
#' @return a data frame containing x and y coordinates of the resulting ASH plot.
#' @rdname ashplot
#' @export
ash_points <- function(x, binwidth = NULL, adjust = 1.0) {
  if (is.null(adjust)) adjust <- 1.0
  if (is.null(binwidth)) binwidth <- diff(range(x)) / (10.0 / adjust)
  left <- x - binwidth
  right <- x + binwidth
  knots <- sort(unique(c(left, x, right)))
  y <- sapply(knots, 
              function(k) sum( 1 / binwidth / length(x) * (1 - abs(x-k) / binwidth) * (abs(x-k) <= binwidth))
  )
  data.frame(x = knots, y = y)
}

#' @rdname ashplot
#' @export
StatAsh <- 
  ggproto("StatAsh", Stat,
          compute_group = function(data, scales, binwidth = NULL, adjust = NULL) {
            ash_points(data$x, binwidth = binwidth, adjust = adjust)
          },
          required_aes = c("x")
  )
 
#' @rdname ashplot
#' @export 
stat_ash <- 
  function(mapping = NULL, data = NULL, geom = "line",
           position = "identity", na.rm = FALSE, show.legend = NA, 
           inherit.aes = TRUE, binwidth = NULL, adjust = 1, ...) {
    ggplot2::layer(
      stat = StatAsh, data = data, mapping = mapping, geom = geom, 
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, binwidth = binwidth, adjust = adjust, ...)
    )
  }

# #' @export
# GeomAsh <- 
#   ggproto("GeomAsh", Geom,
#           required_aes = c("x"),
#           default_aes = aes(size = 1, colour = "black"),
#           draw_key = draw_key_path,
#           
#           draw_panel = function(data, panel_scales, coord) {
#             coords <- coord$transform(data, panel_scales)
#             grid::linesGrob(
#               coords$x, coords$y,
#               gp = grid::gpar(col = coords$colour,lwd = coords$size)
#             )
#           }
#   )

# GeomAsh <- 
#   ggproto("GeomAsh", GeomLine,
#           default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA)
# )

#' @rdname ashplot
#' @param mapping set of aesthetic mappings created by \code{\link{aes}()} 
#' or \code{\link{aes_}()}.
#' @param stat a statistical transformation to use on the data for this layer, as a string.
#' @param geom a geom to use for this layer, as a string.
#' @param position position adjustment, either as a string or the result of a call to
#' a position adjustment function.
#' @param na.rm	 If FALSE (the default), removes missing values with a warning. 
#' If TRUE silently removes missing values. 
#' @param show.legend	 logical. Should this layer be included in the legends? 
#' \code{NA}, the default, includes if any aesthetics are mapped. 
#' \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes	 If \code{FALSE}, overrides the default aesthetics, 
#' rather than combining with them. This is most useful for helper functions that 
#' define both data and aesthetics and shouldn't inherit behaviour from the default plot 
#' specification, e.g. \code{\link{borders}}.
#' @export
#' @examples
#' ggplot(faithful, aes(x = eruptions)) +
#'   geom_histogram(aes(y = ..density..), 
#'     fill = "lightskyblue", colour = "gray50", alpha = 0.2) +
#'   geom_ash(colour = "red") + 
#'   geom_ash(colour = "forestgreen", adjust = 2) + 
#'   geom_ash(colour = "navy", adjust = 1/2) + 
#'   theme_minimal()
geom_ash <- 
  function(mapping = NULL, data = NULL, stat = "ash",
           position = "identity", na.rm = FALSE, show.legend = NA, 
           inherit.aes = TRUE, binwidth = NULL, adjust = 1, ...) {
    ggplot2::layer(
      stat = stat, geom = ggplot2::GeomLine, data = data, mapping = mapping, 
      position = position, show.legend = show.legend, 
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, binwidth = binwidth, adjust = adjust, ...)
    )
  }

#' Geoms and stats for spline smoother
#' 
#' Functions to allow spline smoothing with ggplot2

#' @rdname geom_spline
#' @export
StatSpline <- 
  ggproto("StatSpline", Stat,
          compute_group = function(data, scales, weight = NULL, df = NULL, spar = NULL,
                                   cv = FALSE, all.knots = FALSE, nknots = stats::.nknots.smspl,
                                   df.offset  = 0, penalty = 1,  control.spar = list(),
                                   tol = NULL) {
            if (is.null(tol)) tol <- 1e-6 * IQR(data$x)
            SS <- if (is.null(df)) {
              smooth.spline(data$x, data$y, w = weight, spar = spar,
                            cv = cv, all.knots = all.knots, nknots = nknots,
                            df.offset = df.offset, penalty = penalty, control.spar = control.spar,
                            tol = tol) 
            } else {
              smooth.spline(data$x, data$y, w = weight, df = df, spar = spar,
                            cv = cv, all.knots = all.knots, nknots = nknots,
                            df.offset = df.offset, penalty = penalty, control.spar = control.spar,
                            tol = tol) 
            }
            data_frame(x = SS$x, y = SS$y)
          },
          required_aes = c("x", "y")
  )

#' @rdname geom_spline
#' @param mapping An aesthetic mapping produced with \code{\link{aes}()} or 
#' \code{\link{aes_string}()}.
#' @param data A data frame.
#' @param geom A geom.
#' @param stat A stat.
#' @param position A position object.
#' @param na.rm A logical indicating whether a warning should be issued when
#'   missing values are removed before plotting.
#' @param show.legend A logical indicating whether legends should be included
#'   for this layer.  If \code{NA}, legends will be inclued for each aesthetic
#'   that is mapped.
#' @param inherit.aes A logical indicating whether aesthetics should be 
#'   inherited.  When \code{FALSE}, the supplied \code{mapping} will be 
#'   the only aesthetics used.
#' @param weight An optional vector of weights.  
#'   See \code{\link{smooth.spline}()}.
#' @param df desired equivalent degrees of freedom.
#'   See \code{\link{smooth.spline}()} for details.
#' @param spar A smoothing parameter, typically in (0,1].
#'   See \code{\link{smooth.spline}()} for details.
#' @param cv A logical. 
#'   See \code{\link{smooth.spline}()} for details.
#' @param all.knots A logical.  
#'   See \code{\link{smooth.spline}()} for details.
#' @param nknots An integer or function giving the number of knots to use
#'   when \code{all.knots = FALSE}.  
#'   See \code{\link{smooth.spline}()} for details.
#' @param df.offset A numerical value used to increase the degrees of freedom
#'   when using GVC.
#'   See \code{\link{smooth.spline}()} for details.
#' @param penalty the coefficient of the penalty for degrees of freedom in the 
#'   GVC cireterion.  
#'   See \code{\link{smooth.spline}()} for details.
#' @param control.spar An optional list used to control root finding 
#'   when the parameter \code{spar} is computed.
#'   See \code{\link{smooth.spline}()} for details.
#' @param tol A tolerance for same-ness or uniqueness of the \code{x} values. 
#'   The values are binned into bins of size tol and values which fall into 
#'   the same bin are regarded as the same. Must be strictly positive (and finite).
#'   When \code{NULL}, \code{IQR(x) * 10e-6} is used.
#' @param ... Additional arguments
#' @export 
#' @examples
#' ggplot(Births) + geom_spline(aes(x = date, y=births, colour = wday))
#' ggplot(Births) + geom_spline(aes(x = date, y=births, colour = wday), nknots = 10)
#' 
stat_spline <- 
  function(mapping = NULL, data = NULL, geom = "line",
           position = "identity", na.rm = FALSE, show.legend = NA, 
           inherit.aes = TRUE, weight = NULL, df = NULL, spar = NULL,
           cv = FALSE, all.knots = FALSE, nknots = stats::.nknots.smspl,
           df.offset  = 0, penalty = 1,  control.spar = list(),
           tol = NULL, ...) {
    ggplot2::layer(
      stat = StatSpline, data = data, mapping = mapping, geom = geom, 
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, weight = weight, df = df, spar = spar,
                    cv = cv, all.knots = all.knots, nknots = nknots,
                    df.offset  = df.offset, penalty = penalty,  
                    control.spar = control.spar, tol = tol, ...)
    )
  }

#' @rdname geom_spline
#' @export
geom_spline <- 
  function(mapping = NULL, data = NULL, stat = "spline",
           position = "identity", na.rm = FALSE, show.legend = NA, 
           inherit.aes = TRUE, weight = NULL, df = NULL, spar = NULL,
           cv = FALSE, all.knots = FALSE, nknots = stats::.nknots.smspl,
           df.offset  = 0, penalty = 1,  control.spar = list(),
           tol = NULL, ...) {
    ggplot2::layer(
      stat = stat, geom = ggplot2::GeomLine, data = data, mapping = mapping, 
      position = position, show.legend = show.legend, 
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, weight = weight, df = df, spar = spar,
                    cv = cv, all.knots = all.knots, nknots = nknots,
                    df.offset  = df.offset, penalty = penalty,  
                    control.spar = control.spar, tol = tol, ...)
    )
  }
