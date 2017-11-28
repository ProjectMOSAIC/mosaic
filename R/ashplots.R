
#' @import mosaicCore
NA

#' Average Shifted Histograms
#' 
#' An ASH plot is the average over all histograms of a fixed bin width. 
#' 
#' @param x A formula or numeric vector.
#' @param width The histogram bin width.
#' @param adjust A numeric adjustment to `width`.  Primarily useful when `width` is 
#'   not specified.  Increasing `adjust` makes the plot smoother.
#' @param panel A panel function.
#' @param prepanel A prepanel function.
#' @param ... Additional arguments passed to panel and prepanel functions or `data`, a 
#'   data frame in which to find the variables used for the plot.
#' @param data A data frame.
#' @export
#' @examples
#' ashplot( ~age | substance, groups = sex, data = HELPrct)
ashplot <- function(x, data = data, ..., width = NULL, adjust = NULL, 
                    panel = panel.ashplot, prepanel = prepanel.default.ashplot) 
{
  densityplot(x, data = data, panel = panel, width = width, ..., prepanel = prepanel)
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
    h <- mosaicCore::ash_points(x, binwidth = darg$width, adjust = darg$adjust)
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
        h <- do.call(mosaicCore::ash_points, c(list(x = x[id], binwidth = darg$width, darg$adjust))) 
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
#' @param plot.points One of `TRUE`, `FALSE`, `"jitter"`, or `"rug"` 
#' @param ref a logical indicating whether a reference line should be displayed
#' @param jitter.amount when `plot.points="jitter"`, the value to use as the amount 
#' argument to [jitter()].
#' @param type type argument used to plot points, if requested. 
#'   This is not expected to be useful, it is available mostly to protect a `type` argument, 
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
      
      ash <- mosaicCore::ash_points(x, darg$width, adjust=darg$adjust)    
      if (sum(!is.na(x)) > 1) {
        panel.xyplot(ash$x, ash$y, type="l", ...)
      }
    }
  }


