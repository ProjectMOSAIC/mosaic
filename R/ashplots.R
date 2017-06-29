
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