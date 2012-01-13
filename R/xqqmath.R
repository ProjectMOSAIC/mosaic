#' Augmented version of \code{qqmath}
#'
#' 
#' @param x,data,panel,xqqmath,\dots  as in \code{\link[lattice]{qqmath}}
#'
#' @return a trellis object
#'
#' @export
#' @examples
#' xqqmath(rnorm(100))
#' 
xqqmath <-
function (x, data = NULL, panel = "panel.xqqmath", ...) 
{
    qqmath(x, data = data, panel = panel, ...)
}

#' @rdname xqqmath
#' @param qqmathline a logical: should line be displayed passing through first and third quartiles?
#' @param idline a logical; should the line y=x be added to the plot?
#' @param fitline a logical; should a ftted line be added to plot?  Such a line will use \code{slope}
#'        and \code{intercept} if provided, else the standard deviation and mean of the data.
#' @param slope slope for added math line
#' @param intercept intercept for added math line 
#' @param overlines a logical: should lines be on top of qq plot?
#' @param groups,pch,lwd,lty as in lattice plots
#' @param col.line color to use for added lines
#'
#' @export
panel.xqqmath <-
function (x, qqmathline = !(fitline || idline), idline = FALSE, 
    fitline = FALSE, slope = NULL, intercept = NULL, overlines = FALSE, 
    groups = NULL, ..., col.line = trellis.par.get("add.line")$col, 
    pch = 16, lwd = 2, lty = 2) 
{
    require(lattice)
    if (!is.null(groups)) {
        panel.superpose(x, groups = groups, panel.groups = "panel.xqqmath", 
            qqmathline = qqmathline, idline = idline, fitline = fitline, 
            intercept = intercept, slope = slope, overlines = overlines, 
            ..., col.line = col.line, pch = pch, lwd = lwd, lty = lty)
    }
    else {
        lty <- rep(lty, length = 3)
        if (overlines) {
            panel.qqmath(x, ...)
        }
        if (idline) {
            panel.abline(0, 1, col.line = col.line, lty = lty[3], 
                lwd = lwd)
        }
        if (fitline) {
            if (is.null(slope)) {
                slope = sd(x)
            }
            if (is.null(intercept)) {
                intercept = mean(x)
            }
            panel.abline(intercept, slope, col.line = col.line, 
                lty = lty[2], lwd = lwd)
        }
        if (qqmathline) {
            panel.qqmathline(x, col.line = col.line, lty = lty[1], 
                lwd = lwd, ...)
        }
        if (!overlines) {
            panel.qqmath(x, ...)
        }
    }
}
