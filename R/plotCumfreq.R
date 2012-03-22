#' Cumulative frequency plots
#' 
#' A high-level function for producing a cumulative frequency plot using
#' \code{lattice} graphics.
#' 
#' 
#' @rdname plotCumfreq
#' @param x a formula or numeric vector
#' @param data a data frame in which \code{x} is evaluated if \code{x} is a
#' formula.
#' @param \dots other lattice arguments
#' @seealso \code{\link{histogram}}, \code{\link{densityplot}}
#' @keywords graphics
#' @export
#' @examples
#' plotCumfreq(~eruptions, faithful, xlab = 'duration of eruptions')
#' 
plotCumfreq <- function(x, data, ...) { UseMethod('plotCumfreq') }

#' @rdname plotCumfreq
#' @export
#' @method plotCumfreq formula
#' @param subscripts as in lattice plots
plotCumfreq.formula <- function(x, data=NULL, subscripts,
	...) {
	require(lattice)
	densityplot( x, data=data,
		ylab='cumulative frequency',
		panel=panel.cumfreq,
		prepanel=prepanel.cumfreq,
		...)
}

#' @rdname plotCumfreq
#' @export
#' @method plotCumfreq default
plotCumfreq.default <- function(x, ...) {
	plotCumfreq.formula( ~ x, ...)
}

#' @rdname plotCumfreq
#' @aliases prepanel.cumfreq
#' @export
prepanel.cumfreq <- function(x, ...) {
	list( xlim=range(x), ylim=c(0,1), dx=1, dy=1 )
}

#' @rdname plotCumfreq
#' @aliases panel.cumfreq
#' @param type smooth or step-function?
#' @param groups grouping variable 
#' @export
panel.cumfreq <- function(x, type=c('smooth','step'), groups=NULL, ...) {
    if (!is.null(groups)) {
        panel.superpose(x, 
            ref = FALSE, groups = groups, 
            panel.groups = panel.cumfreq, 
            type = type, ...)
    }
    else {
		type=match.arg(type)
		if (type == 'step') {
			n <- length(x)
			xs <- rep(sort(x),each=2)
			p <- rep(ppoints(n-1),each=2)
			xs <- c(-Inf, xs, Inf)
			p <- c(0,0,p,1,1)
		} else {
			n <- length(x)
			xs <- sort(x)
			p <- ppoints(n)
			p <- aggregate(p, by=list(xs), mean)$x
			xs <- unique(xs)
		}
		panel.lines(x=xs, y=p, ...)
    }
}


