\name{dotPlot}
\alias{dotPlot}
\alias{panel.dotPlot}
\title{Dotplots}
\usage{
  dotPlot(x,
    nint = if (is.factor(x)) nlevels(x) else round(1.3 * log2(length(x)) + 4),
    breaks, ..., panel = panel.dotPlot)

  panel.dotPlot(x, breaks, equal.widths = TRUE,
    groups = NULL, nint = round(log2(length(x)) + 1),
    pch = if (is.null(groups)) trellis.par.get("dot.symbol")$pch else trellis.par.get("superpose.symbol")$pch,
    col = if (is.null(groups)) trellis.par.get("dot.symbol")$col else trellis.par.get("superpose.symbol")$col,
    lty = trellis.par.get("dot.line")$lty,
    lwd = trellis.par.get("dot.line")$lwd,
    col.line = trellis.par.get("dot.line")$col,
    alpha = trellis.par.get("dot.symbol")$alpha, cex = 1,
    type = "count", ...)
}
\arguments{
  \item{x}{a vector of values or a formula}

  \item{nint}{the number of intervals to use}

  \item{panel}{a panel function}

  \item{breaks,equal.widths,groups,pch,col,lty,lwd,col.line,type,alpha}{as
  in \code{\link{histogram}}}

  \item{\dots}{additional arguments}

  \item{cex}{a ratio by which to increase or decrease the
  dot size}
}
\value{
  a trellis object
}
\description{
  A high level function and panel function for producing a
  variant of a histogram called a dotplot.
}
\examples{
dotPlot( ~ height | voice.part, data = singer, nint = 17,
          endpoints = c(59.5, 76.5), layout = c(2,4), aspect = 1,
          xlab = "Height (inches)")
}
\author{
  Randall Pruim (\email{rpruim@calvin.edu})
}
\seealso{
  \code{\link{histogram}}
}
\keyword{graphics}

