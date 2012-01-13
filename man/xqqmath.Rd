\name{xqqmath}
\alias{panel.xqqmath}
\alias{xqqmath}
\title{Augmented version of \code{qqmath}}
\usage{
  xqqmath(x, data = NULL, panel = "panel.xqqmath", ...)

  panel.xqqmath(x, qqmathline = !(fitline || idline),
    idline = FALSE, fitline = FALSE, slope = NULL,
    intercept = NULL, overlines = FALSE, groups = NULL,
    ..., col.line = trellis.par.get("add.line")$col,
    pch = 16, lwd = 2, lty = 2)
}
\arguments{
  \item{x,data,panel,xqqmath,\dots}{as in
  \code{\link[lattice]{qqmath}}}

  \item{qqmathline}{a logical: should line be displayed
  passing through first and third quartiles?}

  \item{idline}{a logical; should the line y=x be added to
  the plot?}

  \item{fitline}{a logical; should a ftted line be added to
  plot?  Such a line will use \code{slope} and
  \code{intercept} if provided, else the standard deviation
  and mean of the data.}

  \item{slope}{slope for added math line}

  \item{intercept}{intercept for added math line}

  \item{overlines}{a logical: should lines be on top of qq
  plot?}

  \item{groups,pch,lwd,lty}{as in lattice plots}

  \item{col.line}{color to use for added lines}
}
\value{
  a trellis object
}
\description{
  Augmented version of \code{qqmath}
}
\examples{
xqqmath(rnorm(100))
}

