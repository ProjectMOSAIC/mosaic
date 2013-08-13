\name{mPlot}
\alias{mPlot}
\alias{mPlots,}
\alias{mScatter}
\alias{mUniplot}
\title{Interactive plotting interfaces}
\usage{
  mPlot(data, default = plotType,
    system = c("lattice", "ggplot2"), show = FALSE)

  mUniplot(data,
    default = c("histogram", "density", "frequency polygon"),
    system = c("lattice", "ggplot2"), show = FALSE)
}
\arguments{
  \item{data}{a data frame containing the variables that
  might be used in the plot.}

  \item{default}{default type of plot to create.}

  \item{system}{which graphics system to use (initially)
  for plotting (\pkg{ggplot2} or \pkg{lattice}).  A check
  box will allow on the fly change of plotting system.}

  \item{show}{a logical, if \code{TRUE}, the code will be
  displayed each time the plot is changed.}
}
\value{
  Nothing.  Just for side effects.
}
\description{
  These functions provide a menu selection system (via
  \pkg{manipulate}) so that different aspects of a plot can
  be selected interactively.  The \code{ggplot2} command
  for generating the plot currently being displayed can be
  copied to the console, whence it can be copied to a
  document for later direct, non-interactive use.
}

