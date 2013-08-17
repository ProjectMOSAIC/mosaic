\name{mPlot}
\alias{mMap}
\alias{mPlot}
\alias{mScatter}
\alias{mUniplot}
\title{Interactive plotting}
\usage{
  mPlot(data, default = plotType,
    system = c("lattice", "ggplot2"), show = FALSE,
    title = "", ...)

  mMap(data, default = "map", system = "ggplot2",
    show = FALSE, title = title, ...)

  mScatter(data,
    default = c("scatter", "boxplot", "violin"),
    system = c("lattice", "ggplot2"), show = FALSE,
    title = "")

  mUniplot(data,
    default = c("histogram", "density", "frequency polygon"),
    system = c("lattice", "ggplot2"), show = FALSE,
    title = "")
}
\arguments{
  \item{data}{a data frame containing the variables that
  might be used in the plot. Note that for maps, the data
  frame must contain coordinates of the polygons comprising
  the map and a variable for determining which corodiantes
  are part of the same region.  See \code{\link{sp2df}} for
  one way to create such a data frame.  Typically
  \code{\link{merge}} will be used to combine the map data
  with some auxilliary data to be displayed as fill color
  on the map, although this is not necessary if all one
  wants is a map.}

  \item{default}{default type of plot to create; one of
  \code{"scatter"}, \code{"boxplot"}, \code{"violin"},
  \code{"histogram"}, \code{"density"}, \code{"frequency
  polygon"}, \code{"xyplot"}, or \code{"map"}.  Unique
  prefixes suffice.}

  \item{system}{which graphics system to use (initially)
  for plotting (\pkg{ggplot2} or \pkg{lattice}).  A check
  box will allow on the fly change of plotting system.}

  \item{show}{a logical, if \code{TRUE}, the code will be
  displayed each time the plot is changed.}

  \item{title}{a title for the plot}

  \item{\dots}{additional arguments}
}
\value{
  Nothing.  Just for side effects.
}
\description{
  These functions provide a menu selection system (via
  \pkg{manipulate}) so that different aspects of a plot can
  be selected interactively. The \pkg{ggplot2} or
  \pkg{lattice} command for generating the plot currently
  being displayed can be copied to the console, whence it
  can be copied to a document for later direct,
  non-interactive use.
}
\details{
  Only \code{mPlot} is required by end users.  The other
  plotting functions are dispatched based on the value of
  \code{default}.

  Currently maps are only supported in \pkg{ggplot2} and
  not in \pkg{lattice}.
}
\examples{
\dontrun{
mPlot(HELPrct, "scatter")
mPlot(HELPrct, "density")
}
}

