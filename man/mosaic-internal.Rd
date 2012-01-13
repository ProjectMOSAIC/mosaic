\name{.is.formula}
\alias{.clean_names}
\alias{.cull_for_do}
\alias{.do.safe.call}
\alias{.flatten}
\alias{.is.formula}
\alias{.is.simple.formula}
\alias{.make.data.frame}
\alias{.merge_data_frames}
\alias{.mosaic_aggregate}
\alias{.simple.part}
\alias{.squash_names}
\title{Check if formula}
\usage{
  .is.formula(x)

  .is.simple.formula(x)

  .simple.part(x)

  .flatten(x)

  .mosaic_aggregate(x, data, FUN,
    overall = mosaic.par.get("aggregate.overall"), ...)

  .make.data.frame(x)

  .clean_names(x)

  .merge_data_frames(a, b)

  .squash_names(object, sep = ":")

  .cull_for_do(object)

  .do.safe.call(what, args, quote = FALSE,
    envir = parent.frame(), ...)
}
\arguments{
  \item{x}{an object}

  \item{x}{a formula}

  \item{x}{a formula}

  \item{x}{an R container object}

  \item{x}{object to be converted}

  \item{x}{a character vector}

  \item{a}{a data frame}

  \item{b}{a data frame}

  \item{object}{an object}

  \item{sep}{a character}

  \item{what}{either a function or a non-empty character
  string naming the function to be called.}

  \item{args}{a list of arguments to the function call. The
  names attribute of args gives the argument names.}

  \item{quote}{a logical value indicating whether to quote
  the arguments.}

  \item{envir}{an environment within which to evaluate the
  call.  This will be most useful if what is a character
  string and the arguments are symbols or quoted
  expressions.}
}
\value{
  TRUE for a formula, FALSE otherwise, even if evaluation
  throws an error

  TRUE if formula has no left-hand side or a simple
  right-hand side (e.g., \code{NULL}, ., 1, or 0)

  simple part of formula or NULL if formula is not simple

  a vector containing items in \code{x}

  a data frame

  a data frame

  a character vector

  a data frame

  a character vector

  an object reflecting some of the information contained in
  \code{object}

  The result of the (evaluated) function call.
}
\description{
  Check if formula

  Check for simple formula

  Extract simple part from formula

  Extract simple part from formula

  Wrapper to modify aggregating behavior of
  \code{\link[Hmisc]{summary.formula}}
}
\details{
  \code{.make.data.frame} converts things to a data frame

  \code{.clean_names} removes unwanted characters from
  character vector

  \code{.merge_data_frames} is a wrapper around merge

  \code{.squash_names} squashes names of a data frame into
  a single string

  \code{.cull_for_do} handles objects like models to do the
  right thing for \code{do}

  \code{.do.safe.call} avoids conflicts between named
  arguments and ... by taking named arguments
  preferentially.
}
\seealso{
  \code{\link{do.call}}
}
\keyword{internal}

