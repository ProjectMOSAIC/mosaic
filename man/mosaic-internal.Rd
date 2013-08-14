\docType{data}
\name{.is.formula}
\alias{.clean_names}
\alias{.cull_for_do}
\alias{.do.safe.call}
\alias{.fetchEnvironment}
\alias{.is.formula}
\alias{.is.simple.formula}
\alias{.make.data.frame}
\alias{.makeNice}
\alias{.merge_data_frames}
\alias{.simple.part}
\alias{.squash_names}
\alias{dynamicsFunction}
\alias{fetchDynamics}
\alias{rkFunction}
\title{Check if formula}
\format{<environment: 0x7fc94de176d8>}
\usage{
  .is.formula(x)

  .is.simple.formula(x)

  .simple.part(x)

  .make.data.frame(x)

  .clean_names(x)

  .merge_data_frames(a, b)

  .squash_names(object, sep = ":")

  .cull_for_do(object)

  .fetchEnvironment

  fetchDynamics(x)

  dynamicsFunction(DE, additionalAssignments = list())

  rkFunction(DE, additionalArguments = list())

  .makeNice(form, params = all.vars(form))

  .do.safe.call(what, args, quote = FALSE,
    envir = parent.frame(), ...)
}
\arguments{
  \item{x}{an object}

  \item{x}{a formula}

  \item{x}{a formula}

  \item{x}{object to be converted}

  \item{x}{a character vector}

  \item{a}{a data frame}

  \item{b}{a data frame}

  \item{object}{an object}

  \item{sep}{a character}

  \item{x}{a list}

  \item{DE}{representation of DE, the result of
  fetchDynamics}

  \item{additionalAssignments,}{a list return a function}

  \item{DE}{representation of DE, the result of
  fetchDynamics}

  \item{additionalAssignments,}{a list return a function}

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

  a data frame

  a character vector

  a data frame

  a character vector

  an object reflecting some of the information contained in
  \code{object}

  a list with two slots: names and functions

  The result of the (evaluated) function call.
}
\description{
  Check if formula

  Check for simple formula

  Extract simple part from formula

  parameters are stored as extra arguments the order of the
  dynamical variables (and "t") is important and will be
  used later

  Create a functions with a vector argument of state, for
  use in rk()

  Simplifying expressions, e.g. pure numbers go to numbers
  Written by Aaron Mayerson, May 2013
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
\keyword{datasets}
\keyword{internal}
\keyword{internel}
\keyword{mosaic}

