\name{mosaic.options}
\alias{mosaic.getOption}
\alias{mosaic.options}
\alias{mosaic.par.get}
\alias{mosaic.par.set}
\alias{mosaicGetOption}
\title{Setting options for mosaic package functions}
\usage{
  mosaic.options(...)

  mosaic.getOption(name)

  mosaic.par.set(name, value, ..., theme, warn = TRUE,
    strict = FALSE)

  mosaic.par.get(name = NULL)
}
\arguments{
  \item{name}{the name of the option being set}

  \item{value}{the value to which to set the option}

  \item{theme}{a list appropriate for a mosaic theme}

  \item{warn}{a logical.  UNUSED at present.}

  \item{strict}{a logical or numeric.}

  \item{\dots}{additional arguments that are turned into a
  list if a list cannot be inferred from \code{theme},
  \code{name}, and \code{value}.}
}
\description{
  A mechanism for setting options in the mosaic package.
}

