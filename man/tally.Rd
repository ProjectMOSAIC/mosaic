\name{tally}
\alias{tally}
\title{Tabulate categorical data}
\usage{
  tally(x, data = parent.frame(),
    format = c("default", "count", "proportion", "percent"),
    margins = TRUE, quiet = TRUE, subset, ...)
}
\arguments{
  \item{x}{an object}

  \item{data}{a data frame or environment in which
  evaluation occurs}

  \item{format}{a character string describing the desired
  format of the results.  One of \code{'default'},
  \code{'count'}, \code{'proportion'}, or \code{'percent'}.
  In case of \code{'default'}, counts are used unless there
  is a condition, in which case proportions are used
  instead.}

  \item{subset}{an expression evaluating to a logical
  vector used to select a subset of \code{data}}

  \item{quiet}{a logical indicating whether messages about
  order in which marginal distributions are calculated
  should be surpressed.  See \code{\link{addmargins}}.}

  \item{margins}{a logical indicating whether marginal
  distributions should be displayed.}

  \item{...}{additional arguments passed to
  \code{\link{table}}}
}
\description{
  Tabulate categorical data
}
\examples{
tally( ~ substance, data=HELPrct)
tally( ~ substance & sex , data=HELPrct)
tally( sex ~ substance, data=HELPrct)   # equivalent to tally( ~ sex | substance, ... )
tally( ~ substance | sex , data=HELPrct)
tally( ~ substance | sex , data=HELPrct, format='count')
tally( ~ substance & sex , data=HELPrct, format='percent')
tally( ~ link, data=HELPrct, useNA="always")
}

