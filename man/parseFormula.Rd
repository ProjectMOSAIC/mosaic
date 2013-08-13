\name{parse.formula}
\alias{condition}
\alias{condition.formula}
\alias{condition.parsedFormula}
\alias{lhs}
\alias{lhs.formula}
\alias{lhs.parsedFormula}
\alias{operator}
\alias{operator.formula}
\alias{operator.parsedFormula}
\alias{parse.formula}
\alias{rhs}
\alias{rhs.formula}
\alias{rhs.parsedFormula}
\title{Parse formulas}
\usage{
  parse.formula(formula, ...)

  rhs(x, ...)

  lhs(x, ...)

  condition(x, ...)

  operator(x, ...)

  \method{rhs}{formula} (x, ...)

  \method{lhs}{formula} (x, ...)

  \method{lhs}{formula} (x, ...)

  \method{lhs}{formula} (x, ...)

  \method{rhs}{parsedFormula} (x, ...)

  \method{lhs}{parsedFormula} (x, ...)

  \method{operator}{parsedFormula} (x, ...)

  \method{condition}{parsedFormula} (x, ...)
}
\arguments{
  \item{formula,}{a formula}

  \item{\dots}{additional arguments, current ignored}

  \item{x,}{an object (currently a \code{formula} or
  \code{parsedFormula})}
}
\value{
  an object of class \code{parsedFormula} from which
  information is easy to extract
}
\description{
  utilities for exptracting portions of formulas.
}
\details{
  currently this is primarily concerned with extracting the
  operator, left hand side, right hand side (minus any
  condition) and the condition. Improvements/extensions may
  come in the future.
}

