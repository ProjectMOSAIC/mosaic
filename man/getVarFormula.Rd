\name{getVarFormula}
\alias{getVarFormula}
\title{Extract data from a data frame using a formula interface}
\usage{
  getVarFormula(formula, data = parent.frame(),
    intercept = FALSE)
}
\arguments{
  \item{data}{a data frame}

  \item{formula}{a formula.  The right-hand side selects
  variables; the left-hand side, if present, is used to set
  row names.  A \code{.} on the right-hand side indicates
  to use all variables not in the LHS.}

  \item{intercept}{a logical indicating whether to include
  the intercept in the model default: FALSE (no intercept)}
}
\description{
  Uses the full model syntax.
}
\examples{
getVarFormula(~wt + mpg, data=mtcars)
}

