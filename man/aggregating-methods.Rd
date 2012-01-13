\docType{methods}
\name{aggregating-methods}
\alias{aggregating-methods}
\alias{count}
\alias{count,ANY-method}
\alias{count,data.frame-method}
\alias{count,factor-method}
\alias{count,formula-method}
\alias{count,logical-method}
\alias{max}
\alias{mean}
\alias{mean,ANY-method}
\alias{mean,data.frame-method}
\alias{mean,formula-method}
\alias{mean,numeric-method}
\alias{median}
\alias{median,ANY-method}
\alias{median,data.frame-method}
\alias{median,formula-method}
\alias{median,numeric-method}
\alias{min}
\alias{prop}
\alias{prop,ANY-method}
\alias{prop,data.frame-method}
\alias{prop,factor-method}
\alias{prop,formula-method}
\alias{prop,logical-method}
\alias{sd}
\alias{sd,ANY-method}
\alias{sd,data.frame-method}
\alias{sd,formula-method}
\alias{sd,numeric-method}
\alias{var}
\alias{var,ANY,ANY,ANY,ANY,ANY-method}
\alias{var,data.frame,ANY,ANY,ANY,ANY-method}
\alias{var,formula,data.frame,ANY,ANY,missing-method}
\alias{var,formula,missing,ANY,ANY,data.frame-method}
\alias{var,matrix,ANY,ANY,ANY,ANY-method}
\alias{var,numeric,ANY,ANY,ANY,ANY-method}
\alias{var,numeric,numeric,ANY,ANY,ANY-method}
\title{Aggregating summary statistics}
\usage{
  min(x, ..., na.rm = TRUE)

  max(x, ..., na.rm = TRUE)
}
\description{
  These drop-in replacements and new summary statistics
  functions are formula-aware and allow the use of simple
  names within data frames. When given formulas, they call
  \code{\link{aggregate}} using the formula.
}
\details{
  These methods are wrappers around functions and methods
  in the \code{base} and \code{stats} packages and provide
  additional interfaces.

  The default value for \code{na.rm} is reversed from the
  functions in \code{base} and \code{stats}. Also,
  \code{na.rm}, \code{use}, and \code{trim} follow
  \code{\dots} so must be named using their full names.
}
\examples{
mean(age, data=HELPrct)
mean(~age, data=HELPrct)
mean(age ~ ., data=HELPrct)
mean(age ~ 1, data=HELPrct)
mean(age ~ NULL, data=HELPrct)
mean(HELPrct$age)
mean(age ~ sex, data=HELPrct)
mean(age ~ sex + treat, data=HELPrct)
median(age, data=HELPrct)
median(~age, data=HELPrct)
median(age ~ ., data=HELPrct)
median(age ~ 1, data=HELPrct)
median(age ~ NULL, data=HELPrct)
median(HELPrct$age)
median(age ~ sex, data=HELPrct)
median(age ~ sex + treat, data=HELPrct)
sd(age, data=HELPrct)
sd(~age, data=HELPrct)
sd(age ~ ., data=HELPrct)
sd(age ~ 1, data=HELPrct)
sd(age ~ NULL, data=HELPrct)
sd(HELPrct$age)
sd(age ~ sex, data=HELPrct)
sd(age ~ sex + treat, data=HELPrct)
var(age, data=HELPrct)
var(~age, data=HELPrct)
var(age ~ ., data=HELPrct)
var(age ~ 1, data=HELPrct)
var(age ~ NULL, data=HELPrct)
var(HELPrct$age)
var(age ~ sex, data=HELPrct)
var(age ~ sex + treat, data=HELPrct)
min(age, data=HELPrct)
max(age, data=HELPrct)
max(~age, data=HELPrct)
max(age ~ ., data=HELPrct)
max(age ~ 1, data=HELPrct)
max(age ~ NULL, data=HELPrct)
max(HELPrct$age)
max(age ~ sex, data=HELPrct)
max(age ~ sex + treat, data=HELPrct)
count(sex, data=HELPrct)
count(sex, data=HELPrct, level='male')
count(HELPrct$sex)
prop(sex, data=HELPrct)
prop(sex, data=HELPrct, level='male')
prop(HELPrct$sex)
}
\author{
  Randall Pruim (\email{rpruim@calvin.edu})
}
\seealso{
  \code{\link[stats]{aggregate}}, \code{\link[stats]{sd}},
  \code{\link[stats]{var}}, \code{\link[stats]{median}},
  \code{\link[base]{mean}}, \code{\link[base]{max}},
  \code{\link[base]{min}}, \code{\link[base]{sum}}
}
\keyword{methods}
\keyword{stats}

