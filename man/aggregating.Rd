\name{mean}
\alias{cor}
\alias{cov}
\alias{favstats}
\alias{fivenum}
\alias{iqr}
\alias{IQR}
\alias{max}
\alias{mean}
\alias{median}
\alias{min}
\alias{prod}
\alias{range}
\alias{sd}
\alias{sum}
\alias{var}
\title{Aggregating functions}
\usage{
  mean(x, ..., data, ..fun.. = base::mean)

  median(x, ..., data, ..fun.. = stats::median)

  range(x, ..., data, ..fun.. = base::range)

  sd(x, ..., data, ..fun.. = stats::sd)

  max(x, ..., data, ..fun.. = base::max)

  min(x, ..., data, ..fun.. = base::min)

  sum(x, ..., data, ..fun.. = base::sum)

  IQR(x, ..., data, ..fun.. = stats::IQR)

  fivenum(x, ..., data, ..fun.. = stats::fivenum)

  iqr(x, ..., data, ..fun.. = stats::IQR)

  prod(x, ..., data, ..fun.. = base::prod)

  sum(x, ..., data, ..fun.. = base::sum)

  favstats(x, ..., data, ..fun.. = fav_stats)

  var(x, ..., data, ..fun.. = stats::var)

  cor(x, y = NULL, ..., data = parent.frame())

  cov(x, y = NULL, ..., data = parent.frame())
}
\arguments{
  \item{x}{an object, often a formula}

  \item{y}{an object, often a numeric vector}

  \item{..fun..}{the underlyin function used in the
  computation}

  \item{data}{a data frame in which to evaluate formulas
  (or bare names)}

  \item{\dots}{additional arguments}
}
\description{
  The \code{mosaic} package makes several summary statistic
  functions (like \code{mean} and \code{sd}) formula aware.
}
\examples{
mean( HELPrct$age )
mean( ~ age, data=HELPrct )
mean( age ~ sex + substance, data=HELPrct )
mean( ~ age | sex + substance, data=HELPrct )
mean( sqrt(age), data=HELPrct )
sum( ~ age, data=HELPrct )
sd( HELPrct$age )
sd( ~ age, data=HELPrct )
sd( age ~ sex + substance, data=HELPrct )
var( HELPrct$age )
var( ~ age, data=HELPrct )
var( age ~ sex + substance, data=HELPrct )
IQR( width ~ sex, data=KidsFeet )
iqr( width ~ sex, data=KidsFeet )
favstats( width ~ sex, data=KidsFeet )

cor( length ~ width, data=KidsFeet )
cov ( length ~ width, data=KidsFeet )
}

