\name{xchisq.test}
\alias{xchisq.test}
\title{Augmented Chi-squared test}
\usage{
  xchisq.test(...)
}
\arguments{
  \item{\dots}{Arguments passed directly to
  \code{\link{chisq.test}}.}
}
\description{
  This augmented version of \code{\link{chisq.test}}
  provides more verbose output.
}
\examples{
# Physicians' Health Study data
phs <- cbind(c(104,189),c(10933,10845))
rownames(phs) <- c("aspirin","placebo")
colnames(phs) <- c("heart attack","no heart attack")
phs
xchisq.test(phs)
}
\author{
  Randall Pruim (\email{rpruim@calvin.edu})
}
\seealso{
  \code{\link{chisq.test}}
}

