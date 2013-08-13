\docType{data}
\name{Utilities2}
\alias{Utilities2}
\title{Utility bills}
\format{A data frame containing 117 observations for the following variables.
  \itemize{
    \item{\code{month}} {month (coded as a number)}
    \item{\code{day}} {day of month on which bill was calculated}
    \item{\code{year}} {year of bill}
    \item{\code{temp}} {average temperature (F) for billing period}
    \item{\code{kwh}} {electricity usage (kwh)}
    \item{\code{ccf}} {gas usage (ccf)}
    \item{\code{thermsPerDay}} {a numeric vector}
    \item{\code{billingDays}} {number of billing days in billing period}
    \item{\code{totalbill}} {total bill (in dollars)}
    \item{\code{gasbill}} {gas bill (in dollars)}
    \item{\code{elecbill}} {exectric bill (in dollars)}
    \item{\code{notes}} {notes about the billing period}
    \item{\code{ccfpday}} {average gas usage per day [\code{Utilities2} only]}
    \item{\code{kwhpday}} {average electric usage per day [\code{Utilities2} only]}
    \item{\code{gasbillpday}} {gas bill divided by billing days [\code{Utilities2} only]}
    \item{\code{elecbillpday}} {electric bill divided by billing days a numeric vector [\code{Utilities2} only]}
    \item{\code{totalbillpday}} {total bill divided by billing days a numeric vector [\code{Utilities2} only]}
    \item{\code{therms}} {\code{thermsPerDay * billingDays} [\code{Utilities2} only]}
    \item{\code{monthsSinceY2K}} {months since 2000 [\code{Utilities2} only]}
  }}
\source{
  Daniel T. Kaplan, \emph{Statistical modeling: A fresh
  approach}, 2009.
}
\description{
  Data from utility bills at a private residence.  This is
  an augmented version of \code{\link{Utilities}}.
}
\examples{
data(Utilities2)
xyplot(gasbillpday ~ temp, Utilities2)
}
\seealso{
  \code{\link{Utilities}}.
}
\keyword{datasets}

