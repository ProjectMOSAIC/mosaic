\docType{data}
\name{SnowGR}
\alias{SnowGR}
\title{Snowfall data for Grand Rapids, MI}
\format{A data frame with 119 observations of the following variables.
  \itemize{
      \item \code{SeasonStart} Year in which season started (July is start of season)
      \item \code{SeasonEnd} Year in which season ended (June is end of season)
      \item \code{Jul} Inches of snow in July
      \item \code{Aug} Inches of snow in August
      \item \code{Sep} Inches of snow in September
      \item \code{Oct} Inches of snow in October
      \item \code{Nov} Inches of snow in November
      \item \code{Dec} Inches of snow in December
      \item \code{Jan} Inches of snow in January
      \item \code{Feb} Inches of snow in February
      \item \code{Mar} Inches of snow in March
      \item \code{Apr} Inches of snow in April
      \item \code{May} Inches of snow in May
      \item \code{Jun} Inches of snow in June
      \item \code{Total} Inches of snow for entire season (July-June)
  }}
\source{
  These data were compiled by Laura Kapitula from data
  available at
  \url{http://www.crh.noaa.gov/grr/climate/data/grr/snowfall/}.
}
\description{
  Official snowfall data by month and season for Grand
  Rapids, MI, going back to 1893.
}
\examples{
data(SnowGR)
histogram(~Total, data=SnowGR)
favstats(SnowGR$Total)
xyplot(Total ~ SeasonStart, SnowGR, type=c('p','smooth'))
if (require(reshape2)) {
  Snow2 <- melt(SnowGR, id=1:2)
  names(Snow2)[3:4] <- c('Time','Snow')
  bwplot(Snow ~ Time, Snow2)
}
}
\keyword{datasets}

