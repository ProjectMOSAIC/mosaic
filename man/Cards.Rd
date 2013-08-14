\docType{data}
\name{Cards}
\alias{Cards}
\alias{card}
\title{Standard Deck of Cards}
\description{
  A character vector with two or three character
  representations of each card in a standard 52-card deck.
}
\details{
  The 2 of clubs is represented as "2C", while the 10 of
  diamonds is "10D".
}
\examples{
deal(Cards, 13)        # bridge hand
deal(Cards, 5)         # poker hand
shuffle(Cards)         # shuffled deck
}
\keyword{datasets}

