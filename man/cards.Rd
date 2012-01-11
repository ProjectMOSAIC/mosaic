\docType{data}
\name{cards}
\alias{card}
\alias{cards}
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
deal(cards, 13)        # bridge hand
deal(cards, 5)         # poker hand
shuffle(cards)         # shuffled deck
}
\keyword{datasets}

