\name{repeater-class}
\alias{repeater-class}
\title{Repeater objects}
\description{
  Repeater objects can be used with the \code{*} operator
  to repeat things multiple time using a different syntax
  and different output format from that used by, for
  example, \code{\link{replicate}}.
}
\section{Slots}{
  \describe{ \item{\code{n}:}{Object of class
  \code{"numeric"} indicating how many times to repeat
  something.} \item{\code{cull}:}{Object of class
  \code{"function"} that culls the ouput from each
  repetition.} \item{\code{mode}:}{Object of class
  \code{"character"} indicating the output mode (NULL or
  'data.frame' or 'list').} }
}
\seealso{
  \code{\link{do}}
}

