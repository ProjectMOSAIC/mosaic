#' Create vector based on roughly equally sized groups
#'
#' @param x a numeric vector
#' @param n number of quantiles
#' @param format a specification of desired output format.
#' @return a vector.  The type of vector will depend on \code{format}.
#' @export
#' @examples
#' tally( ~ ntiles(1:50, 4) )
#' tally( ~ ntiles(1:50, 4, format="center") )
#' tally( ~ ntiles(1:50, 4, format="interval") )
#' tally( ~ ntiles(1:50, 4, format="left") )
#' tally( ~ ntiles(1:50, 4, format="right") )
#' 
ntiles <-  function(x, n=3, format=c("rank", "interval", "center", "left", "right")){
  format <- match.arg(format)
  # Figure out names
  qnames <- c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th")
  if (n > 10) qnames <- c(qnames, paste(11:n,"th",sep=""))
  brks <- co.intervals( x, n, overlap=0 )
  left <- brks[,1]
  right <- brks[,2]
  center <- ( left + right ) / 2
  edge <- c( -Inf, ( tail(left,-1) + head(right,-1) ) / 2, Inf)
  left <- head(edge, -1)
  right <- tail(edge, -1)
  left[1] <- min(x, na.rm=TRUE)
  right[length(right)] <- max(x, na.rm=TRUE)
  brks <- edge
  res <- switch(format, 
                "rank" =  cut(x, breaks=brks, labels=qnames[1:n], ordered_result=TRUE),
                "interval" =  cut(x, breaks=brks, ordered_result=TRUE),
                "center" = center[ as.numeric( cut(x, breaks=brks) ) ],
                "left" = left[ as.numeric( cut(x, breaks=brks) ) ],
                "right" = right[ as.numeric( cut(x, breaks=brks) ) ]
  )
  return(res)
}