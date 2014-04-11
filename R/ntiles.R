#' Create vector based on roughly equally sized groups
#'
#' @param x a numeric vector
#' @param n (approximate) number of quantiles
#' @param format a specification of desired output format.
#' @param digits desired number of digits for labeling of factors.
#' @return a vector.  The type of vector will depend on \code{format}.
#' @export
#' @examples
#' tally( ~ ntiles(1:50, 4) )
#' tally( ~ ntiles(1:50, 4, format="center") )
#' tally( ~ ntiles(1:50, 4, format="interval") )
#' tally( ~ ntiles(1:50, 4, format="left") )
#' tally( ~ ntiles(1:50, 4, format="right") )
#' 
ntiles <-  function(x, n=3, format=c("rank", "interval", "center", "left", "right"), digits=3){
  format <- match.arg(format)
  
  # Figure out names
  qnames <- c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th")
  if (n > 10) qnames <- c(qnames, paste(11:n,"th",sep=""))
  
  xrank <- rank(x, na.last=TRUE)
  xrank[is.na(x)] <- NA
  size <- max(xrank, na.rm=TRUE)
  cts <- round( seq(1, size, length.out = (n+1) ) )
  bin <- as.numeric(cut( xrank, breaks = cts, include.lowest=TRUE ))
  
  left <- min( x ~ bin, na.rm=TRUE )
  right <- max( x ~ bin, na.rm=TRUE )
  center <- signif( ( left + right ) / 2, digits )
  
  res <- switch(format,
                "rank" =  factor(bin, labels=qnames[1:n], ordered=TRUE),
                "interval" =  factor(
                  bin,  
                  labels=paste0("[",signif(left,digits=digits),",", signif(right,digits=digits),"]"), 
                  ordered=TRUE),
                "center" = center[bin],
                "left" = left[bin],
                "right" = right[bin]
  )                
  return(res)
}