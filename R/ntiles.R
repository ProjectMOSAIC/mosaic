#' Create factor based on roughly equally sized groups
#'
#' @param x a numeric vector
#' @param n number of quantiles
#' @export
#' @examples
#' ntiles(1:50, 4)
#' 
ntiles <-  function(x, n=3){
  # Figure out names
  qnames <- c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th")
  if (n > 10) qnames <- c(qnames, paste(11:n,"th",sep=""))
  brks <- co.intervals( x, n, overlap=0 )[,1]
  brks[c(1,n+1)] <- c(-Inf, Inf)
  res <- cut(x, breaks=brks, labels=qnames[1:n], ordered_result=TRUE)
  return(res)
}