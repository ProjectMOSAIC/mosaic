#' Adaptively generate sequences in an interval
#'
#' \code{adapt_seq} is similar to \code{seq} except that instead of 
#' selecting points equally spaced along an interval, it selects points
#' such that the values of a function applied at those points are 
#' (very) roughly equally spaced.  This can be useful for sampling 
#' a function in such a way that it can be plotted more smoothly, 
#' for example.
#'
#'
#' @param from start of interval
#' @param to end of interval
#' @param length.out desired length of sequence
#' @param f a function
#' @param args arguments passed to \code{f}
#' @param quiet suppress warnings about NaNs, etc.
#' @return a numerical vector
#' @examples
#' adapt_seq(0, pi, 25, sin)
#'
#' @export
adapt_seq <-function(from, to, 
                     length.out=200, 
                     f=function(x,...){ 1 }, 
                     args=list(),
                     quiet=FALSE
) 
{
  n <- round(sqrt(length.out))
  n <- max(n,10)
  s <- seq(from, to, length.out=n)
  iteration <- 0
  keepers <- s
  
  repeat{
    iteration <- iteration + 1
    # clean up: remove bad s values
    if (quiet) {
      suppressWarnings(y <- do.call(f, args=c(list(s), args)))
    } else {
      y <- do.call(f, args=c(list(s), args))
    }
    w <- which(is.finite(y))
    a <- min(w) - 1
    a <- max(a,1)
    b <- max(w) + 1
    b <- min(b,length(s))
    w <- sort(unique(c(w, 1, length(s), a, b )))
    s <- s[w]; y <- y[w]
    
    # quit if we have enough or reach edge condition
    # print(list(i=iteration, a=s[a], b=s[b], start=head(s,4), end=tail(s,4)))
    if ((length(s) < 2) || (iteration > 20) || (length(s) > length.out)) 
      return(s[is.finite(y)])
    
    # keep best points + previous keepers
    ds <- ediff(s)
    dy <- ediff(y)
    angle <- atan(dy/ds)
    # score <- abs(ediff(dy/ds, pad="tail"))
    score <- abs(ediff(angle, pad="tail"))
    wsum <- cumsum(is.finite(y))
    a <- which.min( wsum== 1) - 1  # last non-finite
    a <- max(a,1)
    b <- which.min( wsum == max(wsum) ) # first of last run of non-finites
    b < min(b,length(s))
    score[c(a,b)] <- Inf     # keep a and b
    
    w <- which( s %in% keepers | score > quantile(score, probs=0.2, na.rm=TRUE))
    # be sure to keep left and right ends
    w <- sort(unique(c(w, 1, length(s))))
    s <- s[w]
    keepers <- s
    
    # create some more s values at midpoints
    ds <- diff(s)
    mid.s <- s[-1] - .5 * ds
    s <- sort( c(s, mid.s) )
    if (quiet) {
      suppressWarnings(y <- do.call(f, args=c(list(s), args)))
    } else {
      y <- do.call(f, args=c(list(s), args))
    }
  }

#    print(list(i=iteration, s=c(head(s), tail(s))))
  return(s[is.finite(y)])
}

