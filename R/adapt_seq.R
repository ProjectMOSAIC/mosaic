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
  n <- round(log(length.out))
  n <- max(n,10)
  s <- seq(from, to, length.out=n)
  iteration <- 0
  keepers <- s
  
  repeat{
    iteration <- iteration + 1
    # clean up: remove bad s values
    tryCatch(y <- do.call(f, args=c(list(s), args)), 
                  warning=function(w) if (quiet){} else {w})
    w <- which(is.finite(y))
    a <- min(which(is.finite(y))) - 1
    a <- max(a,1)
    b <- max(which(is.finite(y))) + 1
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
    score <- abs(ediff(dy/ds, pad="tail"))
    w <- which( s %in% keepers | score > quantile(score, probs=0.2, na.rm=TRUE))
    # be sure to keep left and right ends
    a <- min(which(is.finite(y))) - 1
    a <- max(a,1)
    b <- max(which(is.finite(y))) + 1
    b < min(b,length(s))
    w <- sort(unique(c(w, 1, length(s), a, b )))
    s <- s[w]
    keepers <- s
    
    # create some more s values at midpoints
    ds <- diff(s)
    mid.s <- s[-1] - .5 * ds
    s <- sort( c(s, mid.s) )
    tryCatch(y <- do.call(f, args=c(list(s), args)), 
                  warning=function(w) if (quiet){} else {w})

  }

#    print(list(i=iteration, s=c(head(s), tail(s))))
  return(s[is.finite(y)])
}

