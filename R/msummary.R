
withClass <- function(object, class, add=TRUE) {
  if (add) {
    class(object) <- c(class, class(object))
  } else {
    class(object) <- class
  }
  object
}

#' @rdname msummary
#' @param x an object to summarize
#' @param digits desired number of digits to display
#' @param symbolic.cor see \code{\link{summary}}
#' @param signif.stars a logical indicating whether to diplay stars to 
#'   indicate significance
#' @param ... aditional arguments
#' @export
print.msummary.lm <-
  function (x, digits = max(3L, getOption("digits") - 3L), 
            symbolic.cor = x$symbolic.cor, 
            signif.stars = getOption("show.signif.stars"), ...) 
  {
    output <- capture.output( 
      print( withClass(x, "summary.lm"), 
             digits=digits, 
             symbolic.cor = symbolic.cor, 
             signif.stars=signif.stars, ...) )
  
    printCoefmat(x$coefficients, digits = digits,
                 signif.stars = signif.stars, signif.legend = FALSE)
    
    rows <- 1:length(output)
    w1 <- min( grep("Coefficients", output) ) 
    w2 <- which.max( ! grepl("\\d", output) & (rows > (w1 + 1)) ) 
    w3 <- which.max( nchar(output) == 0 & (rows >= w2) ) 
    keep <- (rows >= w3)
    cat( paste(output[keep], collapse="\n") )
    return(invisible(x))
  }

#' @rdname msummary
#' @export
print.msummary.glm <-
  function (x, digits = max(3L, getOption("digits") - 3L), 
            symbolic.cor = x$symbolic.cor, 
            signif.stars = getOption("show.signif.stars"), ...) 
  {
    output <- capture.output( 
      print(withClass(x, "summary.glm"), 
            digits=digits, 
            symbolic.cor = symbolic.cor, 
            signif.stars=signif.stars, ...) )
    w1 <- min( grep("Coefficients", output) ) 
    w2 <- which.max( ! grepl("\\d", output) & (1:length(output)) > (w1 + 1) ) 
    w3 <- which.max( nchar(output) == 0 & (1:length(output)) >= w2 ) 
    rows <- 1:length(output)
    keep <- ( (rows >= w1 & rows < w2) | rows >= w3)
    cat( paste(output[keep], collapse="\n") )
    return(invisible(x))
  }

#print.msummary.lm  <- print_summary_lm
#
#print.msummary.glm <- print_summary_glm
#'
#'
#' Modified summaries
#' 
#' \code{msummary} provides modified summary objects that typically produce
#' output that is either identical to or somewhat terser than their 
#' \code{\link{summary}} analogs.  The contents of the object itself are unchanged 
#' (except for an augmented class) so that other downstream functions should work as 
#' before.
#' 
#' @rdname msummary
#' 
#' @export
#' @param object an object to summarise
msummary <- function(object, ...)
  UseMethod("msummary")

#' @rdname msummary
#' @export
#' 
msummary.default <- function(object, ...) {
  summary(object, ...)
}

#' @rdname msummary
#' @export
msummary.lm <- function(object, ...) {
  res <- summary(object, ...)
  class(res) <- c("msummary.lm", class(res))
  res
}

#' @rdname msummary
#' @export
msummary.glm <- function(object, ...) {
  res <- summary(object, ...)
  class(res) <- c("msummary.lm", class(res))
  res
}
