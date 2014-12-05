
#' Alternative print functions
#' 
#' The functions provide alternatives to \code{print} methods
#' for various objects, generally providing terser output.  The
#' \code{\link{verbosity}} function can be used to make these 
#' replace the standard print methods.
#' 
#' @rdname print
#' @seealso \code{\link{verbosity}}
#' @export
print_summary_lm <-
  function (x, digits = max(3L, getOption("digits") - 3L), 
            symbolic.cor = x$symbolic.cor, 
            signif.stars = getOption("show.signif.stars"), ...) 
  {
    output <- capture.output( 
      stats:::print.summary.lm(x, digits=digits, 
                               symbolic.cor = symbolic.cor, 
                               signif.stars=signif.stars, ...) )
  
    printCoefmat(x$coefficients)
    
    rows <- 1:length(output)
    w1 <- min( grep("Coefficients", output) ) 
    w2 <- which.max( ! grepl("\\d", output) & (rows > (w1 + 1)) ) 
    w3 <- which.max( nchar(output) == 0 & (rows >= w2) ) 
    keep <- (rows >= w3)
    cat( paste(output[keep], collapse="\n") )
    return(invisible(x))
  }

#' @rdname print
#' @export
print_summary_glm <-
  function (x, digits = max(3L, getOption("digits") - 3L), 
            symbolic.cor = x$symbolic.cor, 
            signif.stars = getOption("show.signif.stars"), ...) 
  {
    output <- capture.output( 
      stats:::print.summary.glm(x, digits=digits, 
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

