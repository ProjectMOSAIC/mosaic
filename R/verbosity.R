#' Terser print methods
#' 
#' Some of the standard print methods are a bit verbose.  These functions 
#' can be used to reduce the verbosity.  
#'
#' @export 

verbosity <- function( level=c("low", "high") ) {
  level <- match.arg(level)
  
  if (level == "low") { 
    print.summary.lm  <<- print_summary_lm
    print.summary.glm <<- print_summary_glm
  } else {
    print.summary.lm  <<- stats:::print.summary.lm
    print.summary.glm <<- stats:::print.summary.glm
  }
}