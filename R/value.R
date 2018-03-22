#' Extract value from an object
#' 
#' Functions like [integrate()] and [nlm()] return objects that contain more 
#' information that simply the value of the integration or optimization.  `value()` extracts
#' the primary value from such objects.  Currently implemented situations include the output from
#' [integrate()],
#' [nlm()],
#' [cubature::adaptIntegrate()], and 
#' [uniroot()].
#' 
#' @rdname value
#' @param object an object from which a "value" is to be extracted.
#' @param ... additional arguments (currently ignored).
#' @export
#' @examples
#' integrate(sin, 0, 1) %>% value()
#' nlm(cos, p = 0) %>% value()
#' uniroot(cos, c(0, 2)) %>% value()
 
value <- function(object, ...) {
  UseMethod("value")
}

#' @rdname value
#' @export
value.integrate <- function(object, ...) {
  object$value
}

#' @rdname value
#' @export
value.default <- function(object, ...) {
 
  # for anything with a value slot -- in particular integrate() 
  if ("value" %in% names(object))
    return(object$value)
  
  if ("estimate" %in% names(object))
    return(object$estimate)
  
  # # for nlm()
  # 
  # if (all(c("estimate", "minimum", "gradient", "code", "iterations") %in% names(object)))
  #   return(object$estimate)
 
  # for uniroot() 
  if (all(c("root", "f.root", "iter", "init.it", "estim.prec") %in% names(object)))
    return(object$root)
  
  # for cubature::adaptIntegrate()
  if (all(c("integral", "error", "functionEvaluations", "returnCode")  %in% names(object))){   
    if (! object$returnCode == 0L)
      warning("Non-zero return code from adaptIntegrate()", call. = FALSE)
    return(object$integral)
  }
 
  if (is.numeric(object))
    return(as.numeric(object))  # strip off attributes, etc.
  
  # if all else fails...
  return(object)
}


