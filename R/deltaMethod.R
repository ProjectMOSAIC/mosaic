#' deltaMethod has moved to a separate package
#' 
#' \code{deltaMethod} has moved to its own separate package.
#' @examples
#' deltaMethod()
#' 
#' @export
deltaMethod.data.frame <- function(object, ...) {
  .Defunct("deltaMethod", package="deltaMethod", 
           msg = "deltaMethod.data.frame is now in the deltaMethod package.")
}