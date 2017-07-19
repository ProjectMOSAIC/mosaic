# This allows us to use these functions from MASS without attaching the MASS package.

#' @importFrom MASS fitdistr
#' @export
MASS::fitdistr

#' @importFrom MASS fractions
#' @export
MASS::fractions


#' @importFrom mosaicCore lhs rhs condition
#' @export
mosaicCore::lhs
#' @export
mosaicCore::rhs
#' @export
mosaicCore::condition