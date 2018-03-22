# This allows us to use these functions from MASS without attaching the MASS package.

#' @importFrom MASS fitdistr
#' @export
MASS::fitdistr

#' @importFrom MASS fractions
#' @export
MASS::fractions


#' @importFrom mosaicCore lhs rhs condition makeFun 
#' @export
mosaicCore::lhs
#' @export
mosaicCore::rhs
#' @export
mosaicCore::condition
#' @export
mosaicCore::makeFun
#' @export
mosaicCore::counts
#' @export
mosaicCore::props
