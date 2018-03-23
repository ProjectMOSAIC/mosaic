# This allows us to use these functions from MASS without attaching the MASS package.

#' @importFrom MASS fitdistr
#' @export
MASS::fitdistr

#' @importFrom MASS fractions
#' @export
MASS::fractions


#' @importFrom mosaicCore lhs rhs condition makeFun prop prop1 count tally
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
#' @export
mosaicCore::prop
#' @export
mosaicCore::prop1
#' @export
mosaicCore::perc
#' @export
mosaicCore::count
#' @export
mosaicCore::tally
