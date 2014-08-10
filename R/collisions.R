
#' Maximum-likelihood fitting of univariate distributions
#' 
#' Maximum-likelihood fitting of univariate distributions (from \pkg{MASS})
#' @param x see \code{link[MASS]{fitdistr}} in the \pkg{MASS} package
#' @param densfun see \code{link[MASS]{fitdistr}} in the \pkg{MASS} package
#' @param start see \code{link[MASS]{fitdistr}} in the \pkg{MASS} package
#' @param ... see \code{link[MASS]{fitdistr}} in the \pkg{MASS} package
#' @export
fitdistr <- MASS::fitdistr

#' Rational Approximation
#' 
#' Rational Approximation from \pkg{MASS}.
#' 
#' @param x see \code{link[MASS]{fractions}} in the \pkg{MASS} package
#' @param cycles see \code{link[MASS]{fractions}} in the \pkg{MASS} package
#' @param max.denominator see \code{link[MASS]{fractions}} in the \pkg{MASS} package
#' @param ... see \code{link[MASS]{fractions}} in the \pkg{MASS} package
#' @export
fractions <- MASS::fractions