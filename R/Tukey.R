

#' Additional interfaces to TukeyHSD
#' 
#' \code{\link{TukeyHSD}} requires use of \code{\link{aov}}.  
#' Since this is a hinderence for beginners, wrappers
#' have been provided to remove this need.
#' @rdname TukeyHSD.lm
#' @param x an object, for example of class \code{lm} or \code{formula}
#' @param data a data frame.  NB: This does not come second in the argument list.
#' @param which,ordered,conf.level,\dots just as in \code{\link{TukeyHSD}} from the \code{base} package
#' 
#' @export
TukeyHSD.lm <- function(x, which, ordered = FALSE, conf.level=0.95, ...) {
	stats::TukeyHSD( aov(x), which = which, ordered = ordered, conf.level = conf.level, ...)
}

#' @rdname TukeyHSD.lm
#' @examples
#' ## These should all give the same results
#' if (require(mosaicData)) {
#'   model <- lm(age ~ substance, data=HELPrct)
#'   TukeyHSD(model)
#'   TukeyHSD( age ~ substance, data=HELPrct)
#'   TukeyHSD(aov(age ~ substance, data=HELPrct))
#' }
#' @export
TukeyHSD.formula <- function(x, which, ordered = FALSE, conf.level=0.95, data=parent.frame(), ...) {
  TukeyHSD( lm(x, data=data, ...), which = which, ordered = ordered, conf.level=conf.level, ...)
}


