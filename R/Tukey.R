#' @rdname TukeyHSD
#' Additional interfaces to TukeyHSD
#' 
#' TukeyHSD requires use of \code{\link{aov}}.  Since this is a hinderence for beginners, wrappers
#' have been provided to remove this need.
#' @param x an object, for example of class \code{lm} or \code{formula}
#' @param which,ordered,conf.level,\dots just as in \code{\link{TukeyHSD}} from the \code{base} package
#' 
#' @method TukeyHSD,lm
TukeyHSD.lm <- function(x, which, ordered = FALSE, conf.level=0.95, ...) {
  TukeyHSD.aov( aov(x), which =which, ordered = ordered, conf.level=conf.level, ...)
}

#' @rdname TukeyHSD
#' @method TukeyHSD,formula
TukeyHSD.formula <- function(x, data=parent.frame(), which, ordered = FALSE, conf.level=0.95, ...) {
  TukeyHSD.lm( lm(x, data=data, ...), which = which, ordered = ordered, conf.level=conf.level, ...)
}

#' @export
#' @examples
#' ## These should all give the same results
#' model <- lm(age ~ substance, HELPrct)
#' TukeyHSD(model)
#' TukeyHSD( age ~ substance, HELPrct)
#' TukeyHSD(aov(age ~ substance, HELPrct))