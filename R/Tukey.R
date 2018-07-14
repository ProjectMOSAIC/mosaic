#' @importFrom stats reorder
NA

#' Additional interfaces to TukeyHSD
#' 
#' [TukeyHSD()] requires use of [aov()].  
#' Since this is a hindrance for beginners, wrappers
#' have been provided to remove this need.
#' @rdname TukeyHSD.lm
#' @param x an object, for example of class `lm` or `formula`
#' @param data a data frame.  NB: This does not come second in the argument list.
#' @param which,ordered,conf.level,\dots just as in [TukeyHSD()] from the `base` package
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


