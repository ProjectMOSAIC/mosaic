#' @export
panel.lmbands <-
function (x, y, 
		  interval = "confidence", 
		  level = 0.95, 
		  model = lm(y ~ x), 
		  pred.col = trellis.par.get("superpose.line")$col[2], 
    	  conf.col = trellis.par.get("superpose.line")$col[3], 
		  pred.lty = trellis.par.get("superpose.line")$lty[2], 
    	  conf.lty = trellis.par.get("superpose.line")$lty[3], 
		  resolution = 100,
		  ...) 
{
    fit <- makeFun(model)
	Clower <- function(x) {fit(x, interval="confidence")[,2]}
	Cupper <- function(x) {fit(x, interval="confidence")[,3]}
	Plower <- function(x) {fit(x, interval="prediction")[,2]}
	Pupper <- function(x) {fit(x, interval="prediction")[,3]}

	bandDots <- list(...)
	bandDots[['lty']] <- NULL
	bandDots[['col']] <- NULL

	do.call( panel.plotFun, c(list(Plower(x)~x, col=pred.col, lty=pred.lty), bandDots) )
	do.call( panel.plotFun, c(list(Pupper(x)~x, col=pred.col, lty=pred.lty), bandDots) )
	do.call( panel.plotFun, c(list(Clower(x)~x, col=conf.col, lty=conf.lty), bandDots) )
	do.call( panel.plotFun, c(list(Cupper(x)~x, col=conf.col, lty=conf.lty), bandDots) )
#	panel.plotFun(Pupper(x)~x, col=pred.col, lty=pred.lty)
#	panel.plotFun(Clower(x)~x, col=conf.col, lty=conf.lty)
#	panel.plotFun(Cupper(x)~x, col=conf.col, lty=conf.lty)
	panel.plotFun(fit(x) ~ x, ...)
    panel.xyplot(x, y, ...)
}
