#' @export
panel.lmbands <-
function (x, y, 
		  interval = "confidence", 
		  level = 0.95, 
		  model = lm(y ~ x), 
		  band.col = c(conf = trellis.par.get("superpose.line")$col[3], 
					   pred = trellis.par.get("superpose.line")$col[2]), 
		  band.lty = c( conf = trellis.par.get("superpose.line")$lty[3], 
		  				pred = trellis.par.get("superpose.line")$lty[2]),
		  band.show = TRUE,
		  fit.show = TRUE,
		  band.alpha = .6,
		  band.lwd = 1,
		  npts = 100,
		  ...) 
{
	band.alpha <- rep(band.alpha, length.out=2)
	band.lwd <- rep(band.lwd, length.out=2)
	band.show <- rep(band.show, length.out=2)

    fit <- makeFun(model)
	Clower <- function(x) {fit(x, interval="confidence")[,2]}
	Cupper <- function(x) {fit(x, interval="confidence")[,3]}
	Plower <- function(x) {fit(x, interval="prediction")[,2]}
	Pupper <- function(x) {fit(x, interval="prediction")[,3]}

	bandDots <- list(...)
	bandDots[['lty']] <- NULL
	bandDots[['col']] <- NULL
	bandDots[['alpha']] <- NULL
	bandDots[['lwd']] <- NULL

	if (bands.show[2])
	do.call( panel.plotFun, 
			c(list(Plower(x)~x, 
				   col=band.col[2], 
				   lty=band.lty[2], 
				   npts=npts, 
				   alpha=band.alpha[2], 
				   lwd=band.lwd[2]),
			  bandDots) )

	if (bands.show[2])
	do.call( panel.plotFun, 
			c(list(Pupper(x)~x, 
				   col=band.col[2], 
				   lty=band.lty[2], 
				   alpha=band.alpha[2], 
				   lwd=band.lwd[2],
				   npts=npts), 
			  bandDots) )

	if (bands.show[1])
	do.call( panel.plotFun, 
			c(list(Clower(x)~x, 
				   col=band.col[1], 
				   lty=band.lty[1], 
				   alpha=band.alpha[1], 
				   lwd=band.lwd[1],
				   npts=npts), 
			  bandDots) )

	if (bands.show[1])
	do.call( panel.plotFun, 
			c(list(Cupper(x)~x, 
				   col=band.col[1], 
				   lty=band.lty[1], 
				   alpha=band.alpha[1], 
				   lwd=band.lwd[1],
				   npts=npts), 
			  bandDots) )

	if (fit.show)
	panel.plotFun(fit(x) ~ x, ...)

    panel.xyplot(x, y, ...)
}
