utils::globalVariables(c('panel.plotFun1')) 

#' show confidence and preciction bands on plots
#' 
#' @param x,y numeric vectors
#' @param interval a vector subset of \code{'confidence'} and \code{'prediction'}
#' @param level conficence level
#' @param model model to be used for generating bands
#' @param band.col a vector of length 1 or 2 giving the color of bands 
#' @param band.lty a vector of length 1 or 2 giving the line type for bands
#' @param band.show logical vector of length 1 or 2 indicating whether 
#' confidence and prediction bands should be shown
#' @param fit.show logical indicating whether the model fit should be shown
#' @param band.alpha a vector of length 1 or 2 alpha level for bands
#' @param band.lwd a vector of length 1 or 2 giving line width for bands
#' @param npts resolution parameter for bands (increase to get better resolution)
#' @param \dots additional arguments
#' 
#' @export

panel.lmbands <-
function (x, y, 
		  interval = "confidence", 
		  level = 0.95, 
		  model = lm(y ~ x), 
		  band.col = c(conf = slcol[3], pred=slcol[2]),
		  band.lty = c(conf = slty[3], pred=slty[2]),
		  band.show = TRUE,
		  fit.show = TRUE,
		  band.alpha = .6,
		  band.lwd = 1,
		  npts = 100,
		  ...) 
{
  slcol <- trellis.par.get("superpose.line")$col
  slty  <- trellis.par.get("superpose.line")$lty
  
  inflate <- function (x, size, default) {
    if (is.null(x)) return( rep(default,length.out=size) )
    x <- x[!is.na(x)]
    if (length(x) == 0) { return(rep(default, length.out=size)) }
    rep(x, length.out=size)
  }

	band.alpha <- inflate(band.alpha, 2, 0.6)
	band.lwd <- inflate(band.lwd, 2, 1)
	band.show <- inflate(band.show, 2, TRUE)
	band.col <- inflate(band.col, 2, "gray50")
  band.lty <- inflate(band.lty, 2, 1)

  fit <- makeFun(model)
	Clower <- function(x) {fit(x, interval="confidence", level=level)[,2]}
	Cupper <- function(x) {fit(x, interval="confidence", level=level)[,3]}
	Plower <- function(x) {fit(x, interval="prediction", level=level)[,2]}
	Pupper <- function(x) {fit(x, interval="prediction", level=level)[,3]}

	dots <- list(...)
  bandDots <- dots
	bandDots[['lty']] <- NULL
	bandDots[['col']] <- NULL
	bandDots[['alpha']] <- NULL
	bandDots[['lwd']] <- NULL

  if (band.show[2]) {
    do.call( panel.plotFun1, 
             c(list(makeFun(Plower(x)~x), 
                    col=band.col[2], 
                    lty=band.lty[2], 
                    npts=npts, 
                    alpha=band.alpha[2], 
                    lwd=band.lwd[2]),
               bandDots) )
    
    do.call( panel.plotFun1, 
             c(list(makeFun(Pupper(x)~x), 
                    col=band.col[2], 
                    lty=band.lty[2], 
                    alpha=band.alpha[2], 
                    lwd=band.lwd[2],
                    npts=npts), 
               bandDots) )
  }

  if (band.show[1]) {
    do.call( panel.plotFun1, 
             c(list(makeFun(Clower(x)~x), 
                    col=band.col[1], 
                    lty=band.lty[1], 
                    alpha=band.alpha[1], 
                    lwd=band.lwd[1],
                    npts=npts), 
               bandDots) )
    
    do.call( panel.plotFun1, 
             c(list(makeFun(Cupper(x)~x), 
                    col=band.col[1], 
                    lty=band.lty[1], 
                    alpha=band.alpha[1], 
                    lwd=band.lwd[1],
                    npts=npts), 
               bandDots) )
  }

	if (fit.show) {
	  do.call( panel.plotFun1, c(list(makeFun(fit(x) ~ x)), dots) )
	}

  do.call("panel.xyplot", c(list(x, y), dots))
}
