
x <- rep(1:6, each=5) + runif(6 * 5, -.2, .2)
y <-  1*x + 3 + rnorm(30,3)
lsline <- function( x, y, a=NULL, b=0, ... ) {
  if (is.null(a)) {
    a <- mean(y) - b * mean(x) 
  }
  xyplot(y ~ x, 
    panel=function(x,y,...) {
      resid <- y - a - b*x
      grid::grid.rect( x, y, width=resid, height=resid, default.units="native",
        gp=grid::gpar(fill='red',col='red',alpha=0.1),
		    hjust = ifelse (resid < 0, 1, 0),
        vjust = ifelse (resid < 0, 1, 0) 
      )
      panel.xyplot(x,y,...)
      panel.abline(a,b)
      grid::pushViewport(grid::viewport(clip="off"))
      grid::grid.text( x=.95, y=1.01, just=c('right','bottom'), format(sum(resid^2)), 
        gp=grid::gpar(col='red')
      )
      grid::upViewport()
    },
    ...
  )
}

if (require(manipulate)) {
  manipulate( lsline(x,y,a=NULL, b=slope, 
  		type=if (show.regress) c('p','r') else 'p'),
    slope = slider(-5,10, initial=0, step=0.05),
    show.regress = checkbox(FALSE, label="Show regression line")
  )
} else {
  xpnorm( 750, 500, 100 )
}
