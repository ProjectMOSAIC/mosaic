#' Plot a regression model
#' 
#' Visualize a regression model amid the data that generated it. 
#' 
#' @param x A model of type \code{lm} or \code{glm}
#'
#' @param \dots additional arguments
#' 
#' @return A lattice graphics object
#' 
#' @seealso \code{\link{plotPloints}, \link{plotFun}}
#' @export
#' @examples
#' mod = lm( width ~ length + sex + length*sex, data=KidsFeet)
#' plotModel(mod)
#' 

# https://github.com/rpruim/mosaic/issues/40

plotModel <- function( x, data=parent.frame(), ...) {
  if (!(class(x) %in% c("lm", "glm"))) {
    stop(paste("You cannot plotModel with an object of type", class(x)))
  }
  # Determine the dimensionality of the problem
  var.classes = attr(terms(x), "dataClasses")
  var.names = names(var.classes)
  y.name = as.character(lhs(terms(x)))
  x.names = setdiff(names(which(var.classes == "numeric")), y.name)
  z.names = setdiff(names(which(var.classes != "numeric")), y.name)
  pal = trellis.par.get("superpose.symbol")$col
  if (length(x.names) == 1) {
    # draw a scatterplot
    x.name = x.names[1]
    myplot = xyplot(as.formula(paste(y.name, "~", x.name)), data=mod$model)
    
    # make the model into a function
    fmod = makeFun(x)
    # get information about any categorical variables
    z.vals = x$xlevels
    
    if (length(z.vals) == 1) {
      # parallel slopes model
      z.levels = z.vals[[1]]
      # how many regression lines are there?
      k = length(z.levels)
      # put the regression lines on the plot
      for(i in 1:k) {
        # hardcoding works
#        plotFun(fmod(x, "B") ~ x, col = pal[i], add=TRUE)
        # but this doesn't work?? Why? 
        form = as.formula(paste( "fmod(x, \'", z.levels[i], "\') ~ x", sep="" ))
        myplot = plotFun(form, col=pal[i], add=TRUE)
#        plotFun(fmod(x, a) ~ x, a = as.character(z.levels[i]), col = pal[i], add=TRUE)
      }
    }
  } else if (num.q.vars == 3) {
    # draw a 3D plot
  } else if (num.q.vars > 3) {
    # too many variables
    stop("You cannot plotModel if you have more than 3 numeric variables")
  }
  return(myplot)
}