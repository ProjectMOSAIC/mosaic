#' Undocumented Objects
#' 
#' Documentation is still pending for these objects
#' 
#' @rdname undocumented
#' @name undocumented
#' 
#' @aliases mosaic.getOption mosaic.options mosaic.par.get mosaic.par.set project project-methods project;numeric-method project;matrix-method project;formula-method print.function connector dot flowPlot graphPaper interpolatingFunction jacobianAtPoint linearModel mat makeFun rfun rkintegrate rpoly2 showNullclines showTraj singvals smoother solnPlot solveDE spliner trajPlot
#' 
#' 
#' @author Randall Pruim (\email{rpruim@@calvin.edu})
#' 
#' @examples
#' # Is the distribution of golf ball numbers uniform? 
#' golfballs <- c(137, 138, 107, 104)                 # sample data;  n = 486
#' rgolfballs <- do(1000) * table(rdata(486, 1:4))    # random data assuming uniform dist
#' # Now compare based on various test statistics
#' print(statTally(golfballs, rgolfballs, max))   
#' print(statTally(golfballs, rgolfballs, min))
#' print(statTally(golfballs, rgolfballs, function(x) { diff( range(x) )} ))
#' print(statTally(golfballs, rgolfballs, sd))
#' distPlot('beta', params=list(shape1=3, shape2=1/3), kind='density')
#' distPlot('beta', params=list(shape1=3, shape2=1/3), kind='cdf')
#' dot(1:10, 1)  
#' project(1:10, 1)  
#' project(1:10, 1, type='length')  
#' g = rfun(~x, seed=833)
#' curve(g, from=-5,to=5) 
#' plotFun( g(x)~x, x=range(-5,5))
#' g = rfun(~x&y, seed=93232, n=2)
#' plotFun(g(x,y)~x&y, x=range(-3,3), y=range(-3,3))
#' h = linearModel(wage~age*educ, data=CPS)
#' h(age=20,educ=12)
#' 

NA
