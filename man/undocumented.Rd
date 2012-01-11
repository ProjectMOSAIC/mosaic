\name{undocumented}
\docType{methods}
\alias{mosaic.getOption}
\alias{mosaic.options} 
\alias{mosaic.par.get} 
\alias{mosaic.par.set}

\alias{project}
\alias{project-methods}
\alias{project,numeric-method}
\alias{project,matrix-method}
\alias{project,formula-method}

\alias{print.function}
\alias{connector}
\alias{dot}
\alias{flowPlot}
\alias{graphPaper}
\alias{interpolatingFunction}
\alias{jacobianAtPoint}
\alias{linearModel}
\alias{mat}
\alias{makeFun}
\alias{rkintegrate}
\alias{rpoly2}
\alias{showNullclines}
\alias{showTraj}
\alias{singvals}
\alias{smoother}
\alias{solnPlot}
\alias{solveDE}
\alias{spliner}
\alias{trajPlot}

\title{Undocumented Objects}

\description{Documentation is still pending for these objects}

\author{Randall Pruim}

\examples{
# Is the distribution of golf ball numbers uniform? 
golfballs <- c(137, 138, 107, 104)                 # sample data;  n = 486
rgolfballs <- do(1000) * table(rdata(486, 1:4))    # random data assuming uniform dist
# Now compare based on various test statistics
print(statTally(golfballs, rgolfballs, max))   
print(statTally(golfballs, rgolfballs, min))
print(statTally(golfballs, rgolfballs, function(x) { diff( range(x)) } ))
print(statTally(golfballs, rgolfballs, sd))
distPlot('beta', params=list(shape1=3, shape2=1/3), kind='density')
distPlot('beta', params=list(shape1=3, shape2=1/3), kind='cdf')
dot(1:10, 1)  
project(1:10, 1)  
project(1:10, 1, type='length')  
g = rfun(~x, seed=833)
curve(g, from=-5,to=5) 
plotFun( g(x)~x, x=range(-5,5))
g = rfun(~x&y, seed=93232, n=2)
plotFun(g(x,y)~x&y, x=range(-3,3), y=range(-3,3))
h = linearModel(wage~age*educ, data=CPS)
h(age=20,educ=12)
}
