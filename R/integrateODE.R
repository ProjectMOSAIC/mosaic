#' Integrate ordinary differential equations 
#'
#' A formula interface to integration of an ODE
#'
#' @params \ldots arguments
#'
#' @details
#' The equations must be in first-order form.  Each dynamical equation uses
#' a formula interface with the variable name given on the left-hand side of the
#' formula, preceeded by a \code{d}, so use \code{dx~-k*x} for exponential decay.
#' All parameters (such as \code{k}) must be assigned numerical values in the
#' argument list.  All dynamical variables must be assigned initial conditions in the
#' argument list.  The returned value will be a list with one component named after each
#' dynamical variable.  The component will be a spline-generated function of \code{t}.
#' 
#' @author Daniel Kaplan (\email{kaplan@@macalester.edu})
#'
#' @returns a list with splined function of time for each dynamical variable
#'
#' @examples
#' soln = integrateODE( dx~r*x*(1-x/k),k=10,r=.5,tdur=20,x=1)
#' soln$x(10)
#' soln$x(30) # outside the time interval for integration
#' plotFun( soln$x(t)~t, t=range(0,20))
#' soln2 = integrateODE(dx~y, dy~-x, x=1,y=0,tdur=10)
#' plotFun( soln2$y(t)~t, t=range(0,10))
#' SIR epidemic
#' epi = integrateODE( dS~-a*S*I, dI~a*S*I - b*I, a=0.0026,b=.5,S=762,I=1,tdur=20)
integrateODE = function(...) {
  inputs <- list(...)
  # handle the period of integration
  tdur = inputs[["tdur"]] # will be null if not specified
  if( is.null(tdur) ) tdur = list(from=0, to=1, dt=0.01)
  if( is.numeric(tdur) ) tdur = list(from=0,to=tdur,dt=0.01)
  if( is.null(tdur$from) ) tdur$from = 0
  if( is.null(tdur$dt) ) tdur$dt = diff(range(tdur$from,tdur$to))/1000
  # pull out the differential equations 
  eqsInds <- c()
  for (k in 1:length(inputs)) {
    if (class(inputs[[k]])=="formula")
      eqsInds <- c(eqsInds, k)
  }
  dnames <- c()
  dfuns <- c()
  for (k in 1:length(eqsInds) ) {
    tmp = inputs[[eqsInds[k]]]
    nm = tmp[[2]] # should be name type so double [[ ]]
    if (class(nm) != "name") stop(paste("Invalid name on LHS of formula",nm))
    nm = as.character(nm)
    if (grep("^d",nm)!=1) stop("Dynamical variables must start with 'd'")
    dnames[k] <- sub("^d","",nm) # character string with the name
    dfuns[k] <- parse(text=tmp[3]) # an expression so single [ ]
  }

  # get the additional assignments in the argument list
  additionalInds = (!names(inputs) %in% c(dnames,"tdur")) & nchar(names(inputs))!=0 &
    names(inputs) != "t"  # t is a special input
  additionalAssignments = inputs[additionalInds]

  # construct a function representing the dynamics
  # parameters are stored as extra arguments
  # the order of the dynamical variables (and "t") is important and will be used
  # later
  dynfun = function(){}
  # construct the dynamical variable argument list
  tstring=ifelse(! "t"%in% dnames,",t=","")
  ex = parse(text=paste("c(",paste(dnames,dfuns,collapse=",",sep="="),")",sep=""))

  # create the dynamical variables as arguments to the function
  dynArgs = eval(parse(
           text=paste("alist(",  paste(dnames,"=",collapse=",",sep=""), tstring,")")))
  formals(dynfun) = c(dynArgs,additionalAssignments)
  body(dynfun) = ex
  # create a functions with a vector argument of state, for use in rk()
  wrapper = function(state,t) {}
  tstring=ifelse(! "t"%in% dnames,",t","")
  bd = paste("dynfun(",
    paste("state[",1:length(dnames),"]",sep="",collapse=","),tstring,")")
  body(wrapper) = parse(text=bd)
  #create the initial condition vector
  initstate = unlist( inputs[dnames] )
  if (length(initstate) != length(dnames) )
    stop(paste("Must specify an initial condition for every variable."))
  soln = rkintegrate(wrapper,initstate,tstart=tdur$from,tend=tdur$to,dt=tdur$dt)
  
  # Return an object with functions for each of the dynamical variables,
  # defined as NA outside the range of tdur$from to tdur$to.
  # return interpolating functions
  res <- list()
  for (k in 1:length(dnames)) res[[k]] <- approxfun( soln$t, soln$x[,k])
  names(res) <- dnames
  return(res)
}
#' A simple Runge-Kutte integrator
#'
#' Integrates ordinary differential equations using a Runge-Kutta method
#'
#' @param fun the dynamical function with arguments \code{state} (a vector) and \code{t}.
#' @param x0 the initial condition, a vector with one element for each state variable
#' @param tstart starting time
#' @param tend ending time for integration
#' @dt step size for integration
#'
#' @returns a list containing \code{x}, a matrix of the state with one row for each
#' time step and a vector \code{t} containing the times of those steps.
#'
#' @author Daniel Kaplan (\email{kaplan@@macalester.edu})
#'
#' @details
#' This is mainly for internal use by integrateODE.

# Runge-Kutta integration
rkintegrate <- function(fun,x0,tstart=0,tend=1,dt=NULL) {
  if (is.null(dt)) {
    dt <- if( tend > 0 ) min(.01, (tend - tstart)/100)
        else max(-.01, (tend-tstart)/100)
  }
  nsteps <- round( .5+(tend-tstart)/dt);
  xout <- matrix(0,nsteps+1,length(x0));
  tout <- seq(tstart,tend,length=nsteps+1);
  xout[1,] <- x0;
  for (k in 2:(nsteps+1)) {
      time = tout[k]
      k1 <- dt*fun(x0,tout[k-1]);
      k2 <- dt*fun(x0+k1/2,time);
      k3 <- dt*fun(x0+k2/2,time);
      k4 <- dt*fun(x0+k3,time);
      x0 <- x0 + (k1+k4+(k2+k3)*2)/6;
      xout[k,] <- x0;
  }
  return( list(x=xout,t=tout) );
} 
