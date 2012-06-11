#Numerically finds zeros in a couple dimensions.

#Want: to return a couple roots in a radius around a specified starting point.

#problems: Doesn't always work.
#change the x input to avoid warnings.
findZerosMult <- function(..., x=c(0,0), rad = 5, center=c(0,0)){
  system = list(...)
  
  #Need a way to enter in params to a system
  #Example: findZerosMult(a*x^2+b,b=10,x=c(0,0))
  
  numEq = length(system)
  vars = all.vars(system[[1]])
  for(i in (1:numEq)){
    vars = union(vars, all.vars(system[[i]]))
  }
  numVars = length(vars)
  for(i in (1:numEq)) system[[i]] = try(makeFun(system[[i]]),silent=TRUE) #Fix - sloppy
  if(numEq != numVars){#Need to add equations
    newEq = addEq(system,vars,rad, center)
    if(is.numeric(newEq)) return(numeric(0))
    system = list(...,newEq)
  }
  for(i in (1:numEq)){ 
    func = try(makeFun(system[[i]]),silent=TRUE) #Fix - sloppy
    if(!inherits(func, "try-error")) system[[i]] = func
  }
  res = try(Broyden(system, vars,x=x), silent=TRUE)
  if (inherits(res, "try-error")) {
    warning("No zeros found.  Try widening search.")
    return(numeric(0))
  }
  return(res)
}

#'Multi-Dimensional Root Finding
#'
#'Implementation of Broyden's root finding function to numerically compute the root of
#'a system of nonlinear equations
#'
#'@param system A list of functions
#'
#'@param vars A character string list of variables that appear in the functions
#'
#'@param x A starting vector
#'
#'@param tol The tolerance for the function specifying how precise it will be
#'
Broyden <- function(system, vars, x=0, tol = .Machine$double.eps^0.25, maxiters=1e5){
  browser()
  n = length(system)
  if(x==0) x = rep(0,length(system))#change this...
  if(toString(names(x))=="") names(x) = vars
  
  A = diag(n) #Default derivative is the identity matrix
  FF=.evalSys(x,system)
  
  iters=0
  while(max(abs(FF))>tol){#Should probably make this a for loop with maxiters.
    xnew=x-A%*%FF
    names(xnew)=names(x)
    del = xnew-x
    FFnew = .evalSys(xnew,system)
    Del = FFnew-FF
    Anew = A+((del-A%*%Del)%*%t(del)%*%A)/(t(del)%*%A%*%Del)[[1]]
    x=xnew
    A=Anew
    FF=FFnew
    iters=iters+1
    if(iters>maxiters) break
  }
  browser()
  return(data.frame(x,row.names=names(x)))
}

#
#Evaluates a system of equations at a given point.
#
.evalSys <- function(x,System){
  n=length(System)
  FF = rep(0,n)
  for( i in (1:n)){
    body = body(System[[i]])
    for (j in (1:n)){
      body =parse(text=gsub(names(x)[j], paste("(",toString(x[[j]]),")"), deparse(body)))[[1]]
    }
    FF[i]=eval(body)
  }
  return(FF)
}

addEq <- function(system,vars,rad,center){#should fix this for more vars. And clean it up...
  #browser()
  
  if(length(vars)==2){##Add a line through two points with opposite signs.
    set.seed(1) #set seed for random number generation
    done=FALSE
    for(i in (1:1000)){
      point1 =runif(length(vars), min=center[1]-rad, max=center[1]+rad)
      point2 =runif(length(vars), min=center[2]-rad, max=center[2]+rad)
      if(sign(do.call(system[[1]],as.list(point1)) ) != sign(do.call(system[[1]],as.list(point2)) )) done=TRUE
      for(i in (1:length(system))){
        if(sign(do.call(system[[i]],as.list(point1)) ) == sign(do.call(system[[i]],as.list(point2)) )) done=FALSE
      }
      if(done==TRUE) break
    }
    if(done==FALSE){
      warning("No zeros found. Try choosing a different start value or widening your search.")
      return(numeric(0))
    }
    f = function(){}
    formals(f) <- 
      eval(parse( 
        text=paste( "as.pairlist(alist(", 
                    paste(vars, "=", collapse=",", sep=""), "))"
        )
      ))
    m = (point2[2]-point1[2])/(point2[1]-point1[1])
    body(f)<-parse(text=paste(vars[2],"-", toString(point1[2])," - ",deparse(m),"*(",vars[1],"-",toString(point1[1]),")"))
    environment(f) <- environment(system[[1]])
    return(f)
  }
  
  else
    if(length(vars)>=3){##add one or two planes through points with opposite signs.
      set.seed(1) #set seed for random number generation
      done=FALSE
      for(i in (1:1000)){
        p1 =runif(length(vars), min=center[1]-rad, max=center[1]+rad)
        p2 =runif(length(vars), min=center[2]-rad, max=center[2]+rad)
        if(sign(do.call(system[[1]],as.list(p1)) ) != sign(do.call(system[[1]],as.list(p2)) )) done=TRUE
        for(i in (1:length(system))){
          if(sign(do.call(system[[i]],as.list(p1)) ) == sign(do.call(system[[i]],as.list(p2)) )) done=FALSE
        }
        if(done==TRUE) break
      }
      if(done==FALSE){
        warning("No zeros found. Try choosing a different start value or widening your search.")
        return(numeric(0))
      }
      f = function(){}
      formals(f) <- 
        eval(parse( 
          text=paste( "as.pairlist(alist(", 
                      paste(vars, "=", collapse=",", sep=""), "))"
          )
        ))
      newbody=""
      for(i in (1:length(p1))){
        newbody=paste(newbody, "(",toString(p2[i]),"-",toString(p1[i]),")*(",vars[i],"-",toString(p1[i]),")+")
      }
      newbody = paste(newbody,"0")
      body(f)<-parse(text=newbody)
      
      environment(f) <- environment(system[[1]])
      return(f)
    }
}