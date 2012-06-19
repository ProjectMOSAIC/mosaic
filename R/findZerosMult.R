#'Find the zeros of a function of two or more variables
#'
#'Compute numerically the zeros of a function.
#'All free variables (all but the variable on the right side) named in the expression must be assigned 
#' a value via \code{\ldots}
#'
#'@param ... arguments for values 
#'@param x starting vector (guess for Broyden)
#'@param npts number of desired zeros to return
#'@param rad radius around center in which to look for zeros
#'@param center center of search for zeros
#'
findZerosMult <- function(..., x=c(0,0), npts=10, rad = 5, center=c(0,0)){
  dots = list(...)
  system = list()
  freeVars = list()
  roots = data.frame()#where we will store the roots.
  
  #Separate formulae and free vars
  for(i in (1:length(dots))){
    if(class(dots[[i]])=="formula")
      system = append(system, dots[[i]]) #system contains all equations
    else{
      if(class(dots[[i]])=="numeric")
        freeVars[[names(dots)[i]]] <- dots[[i]] #freeVars contains values we will sub in
      else stop(paste("Improper value: ", deparse(dots[[i]])))
    }
  }
  numEq = length(system)
  browser()
  #Extract the rhs variables.
  rhsVars = all.vars(rhs(system[[1]]))
  for(i in (1:length(system))){
    rhsVars = union(rhsVars, all.vars(rhs(system[[i]])))
  }
  
  allvars = all.vars(system[[1]])
  for(i in (1:length(system))){
    allvars = union(allvars, all.vars(system[[1]]))
  }
  
  lhsOnlyVars = setdiff(allvars, rhsVars)
  
  #make sure all equations in the system are function.
  for(i in (1:numEq)) system[[i]] = try(makeFun(system[[i]]),silent=TRUE) #Fix - sloppy
  
  if(numEq != length(rhsVars)){#Need to add equations
    #if there is only one equation, call .oneEq
    if(length(system)==1){ 
      points = .findPoints(system, freeVars, rhsVars, rad, center, npts)
      newEqs = list()
      for(i in (1:length(system))){
        for(j in (1:length(rows(points[[i]])))){
          pt1= points[[i]][j,]
          for(k in (1:length(rows(points[[length(points)-i+1]])))){
            pt2 = points[[length(points)-i+1]][k,]
            #Find the root
            newf<-function(t){}
            newbody = "{"
            for(l in (1:length(rhsVars))){
              newbody = paste(newbody,rhsVars[l], "=t*",toString(pt1[l]),
                              "+(1-t)*",toString(pt2[l]),"\n")
            }
            newbody = parse(text = paste(newbody, 
                            "do.call(system[[1]],as.list(c(parse(text=rhsVars), freeVars)))}"))
            body(newf)<- newbody
            troot = uniroot(newf, c(0,1))$root
            root=pt1
            root = troot*pt1+(1-troot)*pt2
            #for(l in (1:length(rhsVars)))
            #  root[l] = troot*pt1[l]+(1-troot)*pt2[l]
            roots <- rbind(roots, root)
            if(length(rows(roots)) >= npts){
              colnames(roots) = rhsVars
              
              return(roots[order(roots[1]),]) #return ordered by the first variable.
            }
          }
        }
      }
      colnames(roots) = rhsVars
      return(roots[order(roots[1]),])
    }
    #some sort of error message?
    
      #return(.oneEq(system, vars=rhsVars, rad, center))
    #otherwise, keep adding equations until we can call Broyden?
    newEq = .addEq(system,vars=rhsVars,num=length(rhsVars)-numEq,rad, center)
    if(is.numeric(newEq)) return(numeric(0))
    system = append(system,newEq)
  }
  
  res = try(Broyden(system, rhsVars,x=x), silent=TRUE)
  if (inherits(res, "try-error")) {
    warning("No zeros found.  Try a different start value or widening search.")
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
#'@param maxiters maximum number of iterations.
#'
Broyden <- function(system, vars, x=0, tol = .Machine$double.eps^0.5, maxiters=1e5){
  n = length(system)
  if(is.null(x)) x = rep(0,length(system))#Add in something that makes sure this is valid.
  if(toString(names(x))=="") names(x) = vars
  
  A = diag(n) #Default derivative is the identity matrix
  
  #Evaluates a system of equations at a given point.
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
  
  
  FF=.evalSys(x,system)
  
  for(iter in (1:maxiters)){
    if(max(abs(FF))<tol) break
    xnew=x-A%*%FF
    names(xnew)=names(x)
    del = xnew-x
    FFnew = .evalSys(xnew,system)
    Del = FFnew-FF
    Anew = A+((del-A%*%Del)%*%t(del)%*%A)/(t(del)%*%A%*%Del)[[1]]
    x=xnew
    A=Anew
    FF=FFnew
  }
  return(data.frame(zeros=x,row.names=names(x)))
}

#returns npts number of points sorted into bins.  the ith bin can be paired with the
#ith to last bin and used to find the equation of a line with a zero on it.
#
.findPoints<- function(system, freeVars, vars, rad, center, npts = 100){
  numBins = 2^(length(system))
  points = list()
  for(i in (1:numBins)) points[[i]] = data.frame()
  set.seed(1) #set seed for random number generation
  done=FALSE
  for(i in (1:100)){
    point1 =runif(length(vars), min=center[1]-rad, max=center[1]+rad)
    if(sign(do.call(system[[1]], as.list(c(point1, freeVars))) )==1){#change for more than 2 binds
      points[[1]] = rbind(points[[1]], point1)
    }
    else{
      points[[2]] = rbind(points[[2]], point1)
    }
  }
  #points[[1]] = sort(as.numeric(points[[1]]))
  #points[[2]] = sort(as.numeric(points[[2]])) Don't want to sort yet
  
  if(length(points[[1]])==0||length(points[[2]])==0){
    warning("No zeros found. Try choosing a different start value or widening your search.")
    return(numeric(0))
  }
  for(i in (1:length(points))) colnames(points[[i]]) = vars
  return(points)
}

#param system, existing system of equations the new ones will be added to
#param vars the rhs variables of the equations
#param num the numer of equations to be added
#param rad the radius of search for new equations
#param center the center of the search region
#
#add functionality for num
.addEq <- function(system,vars,num, rad,center){#should fix this for more vars. And clean it up...
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

.oneEq <- function(system, vars, rad, center){#depreciated
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
  points = .findPoints(system, vars, rad, center)
  
  newf<-function(t){}
  newbody = "{"
  for(i in (1:length(vars))){
  newbody = paste(newbody,vars[i], "=t*",toString(p1[i]), "+(1-t)*",toString(p2[i]),"\n")
  }
  newbody = parse(text = paste(newbody, "do.call(system[[1]],as.list(parse(text=vars)))}"))
  body(newf)<- newbody
  
  troot = uniroot(newf, c(-rad, rad))$root
  root=p1
  for(i in (1:length(vars)))
    root[i] = troot*p1[i]+(1-troot)*p2[i]
  return(data.frame(zeros=root,row.names=vars))
}