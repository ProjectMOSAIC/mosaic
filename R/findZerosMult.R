#'Find the zeros of a function of two variables
#'
#'Compute numerically the zeros of a function of two variables.
#'All free variables (all but the variable on the right side) named in the expression must be assigned 
#' a value via \code{\ldots}
#'
#'@param ... arguments for values NOTE: if the system has more than one equation and the rhs
#'variables do not match up, there will be an error.
#'@param npts number of desired zeros to return
#'@param rad radius around center in which to look for zeros
#'@param center center of search for zeros
#'@param sortBy options for sorting zeros for plotting.  Options are 'byx', 'byy' and 'radial'.  The
#'default value is 'byx'.
#'
#'@details sorts points in the domain according to the sign of the function value at respective points.
#' Use continuity and uniroot to find zeros between points of opposite signs.  Returns any number of
#' points which may be sorted and plotted according to x, y, or radial values.
#' 
#'@return A data frame of numerical values which should all result in a value of zero when input into
#' original function
#' 
#'@examples
#' findZerosMult(a*x^2-8~a&x, npts = 50)
#' findZerosMult(a^2+x^2-8~a&x, npts = 1000, sortBy='radial')
#'
findZerosMult <- function(..., npts=10, rad = 5, center=NULL, sortBy='byx'){
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
  
  if(is.null(center))
    center = rep(0, length(rhsVars))
  #make sure all equations in the system are functions.
  for(i in (1:numEq)) system[[i]] = try(makeFun(system[[i]]),silent=TRUE) #Fix - sloppy
  
  #if there is only one equation, use uniroot
  if(length(system)==1){ 
    points = .findPoints(system, freeVars, rhsVars, rad, center, npts)
    for(j in (1:length(rows(points[[1]])))){
      pt1= points[[1]][j,]
      for(k in (1:length(rows(points[[length(points)]])))){
        pt2 = points[[2]][k,]
        #Find the root
        newf<-function(t){}
        #construct the body of the function.
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
        roots <- rbind(roots, root)
        if(length(rows(roots)) >= npts){
          colnames(roots) = rhsVars
          return(.sort(roots, center = center, type=sortBy)) #return ordered by the first variable.
        }
      }
    }
    colnames(roots) = rhsVars
    return(.sort(roots, center = center, type=sortBy))
  }
  
  #Use Broyden when system has more than one equation.
  if(length(system)>1){
    set.seed(23)
    
    if(length(system) < length(rhsVars)){ #Need to add equations
      junk = runif(length(rhsVars), min=-2, max=2)
      if(1< (length(rhsVars) - length(system)))
        for(i in 1:(length(rhsVars) - length(system)))
          junk = cbind(junk, runif(length(rhsVars, min=-2,max=2)))
      points = .findPoints(system, freeVars, rhsVars, rad, center, npts)
      for(i in (1:length(system))){
        for(j in (1:(length(rows(points[[i]]))))){#might be a more efficient way to do this
          for(k in (1:(length(rows(points[[length(points)-i+1]]))))){
            browser()
            pt1 = points[[i]][j,]
            pt2 = points[[length(points)-i+1]][k,]
            
            m = cbind(t(pt2-pt1), junk)
            norm = qr(m)$qr[,2]
            hyperplane = function(){}
            body(hyperplane) <- parse(text=paste("(c(",
              toString(norm), ")%*%(c(", paste(rhsVars, collapse=","), ")-c(", toString(pt1), ")))[[1]]", sep=""
              ))
            formals(hyperplane) <- eval(parse( 
              text=paste( "as.pairlist(alist(", 
                          paste(rhsVars, "=", collapse=",", sep=""), "))"
              )
            ))
            environment(hyperplane) <- environment(system[[1]])
            newsystem = append(system, hyperplane)
            root = try(Broyden(newsystem, rhsVars, x=center+.001, maxiters=1e3), silent=TRUE)
            if(!inherits(root, "try-error"))
              roots = rbind(roots, root)
            if(length(rows(roots)) >= npts){
              colnames(roots) = rhsVars
              return(.sort(roots, center = center, type=sortBy)) #return ordered by the first variable.
            }
          }
        }
      }
      colnames(roots) = rhsVars
      return(.sort(roots, center = center, type=sortBy))

    }
    
    root = Broyden(system, rhsVars, x=center+.001)#often does not work for (0,0)
    roots = rbind(roots, root)
    colnames(roots) = rhsVars
    return(roots)
  }
}

#sort the list of zeros by x, y, or radially.
.sort <- function(data, center=c(0,0), type = 'byx'){
  #introduce some sort of infer args business...
  
  if(type == 'byx'){
    return(data[order(data[1]),])
  }
  
  if(type == 'byy'){
    return(data[order(data[2]),])
  }
  
  if(type == 'radial'){
    npts = length(rows(data))
    ref = center
    data$angle <- apply(data, 1, function(row){angle <- atan((ref[2]-row[2])/(ref[1]-row[1]))
                                               if(sign(ref[1]-row[1])==-1) angle = angle+pi
                                               if(sign(angle)==-1) angle = 2*pi+angle
                                               return(angle)})
    data <- data[order(data$angle),]
    
    #     for(i in (1:(npts-2))){
    #      if(sqrt((data[i,1]-data[i+1,1])^2+(data[i,2]-data[i+1,2])^2) >= 
    #        sqrt((data[i,1]-data[i+2,1])^2+(data[i,2]-data[i+2,2])^2)){
    #        browser()
    #        ref.x = mean(data[i,1],data[i+1,1],data[i+2,1])
    #        ref.y = mean(data[i,2],data[i+1,2],data[i+2,2])
    #        ref = c(ref.x,ref.y)
    #         
    #         #find new angles with new reference point
    #        data$angle <- apply(data, 1, function(row){angle <- atan((ref[2]-row[2])/(ref[1]-row[1]))
    #                                                   if(sign(ref[1]-row[1])==-1) angle = angle+pi
    #                                                   if(sign(angle)==-1) angle = 2*pi+angle
    #                                                   return(angle)})
    #         
    #        data[i:npts,]=data[i-1+order(data$angle[i:npts]),]
    #      }
    #     }
    
    data$angle = NULL
    return(data)
  }
  stop("Incorrect entry for type")
}

#returns npts number of points sorted into bins.  the ith bin can be paired with the
#ith to last bin and used to find the equation of a line with a zero on it.
#
.findPoints<- function(system, freeVars, vars, rad, center, npts = 100){
  numBins = 2^(length(system))
  points = list()
  for(i in (1:numBins)) points[[i]] = data.frame()
  set.seed(1) #set seed for random number generation
  for(p in (1:100)){
    pt =runif(length(vars), min=center[1]-rad, max=center[1]+rad)
    binNum = 1
    for(i in (1:(length(system)))){
      if(!sign(do.call(system[[i]], as.list(c(pt, freeVars))) )==1) #Positive
        binNum = binNum + 2^(i-1)
    }
    points[[binNum]] = rbind(points[[binNum]], pt)
  }
  #   if(length(points[[1]])==0||length(points[[2]])==0){
  #     warning("No zeros found. Try choosing a different start value or widening your search.")
  #     return(numeric(0))
  #   }
  
  for(i in (1:length(points))) colnames(points[[i]]) = vars
  return(points)
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
  if(is.null(x)) x = rep(1,length(system))#Add in something that makes sure this is valid.
  if(toString(names(x))=="") names(x) = vars
  
  A = diag(n) #Default derivative is the identity matrix
  
  #Evaluates a system of equations at a given point. FIX.
  .evalSys <- function(.x.,System){
    n=length(System)
    FF = rep(0,n)
    for( i in (1:n)){
      if(length(formals(System[[i]]))!= length(.x.))
        FF[i]=do.call(System[[i]], as.list(.x.[-which(names(.x.)==
          setdiff(names(.x.), names(formals(System[[i]]))))]))
      else FF[i] = do.call(System[[i]], as.list(.x.))
    }
    return(FF)
  }
  
  FF=.evalSys(x,system)
  
  for(iter in (1:maxiters)){
    if(max(abs(FF))<tol) 
      return(x)
    xnew=as.vector(x-A%*%FF)
    names(xnew)=names(x)
    del = xnew-x
    FFnew = .evalSys(xnew,system)
    Del = FFnew-FF
    Anew = A+((del-A%*%Del)%*%t(del)%*%A)/(t(del)%*%A%*%Del)[[1]]
    x=xnew
    A=Anew
    FF=FFnew
  }

}

check_root = function(system, roots){
  browser()
  for(i in length(rows(roots))){
    for(func in system){
      colnames(roots) = names(formals(func))
      print(do.call(func, as.list(roots[i,])))
    }
  }
}

