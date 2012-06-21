#'Find the zeros of a function of two variables
#'
#'Compute numerically the zeros of a function of two variables.
#'All free variables (all but the variable on the right side) named in the expression must be assigned 
#' a value via \code{\ldots}
#'
#'@param ... arguments for values 
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
findZerosMult <- function(..., npts=10, rad = 5, center=c(0,0), sortBy='byx'){
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
  
  #make sure all equations in the system are functions.
  for(i in (1:numEq)) system[[i]] = try(makeFun(system[[i]]),silent=TRUE) #Fix - sloppy
  
  #if there is only one equation, call .oneEq
  if(length(system)==1){ 
    points = .findPoints(system, freeVars, rhsVars, rad, center, npts)
    for(i in (1:length(system))){
      for(j in (1:length(rows(points[[i]])))){
        pt1= points[[i]][j,]
        for(k in (1:length(rows(points[[length(points)-i+1]])))){
          pt2 = points[[length(points)-i+1]][k,]
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
    }
    colnames(roots) = rhsVars
    return(roots[order(roots[1]),])
  }
  
  if(length(system)>1) stop("Currently only works for 2 dims.")
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
  for(i in (1:100)){
    point1 =runif(length(vars), min=center[1]-rad, max=center[1]+rad)
    
    if(sign(do.call(system[[1]], as.list(c(point1, freeVars))) )==1){#change for more than 2 binds
      points[[1]] = rbind(points[[1]], point1)
    }
    else{
      points[[2]] = rbind(points[[2]], point1)
    }
  }
  
  if(length(points[[1]])==0||length(points[[2]])==0){
    warning("No zeros found. Try choosing a different start value or widening your search.")
    return(numeric(0))
  }
  for(i in (1:length(points))) colnames(points[[i]]) = vars
  return(points)
}