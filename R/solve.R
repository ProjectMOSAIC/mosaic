#'Solve an expression
#'
#'@param form Expression to be solved
#'
#'@param \dots Specific numerical values for the free variables in the expression.
#'@param near a value near which zeros are desired
#'@param within only look for zeros at least this close to near.  \code{near} and \code{within} provide an
#'alternative to using \code{xlim} to specify the search space.
#'@param nearest the number of nearest zeros to return.  Fewer are returned if fewer are found.
#'@param iterate maximum number of times to iterate the search. Subsequent searches take place with the range
#'      of previously found zeros.  Choosing a large number here is likely to kill performance without 
#'      improving results, but a value of 1 (the default) or 2 works well when searching in \code{c(-Inf,Inf)} for
#'      a modest number of zeros near \code{near}.
#'@param npts How many sub-intervals to divide the \code{xlim} into when looking for candidates for zeros.  
#'The default is usually good enough.
#' @param sortBy specifies how the zeros found will be sorted. Options are 'byx', 'byy', or 'radial'.
#'If \code{Inf} is involved, the intervals are logarithmically spaced up to the largest finite floating point number.  
#'There is no guarantee that all the roots will be found.
#'
#'
#'@details Uses findZerosMult of findZeros to solve the given expression
#'
#'@return a dataframe with solutions to the expression.
#'
#'@author Cecylia Bocovich
#'
#'@examples
#'solve(3*x==3~x)
#'
#'#plot out sphere
#'sphere = solve(x^2+y^2+z^2==5~x&y&z, within=5, nearest=1000)
#'cloud(z~x+y, data=sphere)
#'
#'
solve <- function(form, ..., near=0, 
                  within=Inf, nearest=10, npts=1000, iterate=1, sortBy=c('byx', 'byy', 'radial')){
  dots = list(...)
  system = list(form)
  sortBy <- match.arg(sortBy)
  freeVars = list()
  
  
  #Separate formulae and free vars
  if(length(dots)>0){
    for(i in (1:length(dots))){
      if(class(dots[[i]])=="formula")
        system = append(system, dots[[i]]) #system contains all equations
      else{
        if(class(dots[[i]])=="numeric")
          freeVars[[names(dots)[i]]] <- dots[[i]] #freeVars contains values we will sub in
        else stop(paste("Improper value: ", deparse(dots[[i]])))
      }
    }
  }
  
  #change expression into formula.
  for(i in (1:length(system))){
    formula = system[[i]]
    exp = lhs(formula)
    exp = parse(text=paste(deparse(exp[[2]], width.cutoff=500), "-",
                           deparse(exp[[3]], width.cutoff=500), sep=""))[[1]]
    formula[[2]] <- exp
    system[[i]] <- formula
    }
  
  return(do.call(findZeros, c(system, freeVars, near=near, 
                       within=within, nearest=nearest, npts=npts, iterate=iterate, sortBy=sortBy)))
}