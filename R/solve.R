#'Solve an expression
#'
#'@param form Expression to be solved
#'
#'@param \dots Extra parameters
#'
#'@details Uses findZerosMult of findZeros to solve.
#'
solve <- function(form, ...){
  dots = list(...)
  system = list(form)
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
  do.call(findZeros, c(system, freeVars))
}