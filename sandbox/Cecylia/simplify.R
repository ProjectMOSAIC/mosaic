#Takes in a call and returns a simplified call

simplify <- function(exp){
  newExp=""
  browser()
  evalExp = try(eval(exp), silent=TRUE) #First try to evaluate the expression.
  if (inherits(evalExp, "try-error")){ 
    for(i in (1:length(exp))){
      if(is.call(exp[[i]])) newExp = paste(newExp,Recall(exp[[i]]))
      else newExp = paste(newExp, deparse(exp[[i]]))
    }
  }
  else return(deparse(evalExp))
  return(newExp)
}