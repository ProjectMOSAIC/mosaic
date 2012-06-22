#Takes in a call and returns a simplified call

simplify <- function(exp){
  browser()
  evalExp = try(eval(exp), silent=TRUE) #First try to evaluate the expression.
  if (inherits(evalExp, "try-error")){ 
    for(i in (1:length(exp))){
      if(is.call(exp[[i]])) exp[[i]] = Recall(exp[[i]])
    }
  }
  else return(evalExp)
  return(exp)
}