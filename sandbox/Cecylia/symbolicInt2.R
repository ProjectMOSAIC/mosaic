#First there are a list of patterns
#
#Each pattern is in the form of a list, specifying a desired parse tree
#elements of the parse tree can be:
#   Math expressions
#   Arithmetic expressions
#   Affine expressions
#   Square of an affine expression
#   Quadratic expression
#   
#
#Examples
#The generic formula for something will integrate into arctan is:
# A/(B+C*(D*x+E)^2) and this will integrate into
# A*(1/D)*(1/sqrt(C))*(1/sqrt(B))*arctan(D*sqrt(C)*x/sqrt(B))
# ^^ double check ^^

pattern1 <- list(One='cos', Two="affine")


#for(ptrn in patternList){
  
#}


#'Takes a call and returns its polynomial coefficients
#'
#'@param tree A call that will be parse
#'@param .x. the variable name
#'
#'@return A list with values of a, b, c, ... satisfying a*.x.^2+b*.x.+c  = tree. The last value in the list, pow, indicates the highest power
#'of the expression.
#'If the expression is not a polynomial, returns an empty list or an error.
#'
.polynomial.expression <- function(tree, .x.){
  
  
  #if it is a simple expression
  if(tree==.x.){
    coeffs <- c(a=1, b=0)
    return(list(coeffs= coeffs, pow=1))
  }
  
  #if it is a constant
  if(class(tree)=='numeric'||class(tree)=='name'){
    coeffs <- c(a=tree)
    return(list(coeffs = coeffs, pow=0))
  }

  if(tree[[1]]=='('){
    return(Recall(tree[[2]], .x.))
  }
  
  if(tree[[1]] == '+'){
    lside <- Recall(tree[[2]], .x.)
    rside <- Recall(tree[[3]], .x.)
    
    if(rside$pow >= lside$pow){
      pow = rside$pow
      coeffs <- rside$coeffs
      lcoeffs <- append(rep(0, pow-lside$pow), lside$coeffs)
      browser()
      coeffs <- coeffs + lcoeffs
      
      return(list(coeffs = coeffs, pow = pow))
    }
    
    else{
      pow = lside$pow
      coeffs <- lside$coeffs
      rcoeffs <- append(rep(0, pow-rside$pow), rside$coeffs)
      coeffs <- coeffs + rcoeffs
      
      return(list(coeffs = coeffs, pow = pow))
    }
  }
  
  if(tree[[1]] == '-'){
    lside <- Recall(tree[[2]], .x.)
    rside <- Recall(tree[[3]], .x.)
    
    if(rside$pow >= lside$pow){
      pow = rside$pow
      coeffs <- rside$coeffs
      lcoeffs <- append(rep(0, pow-lside$pow), lside$coeffs)
      names <- names(coeffs)
      coeffs <- lcoeffs - coeffs
      names(coeffs) <- names
      
      return(list(coeffs = coeffs, pow = pow))
    }
    
    else{
      pow = lside$pow
      coeffs <- lside$coeffs
      rcoeffs <- append(rep(0, pow-rside$pow), rside$coeffs)
      coeffs <- coeffs - rcoeffs
      
      return(list(coeffs = coeffs, pow = pow))
    }
  }
  
  if(tree[[1]] == '*'){
    
    lside <- Recall(tree[[2]], .x.)
    rside <- Recall(tree[[3]], .x.)
    
    pow <- lside$pow + rside$pow
    diff <- abs(lside$pow - rside$pow)
    dim = max(lside$pow, rside$pow)+1
    
    cmatrix <- outer(lside$coeffs, rside$coeffs)
    #pad matrix to make it square
    if(nrow(cmatrix) > ncol(cmatrix)){
      for(i in (1:diff))
        cmatrix <- cbind(cmatrix, 0)
    }
    if(ncol(cmatrix) > nrow(cmatrix)){
      for(i in (1:diff))
        cmatrix <- rbind(cmatrix, 0)
    }
    
    coeffs <- rep(0, pow+1)
    for(i in (1:dim)){
      if(i != 0){
        coeffs[i] <- sum(cmatrix[cbind((i:1), (1:i))])
      }
    }
    for(i in dim:(pow+1)){
      if(i != 0){
        coeffs[i] <- sum(cmatrix[cbind((dim:(1+i-dim)), ((1+i-dim):dim))])
      }
    }
    
    names(coeffs) <- letters[(1:length(coeffs))]
    
    return(list(coeffs = coeffs, pow = pow))
  }
  
  if(tree[[1]] == '^'){    
    #Recursively call as multiplication
    newTree <- tree
    
    tree[[3]] <- tree[[3]] - 1
    if(tree[[3]] == 1){
      tree <- tree[[2]]
    }
    
    newTree <- parse(text= paste(deparse(newTree[[2]]), "*", deparse(tree), sep=""))[[1]]
    return(Recall(newTree, .x.))
    
  }
  
  return(list())
}