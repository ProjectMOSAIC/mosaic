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
#'@param params All names of free variables.  If there are no free variables, the value should be "".
#'@param iterate How many times the call is nested
#'
#'@return A list with values of a, b, c, ... satisfying a*.x.^2+b*.x.+c  = tree. The last value in the list, pow, indicates the highest power
#'of the expression.
#'If the expression is not a polynomial, returns an empty list or an error.
#'
.polynomial.expression <- function(tree, .x., params, iterate=1){
  
  .reduce_coeffs <- function(coeffs, params){
    for(i in length(coeffs)){
      new.x. <- params[1]
      if(length(params)==1)
        params <- ""
      else
        params <- params[-1]
      newco <- .polynomial.expression(coeffs[[i]], new.x., params)
      if(newco$pow>=2){
        expvec <- c(rep("^", newco$pow-1), "", "")
        powvec <- c((newco$pow):2, "", "")
      }
      else{
        expvec <- rep("", newco$pow+1)
        powvec <- rep("", newco$pow+1)
      }
      
      coeffs[[i]] <- parse(text = paste(newco$coeffs, c(rep("*", newco$pow), "") , c(rep(new.x., newco$pow), ""),
                                        expvec, powvec, collapse="+", sep=""))[[1]]
    }
    return(coeffs)
  }
  
  if(params =="")
    return(.poly.exp.num(tree, .x.))
  
  #if it is a simple expression
  if(tree==.x.){
    coeffs <- list(1, 0)    
    return(list(coeffs= coeffs, pow=1))
  }
  
  #if it is a constant
  if(class(tree)=='numeric'||class(tree)=='name'){
    coeffs <- list(tree)
    
    if(iterate==1){
      coeffs <- .reduce_coeffs(coeffs, params)
    }
    return(list(coeffs = coeffs, pow=0))
  }

  if(tree[[1]]=='('){
    return(Recall(tree[[2]], .x., params, iterate=iterate+1))
  }
  
  if(tree[[1]] == '+'){
    lside <- Recall(tree[[2]], .x., params, iterate = iterate+1)
    rside <- Recall(tree[[3]], .x., params, iterate = iterate+1)
    
    if(rside$pow >= lside$pow){
      pow = rside$pow
      coeffs <- rside$coeffs
      lcoeffs <- append(rep(0, pow-lside$pow), lside$coeffs)
      for(i in 1:length(coeffs)){
        if(coeffs[[i]]==0)
          coeffs[[i]] <- lcoeffs[[i]]
        else {
          if(lcoeffs[[i]]==0)
            coeffs[[i]] <- coeffs[[i]]
          else
            coeffs[[i]] <- parse(text = paste(deparse(coeffs[[i]], width.cutoff=500), 
                                          "+", deparse(lcoeffs[[i]], width.cutoff=500), sep=""))[[1]]
        }
      }
      
      if(iterate==1){
        coeffs <- .reduce_coeffs(coeffs, params)
      }
      
      return(list(coeffs = coeffs, pow = pow))
    }
    
    else{
      pow = lside$pow
      coeffs <- lside$coeffs
      rcoeffs <- append(rep(0, pow-rside$pow), rside$coeffs)
      
      for(i in 1:length(coeffs)){
        if(coeffs[[i]]==0)
          coeffs[[i]] <- rcoeffs[[i]]
        else{
          if(rcoeffs[[i]] == 0)
            coeffs[[i]] <- coeffs[[i]]
          else
            coeffs[[i]] <- parse(text = paste(deparse(coeffs[[i]], width.cutoff=500),  
                                          "+", deparse(rcoeffs[[i]], width.cutoff=500), sep=""))[[1]]
        }
      }
      
      if(iterate==1){
        coeffs <- .reduce_coeffs(coeffs, params)
      }
      
      return(list(coeffs = coeffs, pow = pow))
    }
  }
  
  if(tree[[1]] == '-'){
    if(length(tree)==2){
      inside <- Recall(tree[[2]], .x., params, iterate = iterate+1)
      coeffs <- inside$coeffs
      for(i in 1:length(coeffs)){
        if(coeffs[[i]] == 0)
          coeffs[[i]] <- coeffs[[i]]
        else
          coeffs[[i]] <- parse(text = paste("-", deparse(coeffs[[i]]), sep=""))[[1]]
      }
      
      if(iterate==1){
        coeffs <- .reduce_coeffs(coeffs, params)
      }
      
      return(list(coeffs = coeffs, pow=pow))
    }
    
    else{
    lside <- Recall(tree[[2]], .x., params, iterate=iterate+1)
    rside <- Recall(tree[[3]], .x., params, iterate=iterate+1)
    if(rside$pow >= lside$pow){
      pow = rside$pow
      coeffs <- rside$coeffs
      lcoeffs <- append(rep(0, pow-lside$pow), lside$coeffs)
      
      for(i in 1:length(coeffs)){
        if(lcoeffs[[i]]==0)
          coeffs[[i]] <- parse(text = paste("-", deparse(coeffs[[i]], width.cutoff=500), sep = ""))[[1]]
        else{
          if(coeffs[[i]] == 0)
            coeffs[[i]] <- coeffs[[i]]
          else
            coeffs[[i]] <- parse(text = paste(deparse(lcoeffs[[i]], width.cutoff=500), 
                                              "-", deparse(coeffs[[i]], width.cutoff=500), sep = ""))[[1]]
        }
        
      }
      
      if(iterate==1){
        coeffs <- .reduce_coeffs(coeffs, params)
      }
      
      return(list(coeffs = coeffs, pow = pow))
    }
    
    else{
      pow = lside$pow
      coeffs <- lside$coeffs
      rcoeffs <- append(rep(0, pow-rside$pow), rside$coeffs)
      
      for(i in 1:length(coeffs)){
        if(coeffs[[i]] == 0)
          coeffs[[i]] <- parse(text = paste("-", deparse(rcoeffs[[i]], width.cutoff=500), sep = ""))[[1]]
        else{
          if(rcoeffs[[i]] == 0)
            coeffs[[i]] <- coeffs[[i]]
          else
            coeffs[[i]] <- parse(text = paste(deparse(coeffs[[i]], width.cutoff=500), 
                                              "-", deparse(rcoeffs[[i]], width.cutoff=500), sep = ""))[[1]]
        }
      }
      
      if(iterate==1){
        coeffs <- .reduce_coeffs(coeffs, params)
      }
      
      return(list(coeffs = coeffs, pow = pow))
    }
  }
  }
  
  if(tree[[1]] == '*'){
    
    
    lside <- Recall(tree[[2]], .x., params, iterate=iterate+1)
    rside <- Recall(tree[[3]], .x., params, iterate=iterate+1)
    
    pow <- lside$pow + rside$pow
    diff <- abs(lside$pow - rside$pow)
    dim = max(lside$pow, rside$pow)+1
    
    cmatrix <- list()
    for(i in 1:length(lside$coeffs)){
      cmatrix[[i]]<- list()
      for(j in 1:length(rside$coeffs)){
        if(lside$coeffs[[i]]==0 || rside$coeffs[[j]]==0)
          cmatrix[[i]][[j]] <- 0
        else{
          if(lside$coeffs[[i]]==1)
            cmatrix[[i]][[j]] <- rside$coeffs[[j]]
          else{
            if(rside$coeffs[[j]]==1)
              cmatrix[[i]][[j]] <- lside$coeffs[[i]]
            else
              cmatrix[[i]][[j]] <- parse(text = paste("(", deparse(lside$coeffs[[i]], width.cutoff=500), 
                                                      ")*(", deparse(rside$coeffs[[j]], width.cutoff=500), ")", sep=""))[[1]]
          }
        }
      }
    }
    coeffs <- list()
    index=1
    for(i in 1:length(lside$coeffs)){
      for(j in 1:i){
        if(j <= length(rside$coeffs)){
          if(j==1)
            coeffs[[index]] <- cmatrix[[i-j+1]][[j]]
          else{
            if(coeffs[[index]]==0)
              coeffs[[index]] <- cmatrix[[i-j+1]][[j]]
            else{
              if(cmatrix[[i-j+1]][[j]] ==0)
                coeffs[[index]] <- coeffs[[index]]
              else
                coeffs[[index]] <- parse(text=paste(deparse(coeffs[[index]], width.cutoff=500), "+",
                                                    deparse(cmatrix[[i-j+1]][[j]], width.cutoff=500), sep=""))[[1]]
            }
          }
        }
      }
      index = index+1
    }
    for(j in 2:length(rside$coeffs)){
      if(length(rside$coeffs)==1)
        break
      for(i in 1:(length(rside$coeffs)-j+1)){
        if((length(cmatrix)-i )>= 0 && (i+j-1)<= length(rside$coeffs)){
          if(i==1)
            coeffs[[index]] <- cmatrix[[length(cmatrix)-i+1]][[i+j-1]]
          else{
            if(coeffs[[index]] == 0)
              coeffs[[index]] <- cmatrix[[length(cmatrix)-i+1]][[i+j-1]]
            else{
              if(cmatrix[[length(cmatrix)-i+1]][[i+j-1]]==0)
                coeffs[[index]] <- coeffs[[index]]
              else
                coeffs[[index]] <- parse(text = paste(deparse(coeffs[[index]], width.cutoff=500),
                                                      "+", deparse(cmatrix[[length(cmatrix)-i+1]][[i+j-1]], width.cutoff=500), sep=""))[[1]]
            }
          }
        }
      }
      index = index+1
    }
    
    if(iterate==1){
      coeffs <- .reduce_coeffs(coeffs, params)
    }

    return(list(coeffs = coeffs, pow = pow))
  }
  
  if(tree[[1]] == '^'){   
    #Recursively call as multiplication
    newTree <- tree
    tree[[3]] <- eval(tree[[3]]) - 1
    
    if(eval(tree[[3]]) <0) stop("Can only handle positive exponents")
    
    if(tree[[3]] == 1){
      tree <- tree[[2]]
    }
    
    newTree <- parse(text= paste(deparse(newTree[[2]], width.cutoff=500),
                                 "*", deparse(tree, width.cutoff=500), sep=""))[[1]]
    
    return(Recall(newTree, .x., params, iterate = iterate+1))
    
  }
  
  return(list())
}


#------------------------------------------------------------
#Numerical evaluation only
.poly.exp.num <- function(tree, .x.){
  
  
  #if it is a simple expression
  if(tree==.x.){
    coeffs <- c(1, 0)
    return(list(coeffs= coeffs, pow=1))
  }
  
  #if it is a constant
  if(class(tree)=='numeric'||class(tree)=='name'){
    coeffs <- c(tree)
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
      for(i in 1:length(coeffs))
        coeffs <- tryCatch({coeffs[i] <- coeffs[i] + lcoeffs[i]
                            coeffs}, 
                           error = function(e){
                             coeffs <- as.list(coeffs)
                             coeffs[i] <- parse(text = paste(deparse(coeffs[[i]]), "+", deparse(lcoeffs[[i]]), sep=""))
                             coeffs}
        )
      
      
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
    if(length(tree)==2){
      inside = Recall(tree[[2]], .x.)
      coeffs = -inside$coeffs
      pow=inside$pow
      
      return(list(coeffs = coeffs, pow=pow))
    }
    
    else{
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