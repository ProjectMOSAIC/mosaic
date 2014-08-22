#'Takes a call and returns its polynomial coefficients
#'
#'@rdname simplify
#'
#'@param tree A call that will be parsed and simplified recursively
#'@param .x. the variable name with respect to which the polynomial should be most simplified
#'@param params All names of free variables.  If there are no free variables, the value should be ""
#'@param iterate The number of times the call is nested.  Default and proper value when called from the outside is 1
#'
#'@details Will work on any call as long as it can be reduced to a polynomial with respect the the variable
#'and each of the parameters.  Operates recursively, reducing each of the coefficients with respect to the extra parameters
#'in turn.  Calls .polyExp.num when all remaining coefficients are numeric to reduce the expression more fully.
#'
#'@return A list containing a list, \code{coeffs}, of coefficients ordered high to low (i.e. the list (2,3,4) would correspond to
#'the polynomial 2*x^2+3*x+4 ) and value, \code{pow}, indicating the order of the polynomial.
#'If the expression is not a polynomial, this method returns an empty list or an error.
#'
.polyExp <- function(tree, .x., params, iterate=1){
  # Reduce purely numerical coefficients to a number
  # Aaron Mayerson May 29, 2013
  .eval_coeffs <- function(coeffs){
    for (i in 1:length(coeffs)){
      val <- try(eval(coeffs[[i]]),silent=TRUE)
      if( class(val)=="numeric") coeffs[[i]] <- val
    }
    return(coeffs)
  }
  
  #A function the calls .polyExp() on each of the resultant coefficients in turn to further simplify them with
  #respect to the additional parameters.
  .reduce_coeffs <- function(coeffs, params){
    for(i in 1:length(coeffs)){
      new.x. <- params[1]
      if(length(params)==1)
        newparams <- ""
      else
        newparams <- params[-1]
      newco <- .polyExp(coeffs[[i]], new.x., newparams)
      if(newco$pow>=2){
        expvec <- c(rep("^", newco$pow-1), "", "")
        powvec <- c((newco$pow):2, "", "")
      }
      else{
        expvec <- rep("", newco$pow+1)
        powvec <- rep("", newco$pow+1)
      }
      multvec <- c(rep("*", newco$pow), "")
      varvec <-  c(rep(new.x., newco$pow), "")
      #simplify expression
      index <- 1
      for(j in 1:length(newco$coeffs)){
        if(index>length(newco$coeffs)) break
        if(newco$coeffs[[index]]==0){
          newco$coeffs <- newco$coeffs[-index]
          multvec <- multvec[-index]
          varvec <- varvec[-index]
          expvec <- expvec[-index]
          powvec <- powvec[-index]
        }
        else{
          if(newco$coeffs[[index]]==1 && index!= length(newco$coeffs)){
            
            newco$coeffs[index] <- ""
            multvec[index] <- ""
          }
          else{
            if(!(class(newco$coeffs[[index]])=='name')&&!(class(newco$coeffs[[index]])=='numeric')){
              if(newco$coeffs[[index]][[1]]=='+'||newco$coeffs[[index]][[1]]=='-')
                newco$coeffs[[index]] <- paste("(", deparse(newco$coeffs[[index]]), ")", sep="")
            }
          }
          index <- index + 1
        }
        
      }
      if(length(newco$coeffs)==0)
        coeffs[[i]] <- 0
      
      else
        coeffs[[i]] <- parse(text = paste(newco$coeffs, multvec , varvec,
                                          expvec, powvec, collapse="+", sep=""))[[1]]      
    }
    return(coeffs)
  }
  
  #If there are no additional parameters, we can assume the coefficients will be numeric and call a faster,
  #more simplified version
  if(all(params =="") )
    return(.polyExp.num(tree, .x.))
  
  #if it is a simple expression
  if(tree==.x.){
    coeffs <- list(1, 0)    
    return(list(coeffs= coeffs, pow=1))
  }
  
  #if it is a constant
  if(class(tree)=='numeric'||class(tree)=='name'){
    coeffs <- list(tree)
    
    if(iterate==1){
      coeffs <- suppressWarnings(.reduce_coeffs(coeffs, params))
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
        coeffs <- suppressWarnings(.reduce_coeffs(coeffs, params))
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
        coeffs <- suppressWarnings(.reduce_coeffs(coeffs, params))
      }
      
      return(list(coeffs = coeffs, pow = pow))
    }
  }
  
  if(tree[[1]] == '-'){
    if(length(tree)==2){
      inside <- Recall(tree[[2]], .x., params, iterate = iterate+1)
      coeffs <- inside$coeffs
      pow = inside$pow
      for(i in 1:length(coeffs)){
        if(coeffs[[i]] == 0)
          coeffs[[i]] <- coeffs[[i]]
        else
          coeffs[[i]] <- parse(text = paste("-", deparse(coeffs[[i]]), sep=""))[[1]]
      }
      
      # reduce expressions that result in numerical coefficients to numbers, added
      coeffs <- .eval_coeffs(coeffs)
      
      if(iterate==1){
        coeffs <- suppressWarnings(.reduce_coeffs(coeffs, params))
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
              coeffs[[i]] <- lcoeffs[[i]]
            else
              coeffs[[i]] <- parse(text = paste(deparse(lcoeffs[[i]], width.cutoff=500), 
                                                "-", deparse(coeffs[[i]], width.cutoff=500), sep = ""))[[1]]
          }
          
        }
        # reduce expressions that result in numerical coefficients to numbers
        coeffs <- .eval_coeffs(coeffs)
        
        if(iterate==1){
          coeffs <- suppressWarnings(.reduce_coeffs(coeffs, params))
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
          coeffs <- suppressWarnings(.reduce_coeffs(coeffs, params))
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
      coeffs <- suppressWarnings(.reduce_coeffs(coeffs, params))
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
  
  stop("Is not a polynomial")
  return(list())
}


#'Takes a call and returns its polynomial coefficients as numerics.
#'
#'@rdname simplify
#'
#'@details works with the same structure as .polyExp() but will return only if all coefficients reduce to numeric values.
#'
#'@return A list containing a list, \code{coeffs}, of coefficients ordered high to low (i.e. the list (2,3,4) would correspond to
#'the polynomial 2*x^2+3*x+4 ) and value, \code{pow}, indicating the order of the polynomial.
#'If the expression is not a polynomial, this method returns an empty list or an error.

.polyExp.num <- function(tree, .x.){
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
  stop("Is not a polynomial")
  return(list())
}

#' Method for putting a polynomial together given the coefficients and power from .polyExp()
#' 
#' @rdname simplify
#' 
#' @param poly output of .polyExp()
#' @param form original formula - provides information on which variable the polynomial was reduced with respect to.
#' 
#' @return A formula whose left hand side is a polynomial that fits the description given with the input poly.
.makePoly <- function(form, poly){
  
  if(poly$pow>=2){
    expvec <- c(rep("^", poly$pow-1), "", "")
    powvec <- c((poly$pow):2, "", "")
  }
  else{
    expvec <- rep("", poly$pow+1)
    powvec <- rep("", poly$pow+1)
  }
  multvec <- c(rep("*", poly$pow), "")
  varvec <-  c(rep(all.vars(rhs(form)), poly$pow), "")
  #simplify exppolysion
  index <- 1
  for(j in 1:length(poly$coeffs)){
    if(index>length(poly$coeffs)) break
    if(poly$coeffs[[index]]==0){
      poly$coeffs <- poly$coeffs[-index]
      multvec <- multvec[-index]
      varvec <- varvec[-index]
      expvec <- expvec[-index]
      powvec <- powvec[-index]
    }
    else{
      if(poly$coeffs[[index]]==1 && index!= length(poly$coeffs)){
        
        poly$coeffs[index] <- ""
        multvec[index] <- ""
      }
      else{
        if(!(class(poly$coeffs[[index]])=='name')&&!(class(poly$coeffs[[index]])=='numeric')){
          if(poly$coeffs[[index]][[1]]=='+'||poly$coeffs[[index]][[1]]=='-')
            poly$coeffs[[index]] <- paste("(", deparse(poly$coeffs[[index]]), ")", sep="")
        }
      }
      index <- index + 1
    }
  }
  form[[2]] <- if(length(poly$coeffs)==0) 0
  else
    parse(text = paste(poly$coeffs, multvec , varvec,
                                    expvec, powvec, collapse="+", sep=""))[[1]]
  return(form)
}
