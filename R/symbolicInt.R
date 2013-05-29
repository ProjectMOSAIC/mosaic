#'Find the symbolic integral of a formula
#'
#' @param form an object of type formula to be integrated.
#' Rhs of formula indicates which variable to
#' integrate with respect to.  Must only have one variable.
#' @param \ldots extra parameters
#'
#' @details This symbolic integrator recognizes simple polynomials and functions such as
#'\code{sin}, \code{cos}, \code{tan}, \code{sinh}, \code{cosh}, \code{tanh}, \code{sqrt}, and \code{exp}.
#'
#'It will not perform more complicated substitutions
#'or integration by parts.
#'
#' @return symbolicInt returns a function whose body is the symbolic antiderivative of
#'the formula.  If this method does not recognize the formula, it will return an error.
#'
symbolicInt<- function(form, ...){
  dots = list(...)
  #First check if it's a polynomial.  If it is, simplify it.
  params <- setdiff(all.vars(form), all.vars(rhs(form)))
  if(length(params)==0)
    params <- ""
#   res <- try(.polyExp(lhs(form), all.vars(rhs(form)), params), silent=TRUE)
#   if(!inherits(res, "try-error")){
#     form <- .makePoly(form, res)
#   }
  antiDeriv <- symbolicAntiD(form, ...)
  
  #determine which letter the constant will be
  intc = LETTERS[!LETTERS[-(1:2)]%in%all.vars(form)][-(1:2)][1]
  
  #add the constant into the expression
  antiDeriv[[2]] <- parse(text = paste(deparse(lhs(antiDeriv), width.cutoff=500), "+", intc, sep=""))[[1]]
  
  #make the integral formula into a function
  intfun = eval(parse(text=paste("do.call(makeFun, list(antiDeriv, ..., ",intc, "=0))", sep=""))[[1]])
  return(intfun)
}

#'Use recursion to find a symbolic antiderivative
#'
#' @rdname symbolicInt
#'
#' @return a formula implementing giving symbolic anti-derivative.  If the formula
#' isn't found by the algorithm, an error is thrown.  
#' 
symbolicAntiD <- function(form, ...){
  rhsVar = all.vars(rhs(form))
  if(length(rhsVar)!=1) stop("Can only integrate with respect to one variable.")
  constants = setdiff(all.vars(form), rhsVar)
  
  #check if it's just constants
  if(length(grep(rhsVar, deparse(lhs(form), width.cutoff=500)))==0){
    form[[2]]<- parse(text = paste(deparse(lhs(form), width.cutoff=500), "*", rhsVar, sep=""))[[1]]
    return(form)
  }
  
  #check to see if surrounded by parentheses
  if(class(lhs(form))=="("){
    form[[2]]=lhs(form)[[2]]
    return(symbolicAntiD(form, ...))
  }
  
  #Check to see if it is nested
  if(class(lhs(form))=="call"&&is.primitive(eval(lhs(form)[[1]])))
    group = getGroup(toString(lhs(form)[[1]]))[[1]] #determine typ of highest-level expr.
  else group = -1
  if(group =="Arith")
    return(.intArith(form, ...))
  if(group =="Math")
    return(.intMath(form, ...))
  
  #check if it's just x
  if((lhs(form))==rhsVar){
    form[[2]]<- parse(text=paste("1/(2)*", rhsVar, "^2", sep=""))[[1]]
    return(form)
  }
  
  stop("Error: symbolic algorithm gave up")
}

#'Attempts symbolic integration of some mathematical/arithmetical forms
#'
#' @rdname symbolicInt
#'
#' @return An expression with the integral, or throws an error if unsuccessful.
#'
.intArith <- function(form, ...){
  dots = list(...)
  
  rhsVar = all.vars(rhs(form))
  constants = setdiff(all.vars(form), rhsVar)
  
  op = lhs(form)[[1]]
  
  if(length(lhs(form))==2){#binary operation
    if(op=='-'){
      form[[2]] = parse(text=paste("(-1)*",deparse(lhs(form)[[2]], width.cutoff=500),sep=""))[[1]]
      return(symbolicAntiD(form,...))
    }
  }
  
  if(op =='+'||op =="-"){ 
    lform = form
    rform = form
    lform[[2]] = lhs(form)[[2]]
    rform[[2]] = lhs(form)[[3]]
    lform = symbolicAntiD(lform, ...)
    rform = symbolicAntiD(rform, ...)
    
    newform = parse(text=paste(deparse(lhs(lform), width.cutoff=500),
                               deparse(lhs(form)[[1]], width.cutoff=500),
                               deparse(lhs(rform), width.cutoff=500), sep=""))[[1]]
    form[[2]] <- newform
    return(form)
  }
  
  if(op == '*'){
    lform = form
    rform = form
    lform[[2]] = lhs(form)[[2]]
    rform[[2]] = lhs(form)[[3]]
    if(length(grep(rhsVar, deparse(lform[[2]], width.cutoff=500)))>0 &&
      length(grep(rhsVar, deparse(rform[[2]], width.cutoff=500)))>0)#too complex
      stop("Error: symbolic algorithm gave up")
    if(regexpr(rhsVar, deparse(lform[[2]], width.cutoff=500))==1){
      lform = symbolicAntiD(lform, ...)
      
      newform = parse(text=paste(deparse(lhs(lform), width.cutoff=500),
                                 deparse(lhs(form)[[1]], width.cutoff=500),
                                 deparse(lhs(rform), width.cutoff=500), sep=""))[[1]]
      form[[2]] <- newform
      return(form)
    }
    else{
      rform = symbolicAntiD(rform, ...)
      
      newform = parse(text=paste(deparse(lhs(lform), width.cutoff=500),
                                 deparse(lhs(form)[[1]], width.cutoff=500),
                                 deparse(lhs(rform), width.cutoff=500), sep=""))[[1]]
      form[[2]] <- newform
      return(form)
    }
  }
  
  if(op=='/'){#let denominator have negative exponent if there is an x.
    num = lhs(form)[[2]]
    den = lhs(form)[[3]]
    
    #first see if it is a trigonometric substitution problem
    check <- try(.intTrig(form, num, den, rhsVar), silent=TRUE)
    if(!inherits(check, "try-error"))
      return(check)
    
    
    if(length(grep(rhsVar, den))>0){
      form[[2]] = parse(text = paste(deparse(num, width.cutoff=500), "*(",
                                     deparse(den, width.cutoff=500), ")^-1",sep="" ))[[1]]
      return(symbolicAntiD(form, ...))
    }
    else{
      form[[2]] = parse(text = paste("1/(",deparse(den, width.cutoff=500),")*",
                                     deparse(num, width.cutoff=500) , sep=""))[[1]]
      return(symbolicAntiD(form,...))
    }
  }
  
  if(op == '^'){
    
    affexp = .affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0 && length(grep(rhsVar, deparse(lhs(form)[[3]], width.cutoff=500)))==0){
      exp = try(eval(form[[2]][[3]], envir=list(pi=3.1415932653589793, form=form),
                     enclos=NULL), silent=TRUE)
      if(inherits(exp, "try-error"))
        exp = parse(text = paste(deparse(lhs(form)[[3]], width.cutoff=500), "+1"))[[1]]
      else(exp = eval(exp)+1) #change from call to numeric...
      
      if(exp == 0){
        
        if(affexp$a==1)
          form[[2]] = parse(text=paste("log(", deparse(lhs(form)[[2]], width.cutoff=500),
                                       ")", sep=""))[[1]]
        else
          form[[2]] <- parse(text = paste("1/(",deparse(affexp$a, width.cutoff=500) ,")*log(",
                                          deparse(lhs(form)[[2]]), ")", sep=""))[[1]]
        return(form)
      }
      form[[2]][[3]] <- exp
      if(affexp$a==1)
        newform <- paste("1/(", deparse(exp), ")*",
                         deparse(form[[2]]), sep="")
      else
        newform <- paste("1/(",deparse(affexp$a),")*1/(", deparse(exp), ")*",
                         deparse(form[[2]]), sep="")
      form[[2]] <- parse(text=newform)[[1]]
      return(form)
    }
  }
  
  stop("Error: symbolic algorithm gave up")
}

#--------------------------
#'Attempts symbolic integration of some mathematical forms
#'
#' @rdname symbolicInt
#' 
#' @return An expression with the integral, or throws an error if unsuccessful.
#'
.intMath <- function(form, ...){
  
  op = lhs(form)[[1]]
  
  dots = list(...)
  
  rhsVar = all.vars(rhs(form))
  constants = setdiff(all.vars(form), rhsVar)
  
  if(op =="sin"){#trig expression    
    #check to see if we can integrate it
    affexp = .affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0){
      if(affexp$a==1)
        newform = paste("-cos(", deparse(lhs(form)[[2]]), ")", sep="")
      else
        newform = paste("1/(", deparse(affexp$a), ")*-cos(", deparse(lhs(form)[[2]]),
                        ")", sep="")
      form[[2]]= parse(text=newform)[[1]]
      return(form)
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  if(op == "cos"){    
    #check to see if we can integrate it
    affexp = .affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0){
      if(affexp$a==1)
        newform = paste("sin(", deparse(lhs(form)[[2]]), ")", sep="")
      else newform = paste("1/(", deparse(affexp$a), ")*sin(",
                           deparse(lhs(form)[[2]]), ")", sep="")
      form[[2]]= parse(text=newform)[[1]]
      return(form)
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  if(op == "exp"){    
    #Check to see if we can integrate it
    affexp = .affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0){
      if(affexp$a==1)
        newform = paste("exp(", deparse(lhs(form)[[2]]), ")", sep="")
      else
        newform = paste("1/(", deparse(affexp$a), ")*exp(",
                        deparse(lhs(form)[[2]]), ")", sep="")
      form[[2]]= parse(text=newform)[[1]]
      return(form)
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  if(op == "tan"){
    #check to see if we can integrate it
    affexp = .affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0){
      if(affexp$a==1)
        newform = paste("-log(abs(cos(", deparse(lhs(form)[[2]]), ")))", sep="")
      else
        newform = paste("1/(", deparse(affexp$a), ")*-log(abs(cos(",
                        deparse(lhs(form)[[2]]), ")))", sep="")
      form[[2]]= parse(text=newform)[[1]]
      return(form)
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  if(op == "sinh"){
    #check to see if we can integrate it
    affexp = .affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0){
      if(affexp$a==1)
        newform = paste("cosh(", deparse(lhs(form)[[2]]), ")", sep="")
      else
        newform = paste("1/(", deparse(affexp$a), ")*cosh(",
                        deparse(lhs(form)[[2]]), ")", sep="")
      form[[2]]= parse(text=newform)[[1]]
      return(form)
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  if(op == "cosh"){
    #check to see if we can integrate it
    affexp = .affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0){
      if(affexp$a==1)
        newform = paste("sinh(", deparse(lhs(form)[[2]]), ")", sep="")
      else
        newform = paste("1/(", deparse(affexp$a), ")*sinh(",
                        deparse(lhs(form)[[2]]), ")", sep="")
      form[[2]]= parse(text=newform)[[1]]
      return(form)
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  if(op == "sinh"){
    #check to see if we can integrate it
    affexp = .affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0){
      if(affexp$a==1)
        newform = paste("log(cosh(", deparse(lhs(form)[[2]]), "))", sep="")
      else
        newform = paste("1/(", deparse(affexp$a), ")*log(cosh(",
                        deparse(lhs(form)[[2]]), "))", sep="")
      form[[2]]= parse(text=newform)[[1]]
      return(form)
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  if(op == "sqrt"){
    #check to see if we can integrate it
    affexp = .affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0){
      if(affexp$a==1)
        newform = paste("2/3*sqrt(", deparse(lhs(form)[[2]]), ")^3", sep="")
      else
        newform = paste("1/(", deparse(affexp$a), ")*2/3*sqrt(",
                        deparse(lhs(form)[[2]]), ")^3", sep="")
      form[[2]]= parse(text=newform)[[1]]
      return(form)
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  stop("Error: symbolic algorithm gave up")
}

#-------------------------

#'Attempts symbolic integration of some mathematical forms using trigonometric substitution
#'
#' @rdname symbolicInt
#'
#' @param num numerator 
#' @param den denominator
#' @param .x. the variable name

#'
#'@return An expression with the integral, or throws an error if unsuccessful.
#'
.intTrig <- function(form, num, den, .x.){
  params <- all.vars(num)
  if(length(params) == 0)
    params <- ""
  numco <- .polyExp(num, .x., params)
  
  if(numco$pow != 0) stop("Not valid trig sub")
  
  if(den[[1]] == 'sqrt'){
    #Could be arcsin or arccos
    params <- setdiff(all.vars(den), .x.)
    if(length(params)==0)
      params <-""
    
    denco <- .polyExp(den[[2]], .x., params)
    if(denco$pow != 2) stop("Not valid trig sub")
    
    a <- denco$coeffs[[1]]
    b <- denco$coeffs[[2]]
    c <- denco$coeffs[[3]]
    
    if(sign(a)==-1){
      #arcsec?
      
    }
    #complete the square to go from form 1/ax^2+bx+c to 1/a(x-h)^2+k
    if(b==0){
      h <- 0
      k <- c
    }
    else{
      h <- parse(text = paste("-(", deparse(b), ")/(2*(", deparse(a), "))", sep=""))[[1]]
      k <- parse(text = paste(deparse(c), "-((", deparse(b), ")^2/(4*(", deparse(a), ")))", sep=""))[[1]]
    }
    if(is.numeric(c(a,b,c))){
      h <- eval(h)
      k <- eval(k)
    }
    
    #Check what the sign on a and k are.
    if(!is.numeric(a))
      asign=1
    else
      asign=sign(a)
    if(!is.numeric(k))
      ksign=1
    else
      ksign=sign(k)
    
    if(asign==-1&&ksign==1){
      #Arcsin
      if(a!=-1){
        #k <- parse(text = paste("(", deparse(k), ")/(-1*(", deparse(a), "))", sep=""))[[1]]
        num <- parse(text = paste("(", deparse(num), ")/sqrt(-1*(", deparse(a), "))", sep=""))[[1]]
      }
      k <- parse(text = paste("sqrt(", deparse(k), ")", sep=""))[[1]]
      #Now need to integrate it
      
      if(a==-1)
        expr <- parse(text = paste(deparse(num), "*asin((", .x., "-", deparse(h), ")/", deparse(k), ")", sep=""))[[1]]
      else
        expr <- parse(text = paste(deparse(num), "*asin((sqrt(-1*(", deparse(a), "))*(", .x., "-", deparse(h), "))/",
                                   deparse(k), ")", sep=""))[[1]]
      form[[2]] <- expr
      return(form)
      
    }
    
    if(asign==1&&ksign==1){
      #Arcsinh
      if(a!=1){
        #k <- parse(text = paste("(", deparse(k), ")/(-1*(", deparse(a), "))", sep=""))[[1]]
        num <- parse(text = paste("(", deparse(num), ")/sqrt(", deparse(a), ")", sep=""))[[1]]
      }
      k <- parse(text = paste("sqrt(", deparse(k), ")", sep=""))[[1]]
      #Now need to integrate it
      
      if(a==1)
        expr <- parse(text = paste(deparse(num), "*asinh((", .x., "-", deparse(h), ")/", deparse(k), ")", sep=""))[[1]]
      else
        expr <- parse(text = paste(deparse(num), "*asinh((sqrt(", deparse(a), ")*(", .x., "-", deparse(h), "))/",
                                   deparse(k), ")", sep=""))[[1]]
      form[[2]] <- expr
      return(form)
    }
    
    if(asign==1&&ksign==-1){
      #natural log problem
      #        if(a!=1){
      #          num <- parse(text = paste("(", deparse(num), ")/sqrt(", deparse(a), ")", sep=""))[[1]]
      #        }
      #       if(is.numeric(k))
      #         k <- k*-1
      #       else{
      #         if(is.call(k)&&k[[1]]=='-')
      #          k <- k[[2]]
      #         else
      #           k <- parse(text=paste("-1*(", deparse(k), ")", sep=""))[[1]]
      #       }
      #         
      #        k <- parse(text = paste("(", deparse(k), ")", sep=""))[[1]]
      # #       #Now need to integrate it
      # #       
      #        if(a==1){
      #          expr <- parse(text = paste(deparse(num), "*log(", .x., "-", deparse(h), "+
      #                sqrt((", .x., "-", deparse(h), ")^2-", deparse(k), "))", sep=""))[[1]]
      #         }
      #        else{
      #          expr <- parse(text = paste(deparse(num), "*log(", .x., "-", deparse(h), "+
      #                sqrt((", .x., "-", deparse(h), ")^2-", deparse(k), "/(", deparse(a), ")))", sep=""))[[1]]
      #          }
      #        form[[2]] <- expr
      #        return(form)
    }
    
  }
  else{
    params <- setdiff(all.vars(den), .x.)
    if(length(params)==0){
      params <- ""
    }
    denco <- .polyExp(den[[2]], .x., params)
    if(denco$pow != 2) stop("Not valid trig sub")
    
    a <- denco$coeffs[[1]]
    b <- denco$coeffs[[2]]
    c <- denco$coeffs[[3]]
    
    #complete the square to go from form 1/ax^2+bx+c to 1/a(x-h)^2+k
    if(b==0){
      h <- 0
      k <- c
    }
    else{
      h <- parse(text = paste("-(", deparse(b), ")/(2*(", deparse(a), "))", sep=""))[[1]]
      k <- parse(text = paste(deparse(c), "-((", deparse(b), ")^2/(4*(", deparse(a), ")))", sep=""))[[1]]
    }
    if(is.numeric(c(a,b,c))){
      h <- eval(h)
      k <- eval(k)
    }
    
    if(!is.numeric(a))
      asign=1
    else
      asign=sign(a)
    if(!is.numeric(k))
      ksign=1
    else
      ksign=sign(k)
    
    if(asign==ksign){
      #arctan
      if(a!=1){
        num <- parse(text = paste("(", deparse(num), ")/(", deparse(a), ")", sep=""))[[1]]
      }
      
      if(sign(a)==-1){
        num <- parse(text=paste("-1*(", deparse(num), ")", sep=""))[[1]]
        a <- parse(text = paste("-(", deparse(a), ")", sep=""))[[1]]
        k <- parse(text = paste("-(", deparse(k), ")", sep=""))[[1]]
        
      }
      
      if(a==1){
        expr <- parse(text = paste(deparse(num), "*sqrt(1/(", deparse(k), "))*atan((", .x., "-", deparse(h),
                                   ")/sqrt(", deparse(k), "))" , sep=""))[[1]]
      }
      else{
        expr <- parse(text = paste(deparse(num), "*sqrt((", deparse(a), ")/(", deparse(k), "))*atan(sqrt(", deparse(a),
                                   ")*(", .x., "-", deparse(h), ")/sqrt(", deparse(k), "))" , sep=""))[[1]]
      }
      
      form[[2]] <- expr
      return(form)
    }
    
  }  
  
  stop("Not valid trig sub")
}


#'Takes a call and returns its affine coefficients.
#'
#' @rdname symbolicInt
#'
#' @param tree the expression to be analyzed
#' @return A list with values of a and b satisfying a*.x.+b  = tree.
#' If the expression is not affine, returns an empty list.
#'
.affine.exp <- function(tree, .x.){
  #if it is a simple expression
  if(tree==.x.){
    a=1
    b=0
    return(list(a=a,b=b))
  }
  
  #if there is no variable in the expression
  if(length(grep(toString(.x.), deparse(tree), fixed=TRUE))==0){
    a=0
    b=tree
    return(list(a=a,b=b))
  }
  
  #if the expression is more complex
  if(tree[[1]]=='('){
    return(Recall(tree[[2]], .x.))
  }
  
  if(tree[[1]]=='+'){
    lexp=Recall(tree[[2]], .x.)
    rexp=Recall(tree[[3]], .x.)
    
    if(length(lexp)==0||length(rexp)==0)
      return(list())
    
    if(lexp$a==0 && length(rexp)!=0){
      a = rexp$a
      b = parse(text=paste(deparse(lexp$b), "+", deparse(rexp$b),sep=""))[[1]]
      return(list(a=a,b=b))
    }
    
    if(rexp$a==0 && length(lexp)!=0){
      a = lexp$a
      b = parse(text=paste(deparse(lexp$b), "+", deparse(rexp$b),sep=""))[[1]]
      return(list(a=a,b=b))
    }
  }
  
  if(tree[[1]]=='-'){
    lexp=Recall(tree[[2]], .x.)
    rexp=Recall(tree[[3]], .x.)
    
    if(length(lexp)==0||length(rexp)==0)
      return(list())
    
    if(lexp$a==0 && length(rexp)!=0){
      a = parse(text=paste("-1*(",deparse(lexp$a), ")",sep=""))[[1]]
      b = parse(text=paste(deparse(lexp$b), "-(", deparse(rexp$b),")",sep=""))[[1]]
      return(list(a=a,b=b))
    }
    
    if(rexp$a==0 && length(lexp)!=0){
      a = lexp$a
      b = parse(text=paste(deparse(lexp$b), "-(", deparse(rexp$b),")",sep=""))[[1]]
      return(list(a=a,b=b))
    }
  }
  
  if(tree[[1]]=='*'){
    lexp=Recall(tree[[2]], .x.)
    rexp=Recall(tree[[3]], .x.)
    
    if(length(lexp)==0||length(rexp)==0)
      return(list())
    
    if(lexp$a==0 && length(rexp)!=0){
      a = parse(text=paste("(",deparse(lexp$b), ")*(", deparse(rexp$a),")",sep=""))[[1]]
      b = parse(text=paste("(",deparse(lexp$b), ")*(", deparse(rexp$b),")",sep=""))[[1]]
      if(lexp$b==0){
        a = 0
        b = 0
      }
      if(lexp$b==1){
        a = rexp$a
        b = rexp$b
      }
      if(rexp$a ==0){
        a = 0
      }
      if(rexp$b == 0){
        b = 0
      }
      if(rexp$a == 1){
        a = lexp$b
      }
      if(rexp$b == 1){
        b = lexp$b
      }
      return(list(a=a,b=b))
    }
    
    if(rexp$a==0 && length(lexp)!=0){
      a = parse(text=paste("(",deparse(lexp$a), ")*(", deparse(rexp$b),")",sep=""))[[1]]
      b = parse(text=paste("(",deparse(lexp$b), ")*(", deparse(rexp$b),")",sep=""))[[1]]
      
      if(rexp$b==0){
        a = 0
        b = 0
      }
      if(rexp$b==1){
        a = lexp$a
        b = lexp$b
      }
      if(lexp$a==0){
        a = 0
      }
      if(lexp$b==0){
        b = 0
      }
      if(lexp$a==1){
        a = rexp$b
      }
      if(lexp$b==1){
        b = rexp$b
      }
      return(list(a=a,b=b))
    }
  }
  
  if(tree[[1]]=='/'){
    lexp=Recall(tree[[2]], .x.)
    rexp=Recall(tree[[3]], .x.)
    
    if(length(lexp)==0||length(rexp)==0)
      return(list())
    
    if(rexp$a==0 && length(lexp)!=0){
      a = parse(text=paste(deparse(lexp$a), "/(", deparse(rexp$b),")",sep=""))[[1]]
      b = parse(text=paste(deparse(lexp$b), "/(", deparse(rexp$b),")",sep=""))[[1]]
      
      if(rexp$b==1){
        a = lexp$a
        b = lexp$b
      }
      
      return(list(a = a, b = b))
    }
  }
  return(list())
}