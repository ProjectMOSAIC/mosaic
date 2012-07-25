#'Find the symbolic integral of a formula
#'
#'@param form an object of type formula to be integrated.
#'Rhs of formula indicates which variable to
#'integrate with respect to.  Must only have one variable.
#'@param \ldots extra parameters
#'
#'@details This symbolic integrator recognizes simple polynomials and functions such as
#'\code{sin}, \code{cos}, \code{tan}, \code{sinh}, \code{cosh}, \code{tanh}, \code{sqrt}, and \code{exp}.
#'
#'It will not perform more complicated substitutions
#'or integration by parts.
#'
#'@value symbolicInt returns a function whose body is the symbolic antiderivative of
#'the formula.  If this method does not recognize the formula, it will return an error.
#'
symbolicInt<- function(form, ...){
  dots = list(...)
  antiDeriv <- symbolicAntiD(form, ...)
  intc = LETTERS[!LETTERS[-(1:2)]%in%all.vars(form)][-(1:2)][1]
  newform = paste
  antiDeriv[[2]] <- parse(text = paste(deparse(lhs(antiDeriv), width.cutoff=500), "+", intc, sep=""))[[1]]
  intfun = eval(parse(text=paste("do.call(makeFun, list(antiDeriv,",intc, "=0))", sep=""))[[1]])
  return(intfun)
}

#'Use recursion to find a symbolic antiderivative
#'
#'@param form Formula for which to find antiderivative
#'
#'@param \ldots Extra parameters
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
    return(intArith(form, ...))
  if(group =="Math")
    return(intMath(form, ...))
  
  #check if it's just x
  if((lhs(form))==rhsVar){
    form[[2]]<- parse(text=paste("1/(2)*", rhsVar, "^2", sep=""))[[1]]
    return(form)
  }
  
  stop("Error: symbolic algorithm gave up")
}

#--------------------------
intArith <- function(form, ...){
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
    
    #first see if it is a trigonometric substitution
    check <- .TrigSub(den, rhsVar)
    if(!is.null(check))
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
intMath <- function(form, ...){
  
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
.TrigSub <- function(tree, .x.){
  #Note that this takes in the denominator and checks whether it might be a trig expression.
  #Want to return the value of a and x and code for which trig expr.
  if(!is.call(tree)) return(NULL)
  #Note: need to make sure both sides don't have .x. in it
  #Use .affine.exp to find out what a and x really are
  if(tree[[1]]=='+'){
    #arcTan
    if(grep(.x., deparse(tree[[2]]))==1&&length(grep(.x., deparse(tree[[2]])))==0){
      .affine.exp()
    }
    
  }
  
  if(tree[[1]]=='-'){
    
    if(grep(.x., deparse(tree[[2]]))==1){
      #arccos
    }
    
    if(grep(.x., deparse(tree[[3]]==1))){
      #arcsin
    }
  }
  
  return(NULL)
    
}

#'Takes a call and returns its affine coefficients.
#'
#'@param tree A call that will be parse
#'@param .x. the variable name
#'
#'@return A list with values of a and b satisfying a*.x.+b  = tree.
#'If the expression is not affine, returns an empty list.
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
