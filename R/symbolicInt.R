##change everything to return formulae instead of functions.
## add division stuff
#catch when n=-1 and return log instead.

#'Find the symbolic integral of a formula
#'
#'@param form formula to be integrated.  Rhs of formula indicates which variable to
#'integrate with respect to.  Must only have one variable.
#'@param \ldots extra parameters
#'
#'@details works for simple polynomial and trigonometric expressions
#'
symbolicInt<- function(form, ...){
  dots = list(...)
  rhsVar = all.vars(rhs(form))
  if(length(rhsVar)!=1) stop("Can only integrate with respect to one variable.")
  constants = setdiff(all.vars(form), rhsVar)
  
  #check if it's just constants
  if(length(grep(rhsVar, deparse(lhs(form))))==0){
    form[[2]]<- parse(text = paste(deparse(lhs(form)), "*", rhsVar))[[1]]
    return(makeFun(form,...))
  }
  
  #check to see if surrounded by parentheses
  if(class(lhs(form))=="("){
    form[[2]]=lhs(form)[[2]]
    return(symbolicInt(form, ...))
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
    form[[2]]<- parse(text="1/(2)*x^2")[[1]]
    return(makeFun(form, ...))
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
      form[[2]] = parse(text=paste("(-1)*",deparse(lhs(form)[[2]]),sep=""))[[1]]
      return(symbolicInt(form,...))
    }
  }
  
  if(op =='+'||op =="-"){ 
    lform = form
    rform = form
    lform[[2]] = lhs(form)[[2]]
    rform[[2]] = lhs(form)[[3]]
    lfun = symbolicInt(lform, ...)
    rfun = symbolicInt(rform, ...)
    body = parse(text=paste(deparse(body(lfun)),
                            deparse(lhs(form)[[1]]), deparse(body(rfun)), sep=""))[[1]]
    form[[2]] <- body
    return(makeFun(form))
  }
  
  if(op == '*'){
    lform = form
    rform = form
    lform[[2]] = lhs(form)[[2]]
    rform[[2]] = lhs(form)[[3]]
    if(length(grep(rhsVar, deparse(lform[[2]])))>0 &&
      length(grep(rhsVar, deparse(rform[[2]])))>0)#too complex
      stop("Error: symbolic algorithm gave up")
    if(regexpr(rhsVar, deparse(lform[[2]]))==1){
      lfun = symbolicInt(lform, ...)
      body = parse(text=paste(deparse(body(lfun)),
                              deparse(lhs(form)[[1]]), deparse(lhs(rform))))[[1]] #parens???
      form[[2]] <- body
      return(makeFun(form))
    }
    else{
      rfun = symbolicInt(rform, ...)
      body = parse(text=paste(deparse(lhs(lform)),
                              deparse(lhs(form)[[1]]),deparse(body(rfun)) ))[[1]] #parens???
      form[[2]] <- body
      return(makeFun(form))
    }
  }
  
  if(op=='/'){#let denominator have negative exponent if there is an x.
    num = lhs(form)[[2]]
    den = lhs(form)[[3]]
    if(length(grep(rhsVar, den))>0){
      form[[2]] = parse(text = paste(deparse(num), "*", deparse(den), "^-1",sep="" ))[[1]]
      return(symbolicInt(form, ...))
    }
    else{
      form[[2]] = parse(text = paste("1/",deparse(den),"*", deparse(num) , sep=""))[[1]]
      return(symbolicInt(form,...))
    }
  }
  
  if(op == '^'){
    #check to see if surrounded by parentheses
    #if(length(grep("^\\(.*\\)$", deparse(lhs(form)[[2]])))>0)
    #  form[[2]][[2]] = lhs(form)[[2]][[2]] #extract expression
    
    affexp = affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0 && length(grep(rhsVar, deparse(lhs(form)[[3]])))==0){
      exp = try(eval(form[[2]][[3]], envir=list(pi=3.1415932653589793, form=form),
                      enclos=NULL), silent=TRUE)
      if(inherits(exp, "try-error"))
        exp = parse(text = paste(deparse(lhs(form)[[3]]), "+1"))[[1]]
      else(exp = eval(exp)+1) #change from call to numeric...
      
      if(exp == 0){
        if(affexp$a==1) form[[2]] = parse(text=paste("log(", deparse(lhs(form)[[2]]), ")",sep=""))[[1]]
        else form[[2]] <- parse(text = paste("1/(",deparse(affexp$a) ,")*log(", deparse(lhs(form)[[2]]), ")", sep=""))[[1]]
        return(makeFun(form))
      }
      form[[2]][[3]] <- exp
      if(affexp$a==1) newform <- paste("1/(", deparse(exp), ")*",
                                       deparse(form[[2]]), sep="")
      else newform <- paste("1/(",deparse(affexp$a),")*1/(", deparse(exp), ")*",
                                           deparse(form[[2]]), sep="")
      form[[2]] <- parse(text=newform)[[1]]
      return(makeFun(form))
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
    affexp = affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0){
      if(affexp$a==1) newform = paste("-cos(", deparse(lhs(form)[[2]]), ")",sep="")
      else newform = paste("1/(", deparse(affexp$a), ")*-cos(", deparse(lhs(form)[[2]]), ")", sep="")
      form[[2]]= parse(text=newform)[[1]]
      return(makeFun(form))
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  if(op == "cos"){
    #check to see if we can integrate it
    affexp = affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0){
      if(affexp$a==1) newform = paste("sin(", deparse(lhs(form)[[2]]), ")",sep="")
      else newform = paste("1/(", deparse(affexp$a), ")*sin(", deparse(lhs(form)[[2]]), ")", sep="")
      form[[2]]= parse(text=newform)[[1]]
      return(makeFun(form))
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  if(op == "exp"){
    #Check to see if we can integrate it
    affexp = affine.exp(lhs(form)[[2]], rhsVar)
    if(length(affexp)>0){
      if(affexp$a==1) newform = paste("exp(", deparse(lhs(form)[[2]]), ")",sep="")
      else newform = paste("1/(", deparse(affexp$a), ")*exp(", deparse(lhs(form)[[2]]), ")", sep="")
      form[[2]]= parse(text=newform)[[1]]
      return(makeFun(form))
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  if(op =='sqrt'){
    #TODO
  }
  
  stop("Error: symbolic algorithm gave up")
}

#'Takes a call and returns its affine coefficients.
#'
#'@param tree A call that will be parse
#'@param .x. the variable name
#'
#'@return A list with values of a and b.  If the expression is not affine,
#'returns an empty list.
#'
affine.exp <- function(tree, .x.){
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
      
      ##ADD
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
