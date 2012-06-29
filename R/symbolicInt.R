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
  
  #check if it's just constants
  if(is.numeric(lhs(form))||is.element(deparse(lhs(form)), constants)){
    form[[2]]<- parse(text = paste(deparse(lhs(form)), "*", rhsVar))[[1]]
    return(makeFun(form,...))
  }
  
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
    if(length(grep("^\\(.*\\)$", deparse(lhs(form)[[2]])))>0)
      form[[2]][[2]] = lhs(form)[[2]][[2]] #extract expression
    
    regex = paste("\\(?([[:alnum:]]+\\*)*",toString(rhsVar),"([\\+-][[:alnum:]]+)*\\)?",sep="")
    if(length(grep(regex, deparse(lhs(form)[[2]]))>0)
       &&length(grep(rhsVar, deparse(lhs(form)[[3]])))==0){
      exp = try(eval(form[[2]][[3]], envir=list(pi=3.1415932653589793, form=form),
                      enclos=NULL), silent=TRUE)
      if(inherits(exp, "try-error"))
        exp = parse(text = paste(deparse(lhs(form)[[3]]), "+1"))[[1]]
      else(exp = eval(exp)+1) #change from call to numeric...
      #handle the lhs side of ^
      affexp = affine.exp(lhs(form)[[2]])
      
      if(exp == 0){
        form[[2]] <- parse(text = paste("log(", deparse(lhs(form)[[2]]), ")", sep=""))[[1]]
        return(makeFun(form))
      }
      form[[2]][[3]] <- exp
      form[[2]] <- parse(text = paste("1/(", deparse(exp), ")*",
                                           deparse(form[[2]]), sep=""))[[1]]
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
    strexpr = gsub(" ", "", deparse(lhs(form)[[2]]))
    regex = paste("^(([[:digit:]]+\\*)*([", paste(constants, collapse=""),"]+\\*)*)*["
                  ,paste(rhsVar, collapse=""),"]$")
    if(length(grep(regex, strexpr))!=0){
      split = strsplit(strexpr, rhsVar, fixed=TRUE)[[1]]
      if(split=="") split = paste("-cos")
      else{
        split = strsplit(split, "\\*$") #take trailing '*' off end 
        split= paste("-1/(",split,")*cos" ,sep="")
      }
      form[[2]][[1]]= parse(text=split)[[1]]
      return(makeFun(form))
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  if(op == "cos"){
    #check to see if we can integrate it
    strexpr = gsub(" ", "", deparse(lhs(form)[[2]]))
    regex = paste("^(([[:digit:]]+\\*)*([", paste(constants, collapse=""),"]+\\*)*)*["
                  ,paste(rhsVar, collapse=""),"]$")
    if(length(grep(regex, strexpr))!=0){
      split = strsplit(strexpr, rhsVar, fixed=TRUE)[[1]]
      if(split=="") split = paste("sin")
      else{
        split = strsplit(split, "\\*$") #take trailing '*' off end 
        split= paste("1/(",split,")*sin" ,sep="")
      }
      form[[2]][[1]]= parse(text=split)[[1]]
      return(makeFun(form))
    }
    else stop("Error: symbolic algorithm gave up")
  }
  
  if(op == "exp"){
    #Check to see if we can integrate it
    strexpr = gsub(" ", "", deparse(lhs(form)[[2]]))
    regex = paste("^(([[:digit:]]+\\*)*([", paste(constants, collapse=""),"]+\\*)*)*["
                  ,paste(rhsVar, collapse=""),"]$")
    if(length(grep(regex, strexpr))!=0){
      split = strsplit(strexpr, rhsVar, fixed=TRUE)[[1]]
      if(split=="") return(makeFun(form))
      else{
        split = strsplit(split, "\\*$") #take trailing '*' off end 
        split= paste("1/(",split,")*", deparse(lhs(form)) ,sep="")
      }
      form[[2]]= parse(text=split)[[1]]
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
      return(list(a=a,b=b))
    }
    
    if(rexp$a==0 && length(lexp)!=0){
      a = parse(text=paste("(",deparse(lexp$a), ")*(", deparse(rexp$b),")",sep=""))[[1]]
      b = parse(text=paste("(",deparse(lexp$b), ")*(", deparse(rexp$b),")",sep=""))[[1]]
      return(list(a=a,b=b))
    }
  }
  
  if(tree[[1]]=='/'){
    lexp=Recall(tree[[2]], .x.)
    rexp=Recall(tree[[3]], .x.)
    
    if(rexp$a==0 && length(lexp)!=0){
      a = parse(text=paste(deparse(lexp$a), "/(", deparse(rexp$b),")",sep=""))[[1]]
      b = parse(text=paste(deparse(lexp$b), "/(", deparse(rexp$b),")",sep=""))[[1]]
      return(list(a = a, b = b))
    }
  }
  return(list())
}
