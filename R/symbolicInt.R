##change everything to return formulae instead of functions.
## add division stuff
#catch when n=-1 and return log instead.

#'Find the symbolic integral of a formula
#'
#'@param form formula to be integrated.  Rhs of formula indicates which variable to
#'integrate with respect to.  Must only have one variable.
#'@param \ldots extra parameters
#'
#'@details works for simple polynomial expressions
#'
symbolicInt<- function(form, ...){
  dots = list(...)
  rhsVar = all.vars(rhs(form))
  if(length(rhsVar)!=1) stop("Can only integrate with respect to one variable.")
  constants = setdiff(all.vars(form), rhsVar)
  if(class(lhs(form))=="call"&&is.primitive(eval(lhs(form)[[1]])))
    group = getGroup(toString(lhs(form)[[1]]))[[1]] #determine typ of highest-level expr.
  else group = -1
  if(group =="Arith")
    return(intArith(form, ...))
  if(group =="Math")
    return(intMath(form, ...))
  
  if(is.numeric(lhs(form))||is.element(deparse(lhs(form)), constants)){
    form[[2]]<- parse(text = paste(deparse(lhs(form)), "*", rhsVar))[[1]]
    return(makeFun(form,...))
  }
  if((lhs(form))==rhsVar){
    form[[2]]<- parse(text="1/(2)*x^2")[[1]]
    return(makeFun(form, ...))
  }
  
#     if(regexpr(rhsVar, deparse(lhs(form)))[1]==-1){ #this is a constant
#     strexpr = paste(gsub(" ", "", deparse(lhs(form))), "*", rhsVar, sep="")
#     form[[2]] = parse(text=strexpr)[[1]]
#     return(makeFun(form, ...))
#   
#   }
#   
#   strexpr = gsub(" ", "", deparse(lhs(form))) #change formula to string and remove white space
#   #regular expression to identify expr of form c1*x^n
#   regex = paste("^(([[:digit:]]+\\*)*([", paste(constants, collapse=""),"]+\\*)*)*["
#                 ,paste(rhsVar, collapse=""),"](\\^[[:digit:]]+)?$")
#   if(regexpr(regex, strexpr)[1]==1){ #then the expression is of the desired form
#     split = strsplit(strexpr, "\\^")[[1]]
#     n = as.numeric(split[2])#find the exponent value
#     if(is.na(n)) n=1
#     if(n==-1) stop("Error: symbolic algorithm gave up")
#     split[2] = toString(n+1)
#     strexpr = paste(split, collapse="^")
#     split = strsplit(strexpr, paste("\\*[",rhsVar,"]"))[[1]] #divide constants by n+1
#     split[1] = paste(split[1],"/", toString(n+1), sep="")
#     strexpr = paste(split, collapse=paste("*", rhsVar, sep=""))
#     
#     #replace lhs of formula with integrated expression
#     form[[2]] = parse(text = strexpr)[[1]]
#   }
#   else stop("Error: symbolic algorithm gave up")
#   return(makeFun(form))
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
      length(grep(rhsVar, deparse(lform[[2]])))>0)#too complex
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
    if(lhs(form)[[2]] == rhsVar &&length(grep(rhsVar, deparse(lhs(form)[[3]])))==0){
      exp = try(evalq(form[[2]][[3]], envir=list(pi=3.1415932653589793, form=form),
                      enclos=NULL), silent=TRUE)
      if(inherits(exp, "try-error"))
        exp = parse(text = paste(deparse(lhs(form)[[3]]), "+1"))[[1]]
      else(exp = eval(exp)+1) #change from call to numeric...
      ###FIX
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
    
  }
  
  if(op == "log"){
    
  }
  
}

