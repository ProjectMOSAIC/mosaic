#'Find the symbolic integral of a formula
#'
#'@param form formula to be integrate
#'@param \ldots extra parameters
#'
#'@details works for simple polynomial expressions
#'
symbolicInt<- function(form, ...){
  dots = list(...)
  
  rhsVars = all.vars(rhs(form))
  constants = setdiff(all.vars(form), rhsVars)
  
  if(lhs(form)[[1]]=='+'||lhs(form)[[1]]=="-"){ #need to do a recursive call. Possible bug.
                                                  #If '-' acts as negator.
    lform = form
    rform = form
    lform[[2]] = lhs(form[[2]])
    rform[[2]] = lhs(form[[3]])
    lfun = Recall(lform, ...)
    rfun = Recall(rform, ...)
    body = body(parse(text=paste(deparse(lfun),
                                 deparse(form[[1]]), deparse(rfun))))
    body(lfun) = body
    return(lfun)
  }
  
  if(lhs(form)[[1]]=="sin"){#trig expression
    #check to see if we can integrate it
    browser()
    strexpr = gsub(" ", "", deparse(lhs(form)[[2]]))
    regex = paste("^(([[:digit:]]+\\*)*([", paste(constants, collapse=""),"]+\\*)*)*["
                  ,paste(rhsVars, collapse=""),"]$")
    if(regexpr(regex, strexpr)[1]==1){
      split = strsplit(strexpr, paste("*",rhsVars, sep=""), fixed=TRUE)[[1]]
      split= paste("-1/(",split,")*cos" ,sep="")
      form[[2]][[1]]= parse(text=split)[[1]]
      return(makeFun(form))
    }
  }
  browser()
  strexpr = gsub(" ", "", deparse(lhs(form))) #change formula to string and remove white space
  #regular expression to identify expr of form c1*x^n
  regex = paste("^(([[:digit:]]+\\*)*([", paste(constants, collapse=""),"]+\\*)*)*["
                ,paste(rhsVars, collapse=""),"](\\^[[:digit:]]+)?$")
  if(regexpr(regex, strexpr)[1]==1){ #then the expression is of the desired form
    split = strsplit(strexpr, "\\^")[[1]]
    n = as.numeric(split[2])#find the exponent value
    if(n==-1) stop("Error: formula cannot be symbolically integrated")
    split[2] = toString(n+1)
    strexpr = paste(split, collapse="^")
    split = strsplit(strexpr, paste("\\*[",rhsVars,"]"))[[1]] #divide constants by n+1
    split[1] = paste(split[1],"/", toString(n+1), sep="")
    strexpr = paste(split, collapse=paste("*", rhsVars, sep=""))
    
    #replace lhs of formula with integrated expression
    form[[2]] = parse(text = strexpr)[[1]]
  }
  else stop("Error: formula cannot be symbolically integrated")
  return(makeFun(form))
}

