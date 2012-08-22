#'Expand the left-hand side of a formula
#'
#'Expands the contents of functions used in a formula.
#'
#'@rdname expandFun
#'@name expandFun
#'@aliases expandFun
#'
#'@param formula A mathematical expression (see examples and \code{\link{plotFun}})
#'@param \ldots additional parameters
#'
#'@return A list with the new expanded formula and the combined formals
#'
#'
#'@examples
#'f=makeFun(x^2~x)
#'expandFun(f(z)~z) #Returns z^2~z
expandFun <- function(formula,...){
  exp = lhs(formula)
  #See if first operator is a primitive
  #Check if first operation is an arithmetic operator
  if(is.primitive(eval(exp[[1]]))&&getGroup(toString(exp[[1]]))[[1]]=="Arith"){
    ##Recursively replace variables
    if(class(exp[[2]]) == "call"){
      form2 = formula
      form2[[2]] = exp[[2]]
      lside = Recall(form2)
      exp[[2]] = lside$formula[[2]]
    }
    if(class(exp[[3]]) == "call"){
      form3 = formula
      form3[[2]] = exp[[3]]
      rside = Recall(form3)
      exp[[3]] = rside$formula[[2]]
    }
    newcall = paste("(",deparse(exp[[2]]),")",deparse(exp[[1]]),"(",deparse(exp[[3]]),")")
    newcall = parse(text = newcall)[[1]]
    formula[[2]]=newcall
    return(list(formula=formula, formals=
      as.pairlist(c(lside$formals, rside$formals))))
  }
  ##Save function to somehwere
  func =eval(exp[[1]])  # get the function itself
  body=body(func)
  formals=formals(func)
  vars = all.vars(body)
  
  ##Replace body with match.call and evaluate to find out which params map to what
  fnew = func
  body(fnew)=parse(text="as.list(match.call())")
  exp[[1]] <- as.name("fnew")
  argmap = eval(exp)
  #Substitute new variables into the body.
  if("{"==class(body)){#If there is more than one call in the function, takes the last call.
    body=body[[length(body)]]
  }
  sformals = deparse(formals)
  sbody = deparse(body)
  for (pattern in vars) { # loop over the names of the formals
    if(!is.null(argmap[[pattern]])){
      replacement = deparse(argmap[[pattern]])
      sformals = gsub(pattern, replacement, sformals)
      replacement = paste("(",replacement,")") #To preserve order of operations
      newbody = gsub(pattern,replacement, sbody)
      sbody = newbody
    }
  }
  #Substitute lhs of formula for new expression
  newbody=parse(text=newbody)
  formula[[2]] = newbody[[1]]
  #update formals
  sformals = strsplit(sformals, "[[\\(\\)]]*")[[1]][2]
  sformals = paste("as.pairlist(alist(", sformals, "))", sep="")
  formals(func) = eval(parse(text=sformals))
  formals=formals(func)
  
  return(list(formula=formula, formals=formals))
}