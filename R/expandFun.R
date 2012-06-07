#'Expand the left-hand side of a formula
#'
#'Expands the contents of functions used in a formula.
#'
#'@rdname expandFun
#'@name expandFun
#'@aliases expandFun
#'
#'@param formula A mathematical expression (see examples and \code{\link{plotFun}})
#'@param \ldots additional parameters, typically default values for mathematical parameters
#'
#'@return A new formula with expanded left-hand side.
#'
#'@details
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
    if(class(exp[[2]]) == "call")
      exp[[2]] = Recall(exp[[2]])
    if(class(exp[[3]]) == "call")
      exp[[3]] = Recall(exp[[3]])
    newcall = paste("(",deparse(exp[[2]]),")",deparse(exp[[1]]),"(",deparse(exp[[3]]),")")
    newcall = parse(text = newcall)[[1]]
    formula[[2]]=newcall
    return(formula)
  }
  ##Save function to somehwere
  func =eval(exp[[1]])  # get the function itself
  body=body(func)
  formals=formals(func)
  vars = names(formals)
  
  ##Replace body with match.call and evaluate to find out which params map to what
  fnew = func
  body(fnew)=parse(text="as.list(match.call())")
  exp[[1]] <- as.name("fnew")
  argmap = eval(exp)
  #Substitute new variables into the body.
  if("{"==class(body)){#If there is more than one call in the function, takes the last call.
    body=body[[length(body)]]
  }
  sbody = deparse(body) 
  for (pattern in vars) { # loop over the names of the formals
    replacement = deparse(argmap[[pattern]])
    replacement = paste("(",replacement,")") #To preserve order of operations
    newbody = sub(pattern,replacement, sbody)
    sbody = newbody
  }
  
  #Substitute lhs of formula for new expression
  newbody=parse(text=newbody)
  formula[[2]] = newbody[[1]]
  return(formula)
}