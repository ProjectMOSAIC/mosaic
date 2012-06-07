#Symbolic Differentiation when the lhs of a formula is a function
newD <- function(form){
  
  #add a try symbolic D and a numerical D for the failures.
  parse.formula(form)
  form[[2]]<-replace(lhs(form))# replace left hand side of form
  
  mosaic::D(form)
}

replace <- function(exp){
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
    return(newcall)
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
  #  sbody = strsplit(sbody,"" )[[1]] Probably don't need this line???
  for (pattern in vars) { # loop over the names of the formals
    replacement = deparse(argmap[[pattern]])
    replacement = paste("(",replacement,")") #To preserve order of operations
    newbody = sub(pattern,replacement, sbody)
    sbody = newbody
  }
  
  #Substitute lhs of formula for new expression
  newbody=parse(text=newbody)
  return(newbody[[1]])
}