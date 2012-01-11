D = function(expr, ..., ..h..=NULL, numerical=FALSE, method=c("center","right")){
  #vals = list(...)
  sexpr = substitute(expr)
  fm = mosaic:::.createMathFun(sexpr=sexpr, ...)

  .doD(fm, numerical=numerical, method=method, ..h..=..h..,...)
}

.doD = function( fm, ..h..=NULL, numerical=FALSE, method="center",...){
   vals = list(...)
   # see if the expression is simple enough to use the symbolic method
  .ops = setdiff( unique(all.names(fm$sexpr)), unique(all.vars(fm$sexpr)))
  .allowed.ops = c("+","-","*","/","^","(", "exp", "log", "sin", "cos",
    "tan", "sinh", "cosh", "sqrt", "pnorm", "dnorm", "asin", "acos", "atan",
    "gamma", "lgamma", "digamma", "trigamma")
  .can.do.symbolic = all(.ops %in% .allowed.ops)
  if (!numerical & .can.do.symbolic) {
    .df = tryCatch(.d.symbolic(fm), error=function(e){NULL})
    if( !is.null(.df) ) return(.df)
    warning("Could not do derivative symbolically.  Returning numerical derivative.")
  }

  # do the derivative numerically
  # Check whether there are repeated variables in the RHS of the formula
  fm$names = fm$RHS[fm$RHS %in% fm$names]
  # information to pass along to the differentiating functions
  needed = list(sexpr=fm$sexpr, names=fm$names, h=..h..)
  
  if(length(fm$names)==1){ # a first-order derivative
    ..h.. = ifelse( is.null(..h..), 1e-4, ..h.. )
    needed$h = ..h..
    if(method[1]=="right") .df = .d1.right
    if(method[1]=="center") .df = .d1.center
  }
  else{
    if(length(fm$names)==2) {
      ..h.. = ifelse( is.null(..h..), 1e-4, ..h.. )
      needed$h = ..h..
      if( length(unique(fm$names))==1 ) .df = .d2.xx #repeated w.r.t. same variable.
      else .df = .d2.xy #mixed derivative
    }
    else if(length(fm$names)>2) stop("Handles only 1st- and 2nd-order derivatives.")
  }
  
  # Assign the arguments to the function
  fargs = c(list())
    # put the arguments in an "alist" so that they are unbound 
    one = list()
    two = list()
    tmp1 = unique(fm$names)
    if( length(tmp1) > 0) {
     tmp = paste( "alist( ", 
         paste(tmp1, "=",collapse=",",sep=""),")")
     one = eval(parse(text=tmp))    
    }
    tmp2 = all.vars(fm$sexpr,functions=FALSE)
    tmp2 = setdiff(tmp2,tmp1)
    if( length(tmp2) > 0) {
     tmp = paste( "alist( ", 
         paste(tmp2, "=",collapse=",",sep=""),")")
     two = eval(parse(text=tmp))    
    }
    fargs =c(one,two,fargs)
    fargs[names(vals)] = vals

    # EXCEPTIONS for 
    # global variables which are not accessible
    # if they are contained in the formals list
    fargs["pi"] = NULL # let pi stand for pi
    # Pass the sexpr expression and name information to the function as a default
    fargs[["..args"]] = needed
    
  
  formals(.df) = fargs #formals(fm$fun)
  environment(.df) = parent.frame()  # set the environment where the function will look for the expression
  attr(.df,"mosaicType") = "Numerical finite-difference process"
  return(.df) 
}
# =====================
# various functions for performing derivatives
.d1.right = function() {
    ..h.. = ..args$h
    ..baseline = eval(..args$sexpr)
    ..vvv = get(..args$names[1])
    # to avoid round-off error in h 
    ..temp = ..vvv + ..h..
    ..h.. = ..temp-..vvv
    assign(..args$names[1], ..vvv+..h..)
    return( (eval(..args$sexpr) - ..baseline)/..h..)
}
# =====
.d1.center = function() {
        ..h.. = ..args$h
      ..vvv = get(..args$names[1])
      # to avoid round-off error in h 
      ..temp = ..vvv + ..h..
      ..h.. = ..temp-..vvv
      assign(..args$names[1], ..vvv + ..h..)
      ..right = eval(..args$sexpr)
      assign(..args$names[1], ..vvv - ..h..)
      ..left = eval(..args$sexpr)
      return( (..right - ..left)/(2*..h..))
     }
#=============
.d2.xx = function() {
   ..h.. = ..args$h
   ..vvv = get(..args$names[1])
   assign(..args$names[1], ..vvv)
   ..center = eval(..args$sexpr)
   # to avoid round-off error in h 
   ..temp = ..vvv + ..h..
   ..h.. = ..temp-..vvv
   assign(..args$names[1], ..vvv + ..h..)
   ..right = eval(..args$sexpr)
   assign(..args$names[1], ..vvv - ..h..)
   ..left = eval(..args$sexpr)
   return( ((..right+..left)-2*..center)/(..h..^2))
}
# ===============
.d2.xy = function() {
       ..h.. = ..args$h
       ..one = get(..args$names[1])
       ..two = get(..args$names[2])
       # to avoid round-off error in h 
       ..temp = ..one + ..h..
       ..h.one.. = ..temp-..one
       ..temp = ..two + ..h..
       ..h.two.. = ..temp - ..two
       # Evaluate at 4 points on grid Lu, Ru, Ld, Rd
       assign(..args$names[1], ..one + ..h.one..) #move right
       assign(..args$names[2], ..two + ..h.two..) #move up
       ..ru = eval(..args$sexpr)
       assign(..args$names[2], ..two - ..h.two..) #move down
       ..rd = eval(..args$sexpr)
       assign(..args$names[1], ..one - ..h.one..) #move left
       assign(..args$names[2], ..two + ..h.two..) #move up
       ..lu = eval(..args$sexpr)
       assign(..args$names[2], ..two - ..h.two..) #move down
       ..ld = eval(..args$sexpr)
     
       return( ((..ru-..rd)/..h.two.. -(..lu-..ld)/..h.two..)/(4*..h.one..))
     }
# ===================
# Symbolic derivative
.d.symbolic = function(.f ) { #argument is list from .createMathFun
  fform = function(){}
  formals(fform) = formals(.f$fun)
  sexpr = .f$sexpr
  wrtNames = .f$RHS[ .f$RHS %in% .f$names ] # differentiate with respect to these
  for (j in 1:length(wrtNames)) {
    df = deriv(sexpr, namevec=wrtNames[j], function.arg=fform)
    bd = as.character(body(df))
    final.expr = gsub(".grad.+<-", "",bd[length(bd)-2])
    # substitute in the values of .value, .expr1, and so on
    for (k in (length(bd)-4):2 ) {
      nm = gsub(" <-.+$","", bd[k])
      val = gsub("^.+<- ","",bd[k])
      final.expr = gsub(nm,paste("(",val,")",sep=""), final.expr)
    }
    sexpr = parse(text=final.expr)
  }
  body(df) = parse(text=final.expr)
  return(df)
}
