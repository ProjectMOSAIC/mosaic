antiD = function(expr, from=0, to=NULL, ...){
  vals = list(...)
  sexpr = substitute(expr)
  # If it's an expression
  foo = .createMathFun(sexpr=sexpr, ...) 
  if( length(foo$names) != 1 ) {
    stop("This function works only with a single variable of integration.")
  }
  finput = function(.x) {
    assign(foo$names[1], .x)
    return( eval(foo$sexpr))
  }
  needed = list(sexpr=foo$sexpr, names=foo$names) # data passed to the function by arguments
  ..foutput = .antiD.x
  
  if( any( c("to","from") %in% c(foo$names,names(formals(foo$fun)))) )
    stop("Can't use a variable called 'to' or 'from' in a function being integrated.")
  starting.args = alist(to=)
  original.args = formals(foo$fun)
  fargs = c(starting.args, original.args)

  # Construct the argument list so that unbound variables have no default values.  
  tmp2 = all.vars(foo$sexpr)
  tmp2 = setdiff(tmp2,names(fargs))
  one = list()
  if( length(tmp2) > 0) {
     tmp = paste( "alist( ", 
         paste(tmp2, "=",collapse=",",sep=""),")")
     one = eval(parse(text=tmp))    
  }
  fargs =c(fargs, one, list(from=from))
  
  fargs[names(vals)] = vals
  fargs[foo$names] = NULL 
  # EXCEPTIONS for 
  # global variables which are not accessible
  # if they are contained in the formals list
  fargs["pi"] = NULL # let pi stand for pi

  # Pass the sexpr expression and name information to the function as a default
  fargs[["..args"]] = needed
    
  environment(..foutput) = parent.frame()
  formals(..foutput) = fargs
  attr(..foutput,"mosaicType") = "Numerical integration process"
  return(..foutput) 
}
# ===============================
.antiD.x = function() { # really a function of the upper limit: "to"
  ..finput = function(.x) { # Create the function in this environment
    assign(..args$names[1], .x)    
    return( eval(..args$sexpr) )
  }  
  # handle the case where to is fixed and from is assigned to multiple values
  ..multiplier=1
  if( length(from) > 1 & length(to) == 1 ){
    ..temp = to
    to = from
    from = ..temp
    ..multiplier=-1
  }
  # handle situation where both from and to are a set of values.
  if( length(from)>1 & length(to)>1 ){
    if( length(from)!=length(to) ) stop("Either fix 'from' or set it to the same length as 'to'")
    ..res = rep(0,length(to))
    for (..k.. in 1:length(to)) {
      ..res[..k..] = integrate(..finput,from[k],to[k])$value
    }
    return(..res)
  }
  ..val0 = integrate(..finput, from, to[1])$value
  if (length(to) == 1) {
    return(..multiplier*..val0)
  }
  ..res = rep(..val0, length(to))
  for (..k.. in 2:length(..res)) {
    ..res[..k..] = integrate(..finput, to[..k.. - 1], to[..k..])$value
  }
  ..res = cumsum(..res)
  return(..multiplier*..res)
}
