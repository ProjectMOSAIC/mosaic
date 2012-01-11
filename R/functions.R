
makeFun = function( sexpr=NULL, ... ) {
  foo = .createMathFun( sexpr=sexpr, ... )   # find this
  x = foo$fun
#  attr(x,"mosaicType") = "Constructed function"
#  attr(x,"functionExpression") = foo$sexpr
  body(x) = foo$sexpr
  return(x)
}


print.function = function(x, useSource=TRUE, default=FALSE, ...) {
  kind = attr(x,"mosaicType")
  if( is.null(kind) | default) {
    7
  }
  else {
    if( kind %in% c("Numerical finite-difference process","Numerical integration process")) {
      hoo = formals(x)
      if ("..args" %in% names(hoo)) {
        goo = hoo$..args
        hoo$..args = NULL        
      }
      formals(x) = hoo
      body(x) = paste(kind,"on", deparse(goo$sexpr), "with respect to", goo$names  )
    }
    else if( kind == "Fitted Linear Model") {
      vals = x(showcoefs=TRUE) # get the coefficients
      nms = names(vals)
      cc = as.numeric(vals)
      if( nms[1] == "(Intercept)" ) nms[1] = "1"
      body(x) = parse(text=paste(cc, nms, sep="*",collapse="+"))
    }
  }
  base::print.function(x, useSource=useSource, ...)
}

# for printing the arguments to a function, nicely
#arguments = function(f) {
#  nms = names(formals(f))
#  nms[ !nms %in% c("..args","init.x","init.val") ]
#}
