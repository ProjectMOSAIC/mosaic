#' Internal function for handling formula-based expressions
#'
#' \code{.createMathFun} processes an expression and assignments to produce 
#' a function and other information used by D, antiD, and plotFun
#'
#' @author Daniel Kaplan (\email{kaplan@@macalester.edu})
#' @rdname createMathFun
#'
#' @param sexpr a formula containing a mathematical expression.  The left-hand side
#' has the expression, the right side contains the variables
#' @param ... additional assignments giving default values to symbolic parameters.
#'
#' @details
#' \code{.createMathFun} underlies user functions such as \code{makeFun}, \code{plotFun}, 
#' and \code{D}.  It returns a structure that contains a function as well as information
#' about the function needed for plotting, etc.
#'
# can't do this (and shouldn't anyway) with internal functions.
#
# @examples
# .createMathFun( a*sin(x)*y ~ x&y, a=2)

.createMathFun = function(sexpr=NULL, ...) { 
  # sexpr = substitute(expr)
  # reconstruct the function finput from expr
  if(is.character(sexpr)) { # passing a function, e.g. sin
    fcall = paste(sexpr, "(x)~x",sep="")
    expr = parse(text = fcall)
    sexpr = substitute(expr)
  }
  if (is.numeric(sexpr)) {
    fcall = paste(sexpr,"+0*x ~ x",sep="")
    expr = parse(text=fcall)
    sexpr = substitute(expr)
  }
  # is.name(sexpr) is handled below
  
  #sexpr comes from a substitute(expr) in the top-level interface function.
  vals = list(...)
  # if( !is.null(s)) vals[["s"]] = s  
  # The above line, and the use of s in the argument list are
  # a kluge so that "s" can be used in the expression.
  specialNames=NULL
  specialVals=NULL
  rightSide=NULL
  if (is.name(sexpr)) {
    # Strategy, turn it into something of the standard form and call
    # this function recursively.
    fargs = formals(eval(sexpr))
    # kill off anything named ..args
    fargs["..args"] = NULL
    if( length(fargs) == 0 ) fcall = paste(sexpr, "(x)~x",sep="")
    else {
      if( length(fargs) == 1 ) 
        fcall = paste(sexpr,"(",names(fargs),") ~ ", names(fargs))
      else {
        nms = c()
        for (k in 1:length(fargs)) {
          if( nchar(as.character(fargs[[k]]))>0 )
            nms = c(nms, names(fargs)[k])
        }
        fcall = paste( sexpr,"(", paste(nms,collapse=","),")~",
                       paste(nms,collapse="+"))
      }
    }
    expr <- parse(text = fcall)
    sexpr = substitute(expr)
    return( .createMathFun(sexpr, ...) )
  }
  exprClass = tryCatch(class(eval(sexpr)), error=function(e){class(sexpr)})
  if (exprClass == "formula") {
    # Get the special names from the right side of the formula
    expr = eval(sexpr)
    sexpr = expr[[2]] # left side of formula
    specialNames = all.vars(expr[[3]]) # right side of formula
    rightSide = all.names(expr[[3]]) # for detecting possibly repeated names
    specialVals = NULL
  }
  else if (exprClass == "call") {
    nmvals = names(vals)
    # which ones are the limits?
    sz = mapply( length, vals )
    inds = which(sz==2)
    specialNames = nmvals[inds]
    specialVals = vals[inds]
    vals[specialNames] = NULL
  }
  
  # create the formal arguments of the function
  goo = c(list())
  # put the arguments in an "alist" so that they are unbound 
  one = list()
  two = list()
  if( length(specialNames) > 0) {
    tmp = paste( "alist( ", 
                 paste(specialNames, "=",collapse=",",sep=""),")")
    one = eval(parse(text=tmp))    
  }
  tmp2 = all.vars(sexpr)
  tmp2 = setdiff(tmp2,specialNames)
  if( length(tmp2) > 0) {
    tmp = paste( "alist( ", 
                 paste(tmp2, "=",collapse=",",sep=""),")")
    two = eval(parse(text=tmp))    
  }
  goo =c(one,two,goo)
  #goo[specialNames] = NA # put it first in the list
  #goo[all.vars(sexpr)] = NA
  goo[names(vals)] = vals 
  # EXCEPTIONS for 
  # global variables which are not accessible
  # if they are contained in the formals list
  goo["pi"] = NULL # let pi stand for pi
  
  # kill anything with a ..args argument
  goo["..args"] = NULL
  ff = function() {
    eval(sexpr, enclos=parent.frame())
  }
  formals(ff) = goo
  return(list(fun=ff,names=specialNames,
              vals=specialVals,others=names(vals),
              sexpr=sexpr,RHS=rightSide))
}


createMosaicFunction <- function(sexpr=NULL, ...) { 
  # sexpr = substitute(expr)
  # reconstruct the function finput from expr
  if(is.character(sexpr)) { # passing a function, e.g. sin
    fcall <- paste(sexpr, "(x)~x",sep="")
    expr <- parse(text = fcall)
    sexpr <- substitute(expr)
  }
  if (is.numeric(sexpr)) {
    fcall <- paste(sexpr,"+0*x ~ x",sep="")
    expr <- parse(text=fcall)
    sexpr <- substitute(expr)
  }
  # is.name(sexpr) is handled below
  
  #sexpr comes from a substitute(expr) in the top-level interface function.
  vals <- list(...)
  # if( !is.null(s)) vals[["s"]] = s  
  # The above line, and the use of s in the argument list are
  # a kluge so that "s" can be used in the expression.
  specialNames<-character(0)  # NULL
  specialVals<-character(0)  # NULL
  rightSide<-character(0)  # NULL
  if (is.name(sexpr)) {
    # Strategy, turn it into something of the standard form and call
    # this function recursively.
    fargs <- formals(eval(sexpr))
    # kill off anything named ..args
    fargs["..args"] <- NULL
    if( length(fargs) == 0 ) fcall <- paste(sexpr, "(x)~x",sep="")
    else {
      if( length(fargs) == 1 ) 
        fcall <- paste(sexpr,"(",names(fargs),") ~ ", names(fargs))
      else {
        nms <- c()
        for (k in 1:length(fargs)) {
          if( nchar(as.character(fargs[[k]]))>0 )
            nms <- c(nms, names(fargs)[k])
        }
        fcall <- paste( sexpr,"(", paste(nms,collapse=","),")~",
                       paste(nms,collapse="+"))
      }
    }
    expr <- parse(text = fcall)
    sexpr <- substitute(expr)
    return( .createMathFun(sexpr, ...) )
  }
  exprClass <- tryCatch(class(eval(sexpr)), error=function(e){class(sexpr)})
  if (exprClass == "formula") {
    # Get the special names from the right side of the formula
    expr <- eval(sexpr)
    sexpr <- expr[[2]] # left side of formula
    specialNames <- all.vars(expr[[3]]) # right side of formula
    rightSide <- all.names(expr[[3]]) # for detecting possibly repeated names
    specialVals <- character(0)  # NULL
  }
  else if (exprClass == "call") {
    nmvals <- names(vals)
    # which ones are the limits?
    sz <- mapply( length, vals )
    inds <- which(sz==2)
    specialNames <- nmvals[inds]
    specialVals <- vals[inds]
    vals[specialNames] <- character(0)  # NULL
  }
  
  # create the formal arguments of the function
  goo <- c(list())
  # put the arguments in an "alist" so that they are unbound 
  one <- list()
  two <- list()
  if( length(specialNames) > 0) {
    tmp <- paste( "alist( ", 
                 paste(specialNames, "=",collapse=",",sep=""),")")
    one <- eval(parse(text=tmp))    
  }
  tmp2 <- all.vars(sexpr)
  tmp2 <- setdiff(tmp2,specialNames)
  if( length(tmp2) > 0) {
    tmp <- paste( "alist( ", 
                 paste(tmp2, "=",collapse=",",sep=""),")")
    two <- eval(parse(text=tmp))    
  }
  goo <-c(one,two,goo)
  #goo[specialNames] = NA # put it first in the list
  #goo[all.vars(sexpr)] = NA
  goo[names(vals)] <- vals 
  # EXCEPTIONS for 
  # global variables which are not accessible
  # if they are contained in the formals list
  goo["pi"] <- NULL # let pi stand for pi
  
  # kill anything with a ..args argument
  goo["..args"] <- NULL
  ff <- function() {
    eval(sexpr, enclos=parent.frame())
  }
  formals(ff) <- goo
  return(new('mosaicFunction', fun=ff, names=specialNames,
              vals=specialVals, others=as.character(names(vals)),
              sexpr=sexpr, rhs=rightSide))
}

