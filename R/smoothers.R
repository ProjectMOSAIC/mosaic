smoother <- function(formula, data, span=0.5, degree=2, ... ) {
  input.names <- all.vars(formula)[-1]
  L <- loess(formula, data, span=span, degree=degree, ..., control=loess.control(surface="direct"))
  makeDF <- paste( "data.frame( ", paste(input.names,collapse=",",sep=""),")")
  F <- function() {
    D <- eval(parse(text=makeDF))
    predict(L, newdata=D)
  }
  tmp <- paste("alist( ", paste(input.names, "=", collapse = ",", sep = ""), ")")
  tmp <- eval(parse(text = tmp))
  formals(F) <- tmp
  return(F)
}
# =============================
linearModel <- function(formula, data, ...) {
  input.names <- all.vars(formula)[-1]
  L <- lm(formula, data, ...)
  makeDF <- paste( "data.frame( ", paste(input.names,collapse=",",sep=""),")")
  F <- function() {
    if( showcoefs ) coef(L)
    else { # evaluate the function 
      D <- eval(parse(text=makeDF))
      predict(L, newdata=D)
    }
  }
  tmp <- paste("alist( ", paste(input.names, "=", collapse = ",", sep = ""), ",showcoefs=FALSE)")
  tmp <- eval(parse(text = tmp))
  formals(F) <- tmp
  attr(F,"mosaicType") <- "Fitted Linear Model"
  return(F)
}
# =============================
interpolatingFunction <- function(formula, data, method="fmm",monotonic=FALSE,connect=FALSE) {
  fnames <- all.vars(formula)
  if( length(fnames) > 2 )
    stop("Sorry: Doesn't yet handle multiple input variables.")
  y <- get(fnames[1],pos=data)
  x <- get(fnames[2],pos=data)
  if( connect ) SF <- approxfun(x,y,rule=2)
  else {
    if( ! monotonic )  SF <- splinefun(x,y,method=method)
    else SF <- splinefun(x,y,method="monoH.FC")
  }
  F <- function(foobar, deriv=0 ){
    x <- get(fnames[2])
    if(connect) SF(x)
    else SF(x,deriv=deriv)
  }
  if (connect) tmp <- paste("alist( ", fnames[2], "=)", sep="")
  else tmp <- paste("alist( ", fnames[2], "=, deriv=0)", sep="")
  formals(F) <- eval(parse(text=tmp))
  return(F)
}
# ==============
spliner <- function(formula, data,method="fmm",monotonic=FALSE) {
  interpolatingFunction(formula, data, method=method, monotonic=monotonic)
}
# ==============
connector <- function(formula, data, method="linear") {
  interpolatingFunction(formula, data, connect=TRUE)
}
