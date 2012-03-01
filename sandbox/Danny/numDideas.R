# Testing out ideas for numerical derivatives.
numD = function(formula, ...) {
  # translate the formula into a function
  f <- makeFunction(formula, ...)
  # find the variables with respect to which the derivative is to be taken
  dvars <- RHS.vars(formula) # DO WE NEED THIS MORE GENERALLY?
  # What sort of derivative?
  if (length(dvars)==1) { #Simple first derivative 
    res <- function() {
     args <- as.list(match.call()[-1]) # turn arguments into a list
     # TO DO: eliminate any arguments that aren't used in the function (such as .hstep)
     # before the call to do.call().
     # BUT, make sure that any variables from dvars are in the args.
     
     #evaluate any names that were passed as arguments
     #so that we can manipulate the quantity outside the function
     args[[dvars[1]]] <- eval( args[[dvars[1]]]) 
     left <- args; left[[dvars[1] ]] <- left[[dvars[1]]] + .hstep
     right <- args; right[[dvars[1] ]] <- right[[dvars[1]]] - .hstep
     (do.call( f, left ) - do.call(f, right))/(2*.hstep)
    }
    formals(res) <- c(formals(f), .hstep=.000001 ) # back off of 1e-8 in case people take a 2nd deriv.
    return(res) 
  }
  if (length(dvars==2) & dvars[1]==dvars[2]) { # second unmixed partial 
    res <- function() {
      args <- as.list(match.call()[-1]) # turn arguments into a list
      args[[dvars[1]]] <- eval( args[[dvars[1]]] ) #evaluate a name if passed as argument
      left <- args; left[[dvars[1]]] <- left[[dvars[1]]] + .hstep
      right<- args; right[[dvars[1]]] <- right[[dvars[1]]] - .hstep
      return((do.call( f, left ) + do.call(f, right) - 2*do.call(f,args))/(.hstep*.hstep))
    }
    formals(res) <- c(formals(f), .hstep=.0001 )
    return(res)
  }
  if (length(dvars)==2) { # mixed partial
    res <- function() {
      # mixed partials based on 4 corner finite-difference
      args <- as.list(match.call()[-1]) # turn arguments into a list
      args[[dvars[1]]] <- eval( args[[dvars[1]]] ) #evaluate names if passed as arguments
      args[[dvars[2]]] <- eval( args[[dvars[2]]] )
      left <- args; left[[dvars[1]]] <- left[[dvars[1]]] + .hstep
      lu <- left; lu[[dvars[2]]] <- lu[[dvars[2]]] + .hstep #left upper
      lb <- left; lb[[dvars[2]]] <- lb[[dvars[2]]] - .hstep #left bottom
      right <- args; right[[dvars[1]]] <- right[[dvars[1]]] - .hstep
      ru <- right; ru[[dvars[2]]] <- ru[[dvars[2]]] + .hstep #right upper
      rb <- right; rb[[dvars[2]]] <- rb[[dvars[2]]] - .hstep #right bottom
      return((do.call(f,lu)+do.call(f,rb) - (do.call(f,lb)+do.call(f,ru)))/(4*.hstep*.hstep))
    }
    formals(res) <- c(formals(f), .hstep=.0001 )
    return(res)
  }
  if (length(dvars)>2){
    stop("Order greater than 2 not yet implemented.")
  }
}



# Get the variables on the RHS of a formula, in order, with repeats
RHS.vars <- function(formula) {
  vars <- all.names(rhs(formula))
  vars[vars %in% all.vars(rhs(formula))]
}

