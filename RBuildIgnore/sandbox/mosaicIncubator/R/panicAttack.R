#' Check for accidental overwriting of critical functionality
#' 
#' @details
#' Sometimes people will unintentially create a function whose name masks that of a
#' function in R.  An example is a statement like \code{c<-makeFun(x^2~x)} 
#' which will hide the all-important \code{c()} function.  This causes great consternation,
#' since seemingly unrelated things will stop working.  \code{panicAttack()} identifies
#' where a name created by a user masks things in this way. 
#' 
#'  @param fix  Whether or not to remove the offending objects 
#'  in the user's global environment.  The default (\code{FALSE}) behavior
#'  is to display a list of conflicts.  The user, if desired, can then rename these by 
#'  hand and \code{remove()} them by hand.  If \code{fix=TRUE}, the conflicting functions 
#'  will be removed from the user's global environment.
#'  
#'  @author Aaron Mayerson and Daniel Kaplan  (\email{kaplan@@macalaster.edu})
#'  
#'  @seealso \code{\link{remove}}
#'  
#'  @return A character vector of the conflicting function names.
#'  
#'  @examples
#'  \dontrun{
#'  require(mosaic)
#'  c <- makeFun(x~x) # Trouble!
#'  plotFun <- c # more trouble!
#'  panicAttack() # identify the problems
#'  panicAttack(fix=TRUE)  # removed offending problems automatically
#'  }
#'  
#'  @export

panicAttack <- function(fix=FALSE){
  funs <- utils::lsf.str(envir = .GlobalEnv)
  problems <- NULL  #base::c()
  if (length(funs) == 0){
    message("There are no user-defined functions.")
    return(base::invisible(problems))
  }
  for (i in 1:base::length(funs)){
    name <- funs[i]
    temp <- base::get(name, envir = .GlobalEnv)
    # Strategy: remove it and see if it still exists
    base::remove(list = base::list(name)[[1]], envir = .GlobalEnv)
    # If it exists add the name on to the list of problem functions
    if(base::exists(name,mode="function")) problems <- base::c(problems, name)
    # Put it back and clean up
    base::assign(base::get("name"),base::get("temp"), envir = .GlobalEnv)
    base::remove(temp)
  }
  nConflicts <- base::length(problems)
  if (nConflicts == 0){
    base::message("No built-in functions have been over-ridden.")
    return(base::invisible(problems))
  }
  conflictListing <- base::paste(base::paste("\t",problems,"()",sep=""),collapse="\n")
  if(fix){
    txt <- "The following user-defined functions were removed: \n"
    for (i in 1:nConflicts) base::remove(list = base::list(problems[i])[[1]], envir = .GlobalEnv)
    problems <- NULL
  }
  else{
    txt <- "The following user-defined functions conflict with built-in functions: \n"
  }
  base::message(paste(txt,conflictListing,sep=""))
  return(base::invisible(problems))
} 