#' A Web and Library Data-Loading Facility
#' 
#' \code{fetchData} provides a means for students and others to 
#' locate and load data sets and R commands provided by instructors.  
#' Data can be pre-loaded for off-line sessions, can be positioned 
#' on identified web sites, or loaded from packages.  
#' \code{fetchData} also will load local files using a complete path name (relative
#' to the current working directory) or, if no name is given, via a dialog box.
#'
#' @param name a character string naming a data set.  
#'    This will often end in \code{.csv} for reading in a data set. 
#'    If it ends in \code{.r} or \code{.R}, the file will be "sourced".
#' @param show.path If \code{TRUE}, causes the current search path to be returned
#' @param add.to.path Name of a web directory (ending in \code{/}), which 
#' should be pre-pended to the search path.
#'   
#' @param drop.from.path Name of a web directory to be deleted from the path. 
#' @param cache If \code{TRUE}, indicates that a data set is to be pre-loaded 
#' into the cached library.  This allows, 
#' for instance, users to pre-load on-line data to be used when they are off-line.
#' @param verbose a logical indicating whether additional status messages (e.g., indicating
#'    where the dataset was located) should be printed.
#' 
#' 
#' @details
#' There are two major purposes for this function. One is to provide a
#' consistent interface to reading data: a file name is given and a data frame is
#' returned, which can be assigned to an object as the user desires.  This
#' differs from the behavior of \code{data}, which doesn't return a value but
#' instead creates an object without explicit assignment.
#' 
#' The other purpose is to allow instructors or other group leaders to post data 
#' and R code on
#' web sites that can be searched as naturally as if the data were on the users'
#' own machines.  For instance, an instructor might want to post a new data set
#' just before class, enabling her students to access it in class.
#' 
#' To support this, \code{fetchData} allows new web sites to be added to the
#' web search path.  Typically, the command to add a site would be in a script
#' file that is provided to the student that could be run automatically at start
#' up or \code{source}d over the web.  That is, an instructor might create a
#' script file stored on a website and, using a web page, provide students with
#' the text of the command to \code{source} it. 
#' 
#' Currently, https addresses are changed to http
#' 
#' @return a data frame.
#'
#' @examples
#' \dontrun{dome <- fetchData("Dome.csv")}
#' \dontrun{carbon <- fetchData("CO2")}
#' \dontrun{fetchData(show=TRUE)}
#' \dontrun{fetchData(add.to.path="http://www.macalester.edu/~kaplan/ISM/datasets/")}
#' \dontrun{fetchData(drop.from.path="http://www.macalester.edu/~kaplan/ISM/datasets/") }
#' \dontrun{dome <- fetchData("Dome.csv", cache=TRUE)}
#' @keywords util 
#' @export


fetchData <- function(name=NULL,show.path=FALSE, 
                         add.to.path = NULL, drop.from.path = NULL, 
                         cache = FALSE, verbose = TRUE) {
  # Deal with lack of https support
  .Defunct(msg="Use 'fetchData' from the fetch package instead.")
  if( !is.null(name)) name <- .https2http(name)
  # Handle various actions
  if(show.path) return( get("path",envir=.fetchEnvironment))
  if(!is.null(add.to.path)) {
    .fetchStorage(add=.https2http(add.to.path))
    return( fetchData(show.path=TRUE) )
  }
  if(!is.null(drop.from.path)) {
    .fetchStorage(drop=.https2http(drop.from.path))
    return( fetchData(show.path=TRUE) )
  }
  
  ## Get the data
  file.found <- FALSE
  look.for.name <- FALSE
  ## Is it a request to search on the local computer?  
  if (!file.found && is.null(name) ) { 
    name = file.choose()
    look.for.name <- TRUE
  }
  ## What should we do when we find it?
  action <- .fileTypeAction(name)
  
  ## Check the data sets in packages
  if( !file.found ) {
    dataEnv <- new.env()
    suppressWarnings( data(name, envir=dataEnv) )
    if (exists(name, envir=dataEnv)) {
      val = get(name,envir=dataEnv)
      if(verbose) message(paste("Data",name,"found in package."))
      file.found <- TRUE
    }
  }
  
  ## Is the name complete enough to work on its own. 
  if( !file.found ) {
    val <- action(name)
    if( !is.null(val) ) {
      if( !look.for.name ) 
        if(verbose) message("Complete file name given.  No searching necessary.")
      file.found <- TRUE
    }
  }
  
  ## Is it a URL
  if (!file.found && .isURL(name)) {
    val <- action(name)
    file.found <- TRUE
  }
  ## Check the current directory
  if (!file.found) {
    if( length(ls(pattern=name))>0 ) {
      if(verbose) message("File found in current working directory.")
      val <- action(name)
      file.found <- TRUE
    }
  }
  
  ## Check the library
  if( !file.found ) {
    val <- .checkLibrary(name)
    if( !is.null(val) ) {
      file.found <- TRUE
      if(verbose) message("Data found in mosaic cache.")
    }
  }
  ## Look through web sites on the path
  if( !file.found ) {
    path <- fetchData(show.path=TRUE)
    for (k in path) {
      location <- paste(k, name, sep="")
      val <-  action(location)
      if( !is.null(val)) {
        if (verbose) message(paste("Retrieving from", location)) 
        file.found <- TRUE
        break
      }
    }
  }
  

  if( !file.found )
    stop("Can't locate file ",name)
  

  ## Did we want to cache it?
  if (cache & !is.null(val)) {
    .fetchStorage(cache=TRUE, name=name, val=val )
    message("Caching data.")
  }
  
  return(val)
}


.fetchEnvironment <- new.env()
# initialize the storage environment
assign( "data.library", list(), envir=.fetchEnvironment)
assign( "path", c("http://www.mosaic-web.org/go/datasets/"), envir=.fetchEnvironment)

.fetchStorage <- function(...){
  dots <- list(...)
  
  if( "add" %in% names(dots) ) {
    path <- get("path", envir=.fetchEnvironment)
    assign("path", c(path, dots[["add"]]), envir=.fetchEnvironment)
  }
  if( "drop" %in% names(dots) ) {
    path <- get("path", envir=.fetchEnvironment)
    to.be.dropped <- dots[["drop"]]
    ind <- which( to.be.dropped == path )
    if (length(ind) == 0 )
      warning(paste("Path", to.be.dropped, "not found in search path:", paste(path,collapse=":")))
    else {
      path <- path[-ind]
      assign("path", path, envir=.fetchEnvironment)
    }
  }
  if( "cache" %in% names(dots) ) { 
    # cache is true. name contains name, val contains data
    if (!("name" %in% names(dots) && "val" %in% names(dots))) # should never get here
      stop("Must provide arguments <name> and <val> when caching.")
    lib <- get("data.library", envir=.fetchEnvironment)
    lib[[ dots[["name"]] ]] <- dots[["val"]]
    assign("data.library", lib, envir=.fetchEnvironment)
  }
}
.checkLibrary <- function(name){  
  lib <- get("data.library", envir=.fetchEnvironment)
  if( name %in% names(lib) ) return(lib[[name]])
  else return(NULL)
}

.loadWorkspace <- function(name) { stop("Not yet implemented.")}
.read.csv <- function(name) {
  res <- try( suppressWarnings(read.csv( name )), silent=TRUE )
  if( is.null(res) | class(res)=="try-error" ) return(NULL)
  else return(res)
}
.source.file <- function(name){
  res <- try( suppressWarnings(source( name )), silent=TRUE )
  if( is.null(res) | class(res)=="try-error" ) return(NULL)
  else return(TRUE)
}
# What to do with various file types
.fileTypeAction <- function(name) {
  matches <- function(pattern) { length( grep(pattern,name)) > 0 }
  if( matches(".r$|.R$") ) return( .source.file )
  if( matches(".csv$|.CSV$") ) return( .read.csv )
  if( matches(".Rdata$|.rdata$|.rda$|.RDA$")) return( .loadWorkspace )
  return(read.csv) # default
}
.isURL <- function(name) {length( grep("^http|^https|^ftp", name)) > 0}
.https2http <- function(name) sub("^https","http",name)
