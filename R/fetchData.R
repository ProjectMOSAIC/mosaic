#' A Web and Library Data-Loading Facility
#' 
#' \code{fetchData} provides a means for students and others to locate and load data sets provided by instructors.  
#' Data can be pre-loaded for off-line sessions, can be positioned on identified web sites, and can be loaded from packages.  
#' \code{fetchData} also will load local \code{.csv} files using \code{file.choose()}.
#'
#' @name fetchData
#'
#' @param name a character string naming a data set.  
#'    This will often end in \code{.csv} for reading in a data set. When used in conjunction with \code{TRUE} values for the following arguments, it can also name a web directory (always ending in \code{/}). 
#' It can also give a name to a data set to be stored in the cached library.  See
#' 
#' @param add.to.path If \code{TRUE}, indicates that the web search path is to printed out, or, 
#' if \code{name} is specified, the name should be a web directory (ending in \code{/}), which should be pre-pended 
#' to the search path.
#'   
#' @param drop.from.path If \code{TRUE}, wipes out the web search path, or, if \code{name} is specified, removes that 
#' web directory from the search path.
#' @param add.to.library If \code{TRUE}, indicates that a data set is to be pre-loaded into the cached library.  This allows, 
#' for instance, users to pre-load on-line data to be used when they are off-line.
#' @param var The data frame to be put in the cached library if \code{add.to.library=TRUE}.
#' @param directory The name of a web directory to be searched but not added to the search path.
#' 
#' 
#' @details
#' There are two major purposes for this function. One is to provide a
#' consistent interface to reading data: a file name is given and a data frame is
#' returned, which can be assigned to an object as the user desires.  This
#' differs from the behavior of \code{data}, which doesn't return a value but
#' instead creates an object without explicit assignment.
#' 
#' The other purpose is to allow instructors or other group leaders to post data on
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
#' @return a data frame.
#' @author Daniel Kaplan (\email{kaplan@@macalester.edu}) and Randall Pruim (\email{rpruim@@calvin.edu})
#'
# ############## Local Data Storage #########
# internal fetch data function
#'
#' @rdname fetchData
#' @keywords internal
.fetchData.storage.helper <- function( ){
  local.library <- list()
  search.path <- c("http://www.mosaic-web.org/datasets/",
                   "http://www.macalester.edu/~kaplan/ISM/datasets/",
                   "http://dl.dropbox.com/u/5098197/Math155/Data/")
  
  fun <- function(library=FALSE,searchpath=FALSE,val=NULL,name=NULL,action) {
    if( library ) {
      if( action=="add"){ local.library[[name]] <<- val; return(c())}
      if( action=="get"){ return( local.library[[name]] ) }
      if( action=="names"){ return( names(local.library) ) }
    }
    if( searchpath ){
      if( action=="add") {search.path <<- c(name, search.path); return(search.path)}
      if( action=="delete") {
        if( is.null(val) ) search.path <- c()
        else search.path <- search.path[ val != search.path ]
        return(search.path)
      }
      if( action=="get") return(search.path)
    }
    stop("Can use only for the fetchData library and search path.")
  }
  return(fun)
}
# ###############
.fetchData.storage <- .fetchData.storage.helper() # run the function to create .fetchData.storage
#' @export
#' @examples
#' kids <- fetchData("KidsFeet.csv")
#' carbon <- fetchData("CO2")
#' fetchData(add.to.path=TRUE)
#' fetchData(add.to.path=TRUE,name="http://www.macalester.edu/~kaplan/ISM/datasets/")
#' fetchData(drop.from.path=TRUE,name="http://www.macalester.edu/~kaplan/ISM/datasets/") 
#' fetchData(drop.from.path=TRUE) 
#' fetchData(add.to.library=TRUE,name="mydata.csv",var=data.frame(x=c(1,2,3), y=c(7,1,4)))
#' 
#' @keywords mosaic 
#' 


# ############
#' Fetch data sets from internet repositories
#'
#' @export
fetchData <- function(name=NULL,
  add.to.path=FALSE, drop.from.path=FALSE,
  add.to.library=FALSE, directory=NULL, var=NULL){
  # #### load a data set to the local library
  if (add.to.library) {
      if( !is.null(var)) {
        .fetchData.storage(name=name,val=var,library=TRUE,action="add")
      }
      else {
        goo <- fetchData(name)  # get it from the web site
        if (exists("goo") ) .fetchData.storage(name=name,val=goo,library=TRUE,action="add") 
        else warning("Can't find file ", name)
      }
      return(NULL)
  }
  # #### Interface to the search path
  if (add.to.path) {
    if (!is.null(name) )
      .fetchData.storage(searchpath=TRUE,name=name,action="add")
    return(.fetchData.storage(searchpath=TRUE,action="get"))
  }
  if (drop.from.path) { # leaving out name means that path will be emptied.
      return(.fetchData.storage(searchpath=TRUE,name=name,action="delete"))
  }
  # #### Look for file in a local directory.
  if( is.null(name) )
    return( read.csv( file.choose() ))
  # #### Look on web, in library, in packages.         
  if (name %in% .fetchData.storage(library=TRUE,action="names") )
    return( .fetchData.storage(library=TRUE,action="get",name=name) )
  else {
    res <- NULL
    # look for it in the web sites
    web.sites <- c(directory,.fetchData.storage(searchpath=TRUE,action="get"))
    for (k in web.sites) {
      res <- try( suppressWarnings(read.csv( paste(k,name,sep="") )), silent=TRUE)
      if( !(is.null(res) | class(res)=="try-error") ) return(res)
    }
    # If not found on the web search path or in the local library, try packages.
    # This is just for convenience.  Strip .csv from the end of the string if it's there
    foo <- gsub(".csv$|.CSV$","",name)
    suppressWarnings( data(list=c(foo)) )
    if (exists(foo)) return(get(foo)) 
    if (is.null(res) | class(res) == "try-error" )
      stop("Can't locate file ",name )
  }
}
