#' Read data files
#' 
#' A wrapper around various file reading functions.
#' 
#' @param file character:
#' 	 The name of the file which the data are to be read from. 
#' 	 This may also be a complete URL or a path to a compressed file.
#' 	 If it does not contain an absolute path, the file name is 
#' 	 relative to the current working directory, 
#' 	 \code{getwd()}.  Tilde-expansion is performed where supported. 
#' 	 See \code{\link{read.table}} for more details.
#' 
#' @param header logical;  
#'     For \code{.txt} and \code{.csv} files, this indicates whether the first line of the file includes variables names.
#' 
#' @param na.strings character: strings that indicate missing data.
#' 
#' @param comment.char 
#' character: a character vector of length one containing a single character or an empty string. Use "" to turn 
#' off the interpretation of comments altogether.
#' 
#' @param filetype one of \code{"default"}, \code{"csv"}, \code{"txt"}, or \code{"rdata"}
#' indicating the type of file being loaded.  The default is to use the filename
#' to guess the type of file.
#' 
#' @param \dots  additional arguments passed on to 
#'   \code{\link{read.table}}, or \code{\link{load}} or one of the functions
#'   in the \code{readr} package.  Note that a message will indicate which 
#'   underlying function is being used.
#'   
#' 
#' 
#' @details
#' Unless \code{filetype} is specified,
#' \code{read.file} uses the (case insensitive) file extension to determine how to read
#' data from the file.  If \code{file} ends in \code{.rda} or \code{.rdata}, then
#' \code{\link{load}} is used to load the file.  If \code{file}
#' ends in \code{.csv}, then \code{\link{readr::read_csv}} is used.  Otherwise,
#' \code{\link{read.table}} is used.
#' @param package if specified, files will be search for among the documentation
#' files provided by the package.
#' 
#' @return A data frame, unless \code{file} unless \code{filetype} is \code{"rdata"}, 
#' in which  case arbitrary objects may be loaded and a character vector
#' holding the names of the loaded objects is returned invisibly.
#' @seealso \code{\link{readr::read_table}}, 
#' \code{\link{readr::read_csv}}, 
#' \code{\link{load}}.
#' 
#' @keywords util 
#' @export

read.file <-
function (file, header = T, na.strings = "NA",
    comment.char = NULL, filetype = c("default", "csv", "txt", "tsv", "fw", "rdata"), 
    package=NULL, ...) 
{
  
  
    if (!is.null(package)) {
      file <- docFile(file, package=package, character.only=TRUE)
    }
    filetype <- match.arg(tolower(filetype), choices=filetype)
    if (filetype == "default") {
      filetype <- "txt"
      if (regexpr("\\.csv$", tolower(file)) > 0) {
        filetype <- "csv"
      } 
      if (regexpr("\\.tsv$", tolower(file)) > 0) {
        filetype <- "tsv"
      } 
      if ( regexpr("\\.rdata$", tolower(file)) > 0 || 
            regexpr("\\.rda", tolower(file)) >0 ){
        filetype <- "rdata"
      } 
    }
    
    if( ! filetype=="txt") {
      if (! is.null(comment.char)) message("comment.char is currently being ignored.")
      
      if (length(na.strings) > 1) {
        message("Currently, only the first item in na.strings is used.")
        message("Additional items will be ignored.")
        na.strings = na.strings[1]
      }
    }
    
    if (!file.exists(file) && grepl("https://", file)) {  # assume we are reading a URL
      if (! requireNamespace("RCurl")) stop("Package `RCurl' must be installed.")
      file <- textConnection(RCurl::getURL(file))
    }
    
    if (filetype == "csv") {
      message("Reading data with readr::read_csv()")
      return(as.data.frame(readr::read_csv(file, col_names = header, na = na.strings, ...)))
    }
    
    if (filetype == "fw") {
      message("Reading data with readr::read_table()")
      return(as.data.frame(readr::read_table(file, col_names = header, na = na.strings, ...)))
    }
    
    if (filetype == "tsv") {
      message("Reading data with readr::read_tsv()")
      return(as.data.frame(readr::read_tsv(file, col_names = header, na = na.strings, ...)))
    }
    
    if (filetype == "rdata") {
      message("Reading data with load()")
      varNames <- load(file)  
      return(invisible(varNames))
    }
    
    # fall through to read_table() for any other file format.
    message("Reading data with read.table()")
    return(
      read.table(file, header = header, na.strings = na.strings, stringsAsFactors=FALSE,...)
    )
}
