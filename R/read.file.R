#' Read data files
#' 
#' A wrapper around 
#' \code{\link{read.table}}, \code{\link{read.csv}}, and \code{\link{load}}
#' to unify and simply reading data from files.
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
#'   \code{\link{read.table}}, \code{\link{read.csv}}, or \code{\link{load}}.
#' 
#' 
#' @details
#' Unless \code{filetype} is specified,
#' \code{read.file} uses the (case insensitive) file extension to determine how to read
#' data from the file.  If \code{file} ends in \code{.rda} or \code{.rdata}, then
#' \code{\link{load}} is used to load the file.  If \code{file}
#' ends in \code{.csv}, then \code{\link{read.csv}} is used.  Otherwise,
#' \code{\link{read.table}} is used.
#' 
#' @return A data frame, unless \code{file} unless \code{filetype} is \code{"rdata"}, 
#' in which  case arbitrary objects may be loaded and a character vector
#' holding the names of the loaded objects is returned invisibly.
#' @seealso \code{\link{read.table}}, 
#' \code{\link{read.csv}}, 
#' \code{\link{load}}.
#' 
#' @keywords util 
#' @export

read.file <-
function (file, header = T, na.strings = c("NA", "", ".", "na", 
    "-"), comment.char = "#", filetype = c("default", "csv", "txt", "rdata"), ...) 
{

    filetype <- match.arg(tolower(filetype), choices=filetype)
    if (filetype == "default") {
      filetype <- "txt"
      if (regexpr("\\.csv$", tolower(file)) > 0) {
        filetype <- "csv"
      } 
      if ( regexpr("\\.rdata$", tolower(file)) > 0 || 
            regexpr("\\.rda", tolower(file)) >0 ){
        filetype <- "rdata"
      } 
    }
    
    if (!file.exists(file) && grepl("https://", file)) {  # assume we are reading a URL
      if (! require(RCurl) ) {
        stop("The `RCurl' package is required to download via https")
      }
      file <- textConnection(getURL(file))
    }

    
    if (filetype == "csv") 
      return(read.csv(file, header = header, na.strings = na.strings, 
                      comment.char = comment.char, ...))
    
    if (filetype == "rdata") {
      varNames <- load(file)  
      return(invisible(varNames))
    }

    # fall through to read.table() for any other file format.
    return(read.table(file, header = header, na.strings = na.strings, 
                      comment.char = comment.char, ...))
}
