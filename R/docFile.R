
#' Return the path to a documentation file in a package
#' 
#' Return the path to a documentation file in a package
#' 
#' @param file the name of a file
#' @param package the name of a package
#' @return a character vector specifying the path to the file on the user's system.
#' @export
#' @examples
#' MustangPrice <- read.file(docFile("MustangPrice.csv", "mosaic"))

docFile <- function(file, package="mosaic", character.only=FALSE) {
  if (! character.only)  package <- as.character(substitute(package))
  path <- find.package(package, verbose=FALSE)
  return(file.path(path,"doc",file))
}