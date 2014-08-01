
#' Select a subset of columns
#' 
#' Select a subset of columns using \code{dplyr::select}.
#' 
#' @rdname select
# #' @param .data a data frame or object that inherits from class \code{"tbl"}
#' @param ... addtional arguments passed to \code{dplyr::select}.
#' @details
#' Both the \pkg{MASS} and \pkg{dplyr} packages define a generic
#' \code{select}.  When the \pkg{mosaic} package is loaded, 
#' \code{dplyr::select} will be preferred.
#' 
#' @seealso \code{dplyr::\link[dplyr]{select}} 
#' and  \code{MASS::\link[MASS]{select}} 
#' @examples
#' HELPrct %>% sample(5) %>% select(sex, substance, homeless)
#' @export
#' 
select <- function(...) {
  dplyr::select(...)
}