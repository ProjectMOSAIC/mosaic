tryCatch(utils::globalVariables( c('Var') ),
         error=function(e) message('Looks like you should update R.'))


#' @rdname defunct-fetch
#' @export

fetchGapminder1 <- function(...) {
  .Defunct(msg="Use fetchGapminder1() from the fetch package instead.")
}

#' @rdname defunct-fetch
#' @export
fetchGapminder <- function(...) {
  .Defunct(msg="Use fetchGapminder() from the `fetch' package instead.")
}
