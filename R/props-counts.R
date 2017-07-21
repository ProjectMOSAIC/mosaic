
#' Compute all proportions or counts
#' 
#' These wrappers around tally make it easy to compute proportions
#' and counts.
#' 
#' @inheritParams tally
#' 
#' @seealso \code{\link{prop}()}
#' @seealso \code{\link{count}()}
#' @examples
#' props(HELPrct$substance)
#' props( ~ substance, data = HELPrct)
#' props( ~ substance | sex, data = HELPrct)
#' props( ~ substance | sex, data = HELPrct, format = "percent")
#' counts( ~ substance | sex, data = HELPrct)
#' if (require(ggformula)) {
#'   df_stats( ~ substance | sex, data = HELPrct, props, stats, format = "long")
#' }
#' @export
#' @rdname props

props <- function(x, format = "proportion", ...) { 
  res <- tally(x, ..., format = format)
  names(res) <- paste0("prop_", names(res))
  res
}

#' @rdname props
#' @export
counts <- function(x, format = "count", ...) { 
  res <- tally(x, ..., format = format)
  names(res) <- paste0("num_", names(res))
  res
}
