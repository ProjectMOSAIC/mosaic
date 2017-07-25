
#' Compute all proportions or counts
#' 
#' Compute vector of counts, proportions, or percents for each unique value (and \code{NA} if there
#' is missing data) in a vector.
#' 
#' @param x A vector or a formula.
#' @param format One of \code{"count"}, \code{"proportion"}, or \code{"percent"}.  May be abbreviated.
#' @param data A data frame.
#' @param ... Arguments passed to methods.
#' 
#' @seealso \code{\link{prop}()}
#' @seealso \code{\link{count}()}
#' @examples
#' props(HELPrct$substance)
#' # Formula version removes missing data (for now, may change this in future versions)
#' props(HELPmiss$link)
#' props( ~ link, data = HELPmiss)
#' tally( ~ link, data = HELPmiss, format = "prop")
#' props( ~ substance | sex, data = HELPrct)
#' props( ~ substance | sex, data = HELPrct, format = "percent")
#' counts( ~ substance | sex, data = HELPrct)
#' if (require(ggformula)) {
#'   df_stats( ~ substance | sex, data = HELPrct, props, counts)
#' }
#' 
#' @export
#' @rdname props

counts <- function(x, ...) {
  UseMethod("counts")
}

#' @rdname props
#' @export
 
counts.default <- 
  function(x, ..., format = c("count", "proportion", "percent")) {
    format = match.arg(format)
    uval <- sort(unique(x))
    
    res <- sapply(uval, function(v) base::sum(x == v, na.rm = TRUE))
    names (res) <- 
      paste0(
        switch(format, count = "n_", proportion = "prop_", percent = "perc_"), 
        as.character(uval)
      )
    
    n_missing <- base::sum(is.na(x), na.rm = TRUE)
    if (n_missing > 0L) {
      names(n_missing) <-
        switch(format, count = "n_NA", proportion = "prop_NA", percent = "perc_NA") 
      res <- c(res, n_missing)
    }
    # do arithmetic to convert to proportions or percents, and return result
    switch(
      format,
      count = res,
      proportion =   res / length(x),
      percent =  100 * res / length(x)
    )
  }

#' @rdname props
#' @export
counts.formula <- function(x, data, ..., format = "count") {
  mosaicCore::df_stats(x, data = data, "counts", fargs = list(format = format))
}

#' @rdname props
#' @export
props <- function(x, ..., format = "proportion") { 
  counts(x, format = "proportion", ...)
}

#' @rdname props
#' @export
percs <- function(x, ..., format = "percent") { 
  counts(x, ..., format = "percent")
}
