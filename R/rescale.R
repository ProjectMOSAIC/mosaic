
#' Rescale 
#' 
#' Rescale vectors or variables within data frames.  This can be useful
#' for comparing vectors that are on different scales, for example in
#' parallel plots or heatmaps.
#' @rdname rescale
#' @param x an R object to rescale
#' @param domain a numeric vector of length 2 or `NULL`
#' @param range a numeric vector of length 2
#' @param ... additional arguments
#' @export 
rescale <- function(x, range, domain = NULL, ...) {
  UseMethod("rescale")
}

#' @rdname rescale
#' @export
rescale.data.frame <- function( x, 
                                range = c(0,1), 
                                domain = NULL,
                                ...) {
  dfapply( x, 
           function(x) { rescale(x, range = range, domain = domain, ...) },
           function(x) TRUE ) %>%
    as.data.frame()
}

#' @rdname rescale
#' @export
rescale.factor <- function(x, range, domain = NULL, ...) {
  if( is.null(domain)) domain <- range(1:nlevels(x))
  rescale( as.numeric(x), domain = domain, range = range)
#  width <- diff(range)
#  n <- length(levels(x)) - 1
#  range[1]  - 1/n + width * as.numeric(x) / n
}

#' @rdname rescale
#' @export
rescale.numeric <- function(x, range = c(0,1), domain = NULL, ...) {
  if (is.null(domain)) domain <- range(x, na.rm = TRUE)
  range_width  <- diff(range)
  domain_width <- diff(domain)
  range[1] + range_width * (x - min(x)) / domain_width
}

#' @rdname rescale
#' @export
rescale.default <- function(x, range = c(0,1), domain = NULL, ...) {
  rescale( as.numeric(x, range = range, domain = domain, ...) )
}

#' @rdname rescale
#' @export
rescale.character <- function(x, range=c(0,1), domain = NULL, ...) {
  rescale( as.factor(x), range = range, domain = domain)
}
