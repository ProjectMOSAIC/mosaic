#' Inspect objects
#' 
#' Print a short summary of the contents of an object.  Most useful as a way to
#' get a quick overview of the variables in data frame.
#' 
#' @param object a data frame or a vector
#' @param additional arguments passed along to methods
#' @param max.level an integer giving the depth to which lists should be expanded
#' @param digits and integer giving the number of digits to display
#' @param select a logical, character (naming variables), or numeric vector or a 
#'   function used to select variables to which \code{FUN} is applied.  If a function,
#'   it should take a vector as input and return a single logical. See examples here and
#'   at \code{link{dfapply}}.
#'   
#' @export
#' @examples
#' inspect(Births78)
#' inspect(Births78, is.numeric)
 
inspect <- function(object, ...) {
  UseMethod("inspect")
}

#' @rdname inspect
#' @export
inspect.list <- function(object, max.level = 2, ...) {
  str(object, max.level = max.level, ...)
}

#' @rdname inspect
#' @export
inspect.character <- function(object, ...) {
  inspect(factor(object)) %>% mutate(class = "character")
}

#' @rdname inspect
#' @export
inspect.logical <- function(object, ...) {
  inspect(as.character(object, ...)) %>% mutate(class = "logical")
}

#' @rdname inspect
#' @export
inspect.numeric <- function(object, ...) {
  bind_cols(
    data_frame(class = head(class(object),1)),
    favstats(object, ...) 
    )
}

#' @rdname inspect
#' @export
inspect.factor <- function(object, ...) {
  tbl <- sort(table(object), decreasing = TRUE) 
  p <- round(100 * tbl / base::sum(tbl, na.rm = TRUE), 1)
  lns <- nchar(names(p))
  idx <- which(cumsum(lns + 10) <= 40)
  popular <- paste(names(p[idx]), ' (', p[idx], "%)", sep = "", collapse = ", ")
  if (length(idx) < length(p)) popular <- paste(popular, "...")
  popular <- sprintf("%-45s", popular) 
  
  data_frame(
    class = head(class(object),1),
    levels = length(levels(object)),
    n = length(object) - n_missing(object),
    missing = n_missing(object),
    distribution = popular
  )
}

#' @rdname inspect
#' @export
inspect.POSIXt <- function(object, ...) {
  data_frame(
    class = head(class(object),1),
    first = min(object),
    last = max(object),
    min_diff = min(diff(sort(object))),
    max_diff = max(diff(sort(object))),
    n = length(object) - n_missing(object),
    missing = n_missing(object)
  )
}

#' @rdname inspect
#' @export
inspect.data.frame <- function(object, select = TRUE, digits = getOption("digits", 3), ...) {
  L <- dfapply(object, inspect, select = select, ...)
  classes <- sapply( L, function(x) if(is.null(x[["class"]])) "" else x[["class"]] ) 
  classes[classes %in% c("numeric", "integer")] <- "quantitative"
  classes[classes %in% c("factor", "ordered", "logical", "character")] <- "categorical"
  classes[classes %in% c("POSIXt", "POSIXct")] <- "time"
  uclasses <- sort(unique(classes))
  res <- list()
  for (class in uclasses) {
    idx <- which(classes == class)
    res[[class]] <- 
    bind_cols(data_frame(name = names(L[idx])), bind_rows(L[idx]))
  }
  
  structure(res, class = "inspected_data_frame", digits = digits)
}

#' @rdname inspect
#'@export
print.inspected_data_frame <- function(x, digits = NULL, ...) {
  if (is.null(digits)) digits <- attr(x, "digits")
  for (n in names(x)) {
    cat("\n")
    cat(paste0(n, " variables:  \n"))
    print(as.data.frame(x[[n]]), digits = digits)
  }
  invisible(x)
}