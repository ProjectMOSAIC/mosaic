#' Inspect objects
#' 
#' Print a short summary of the contents of an object.  Most useful as a way to
#' get a quick overview of the variables in data frame.
#' 
#' @export
#' @examples
#' inspect(Births78)
#' 
inspect <- function(object, ...) {
  UseMethod("inspect")
}

#' @export
inspect.default <- function(object, ...) {
  NA
}

#' @export
inspect.character <- function(object, ...) {
  inspect(factor(object)) %>% mutate(class = "character")
}

#' @export
inspect.logical <- function(object, ...) {
  inspect(as.character(object, ...)) %>% mutate(class = "logical")
}

#' @export
inspect.numeric <- function(object, ...) {
  bind_cols(
    data_frame(class = head(class(object),1)),
    favstats(object, ...) 
    )
}

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

#' @export
inspect.data.frame <- function(object, select = TRUE, digits = 3, ...) {
  L <- dfapply(object, inspect, select = select, ...)
  classes <- sapply( L, function(x) if(is.null(x[["class"]])) "" else x[["class"]] ) 
  classes[classes %in% c("numeric", "integer")] <- "quantitative"
  classes[classes %in% c("factor", "ordered", "logical", "character")] <- "categorical"
  classes[classes %in% c("POSIXt", "POSIXct")] <- "time"
  uclasses <- sort(unique(classes))
 
  for (class in uclasses) {
    idx <- which(classes == class)
    cat("\n")
    cat(paste0(class, " variables:  \n"))
    print( bind_cols(data_frame(name = names(L[idx])), bind_rows(L[idx])) %>% as.data.frame(), digits = digits)
  }
}