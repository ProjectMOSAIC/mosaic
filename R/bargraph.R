#' Create bar graphs from raw data
#'
#' [lattice::barchart()] from the `lattice` package makes bar graphs from
#' pre-tabulated data.  Raw data can be tabulated using [xtabs()], but the syntax
#' is unusual compared to the other lattice plotting functions.  `bargraph` provides 
#' an interface that is consistent with the other `lattice` functions.
#'
#' @param x a formula describing the plot
#' @param data a data frame in which the formula `x` is evaluated
#' @param groups a variable or expression used for grouping.  See [lattice::barchart()].
#' @param horizontal a logical indicating whether bars should be horizontal
#' @param \dots additional arguments passed to [lattice::barchart()]
#' @param origin beginning point for bars.  For the default behavior used by 
#'        [lattice::barchart()] set `origin` to `NULL`, but
#'         0 is often a better default. If 0 is not good, perhaps you should use
#'        a different kind of plot as the results may be misleading.
# #' @param subset a vector used to subset \code{data}.  This may be an expression that
# #' will be evaluated within \code{data}.
#' @param ylab a character vector of length one used for the y-axis label
#' @param xlab a character vector of length one used for the x-axis label
#' @param type one of `"frequency"`, `"count"`, `"percent"`, or `"proportion"`
#'   indicating what type of scale to use.  Unique prefixes are sufficient.
#' @param auto.key a logical expression indicating whether a legend should be automatically produced
#' @param scales is a list determining how the x- and y-axes are drawn
#' @return a trellis object describing the plot
#' @seealso [lattice::barchart()]
#' @details `bargraph(formula, data=data, ...)` works by creating a new data frame from `xtabs(formula, data=data)`
#' and then calling [lattice::barchart()] using modified version of the formula and this
#' new data frame as inputs.  This has implications on, for example, conditional plots where 
#' one desires to condition on some expression that will be evaluated in `data`.  This typically
#' does not work because the required variables do not exist in the output of `xtabs`.  One solution
#' is to first add a new variable to `data` first and then to condition using this new variable.
#' See the examples.
#'
#' @examples
#' if (require(mosaicData)) {
#' data(HELPrct)
#' bargraph( ~ substance, data = HELPrct)
#' bargraph( ~ substance, data = HELPrct, horizontal = TRUE)
#' bargraph( ~ substance | sex, groups = homeless, auto.key = TRUE, data = HELPrct)
#' bargraph( ~ substance, groups = homeless, auto.key=TRUE, 
#'             data = HELPrct %>% filter(sex == "male"))
#' HELPrct2 <- mutate(HELPrct, older = age > 40)
#' bargraph( ~ substance | older, data = HELPrct2)
#' }
#' @export

bargraph <- function(x, data = parent.frame(), groups = NULL, horizontal = FALSE, origin = 0, 
                     ylab = ifelse(horizontal, "", type), 
                     xlab = ifelse(horizontal, type, ""), 
                     type = c("count", "frequency", "proportion", "percent"),
                     auto.key = TRUE,
                     scales = list(),
                     ...) {
  type <- match.arg(type)
  if (type == "frequency") type <- "count"
  
  if (!inherits(x, "formula") || !is.null(lhs(x)))
    stop("first argument should be a formula with no lhs.")
  
  xtab0 <- tally(x, data = data, groups = groups, format = type, groups.first = TRUE)
  
  xtab <- as.data.frame(xtab0) 
  # grab the last variable name, to handle the case that 
  # there is a variable called "Freq"
  lastvar <- names(xtab)[ncol(xtab)]
  # add dummy variable to handle case when 
  xtab <- xtab %>% dplyr::mutate(..X.. = xtab[, 1])
  sgroups <- substitute(groups)
  
  
  
  if (horizontal) {
    if (! is.null(condition(x))){
      formula <- as.formula( paste("..X.. ~ ", lastvar, " | ", deparse(condition(x)) ) )
    } else {
      formula <- as.formula( paste("..X.. ~ ", lastvar) )
    }
  } else {
    if (! is.null(condition(x))){
      formula <- as.formula(paste(lastvar, " ~ ..X.. | ", deparse(condition(x))))
    } else {
      formula <- as.formula(paste(lastvar, " ~ ..X..")) 
    }
    if(length(scales) == 0 ){
      scales = list(x = list(rot = 30))
    }
  }
  if (xlab == "" && ! horizontal) { xlab <- names(dimnames(xtab0))[1] }
  if (ylab == "" &&   horizontal) { ylab <- names(dimnames(xtab0))[1] }
  
  barchart(formula, data = xtab, groups = eval(sgroups), 
           origin=origin, ylab = ylab, xlab = xlab, auto.key = auto.key, scales=scales, ...)
}
