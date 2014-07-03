#' Create factors from logicals
#' 
#' A utility function for creating new factors from logicals describing the levels
#' 
#' @param \dots named logical "rules" defining the levels of the factor.
#' @param .ordered a logical indicating whether the resulting factored should be ordered
#' @param .method one of \code{"unique"}, \code{"first"}, and \code{"last"}.  
#' If \code{"unique"}, exactly one rule must be \code{TRUE} for each position.
#' If \code{"first"}, the first \code{TRUE} rule defines the level.
#' If \code{"last"}, the last \code{TRUE} rule defines the level.
#' @param .debug one of \code{"default"}, \code{"always"}, and \code{"never"}, indicating
#' whehter debugging information should be printed.  If \code{"default"}, debugging 
#' information is printed only when multiple rules give conflicting definitions 
#' for some positions.  
#' @param .sort One of \code{"given"} (the default) or \code{"alpha"} or 
#' a vector of integers the same length as the number of levels indicating the 
#' order in which the levels should appear in the resulting factor.
#' @param .default character vector of length 1 giving name of default level or 
#' \code{NULL} for no default.
#' 
#' @details
#' Each logical "rule" corresponds to a level in the resulting factor.  
#' If \code{.default} is defined, an implicit rule is added that is \code{TRUE} 
#' whenever all other rules are \code{FALSE}.
#' When there are multiple \code{TRUE} rules for a slot, the first or last such is used
#' or an error is generated, depending on the value of \code{method}.   
#' 
#' \code{derivedFactor} is designed to be used with \code{transform} to add new 
#' factor variables to a data frame.  See the examples.
#' 
#' @examples
#' Kf <- transform(KidsFeet, biggerfoot2=derivedFactor(
#'                    dom = biggerfoot == domhand,
#'                    nondom = biggerfoot != domhand)
#'                    )
#' tally( ~biggerfoot + biggerfoot2, data=Kf)
#' tally( ~biggerfoot + domhand, data=Kf)
#' 
#' # Three equivalent ways to define a new variable
#' # Method 1: explicitly define all levels
#' modHELP <- transform(HELPrct, drinkstat = derivedFactor( 
#'   abstinent = i1 == 0,
#'   moderate = (i1>0 & i1<=1 & i2<=3 & sex=='female') |
#'      (i1>0 & i1<=2 & i2<=4 & sex=='male'),
#'   highrisk = ((i1>1 | i2>3) & sex=='female') | 
#'       ((i1>2 | i2>4) & sex=='male'),
#'   .ordered = TRUE)
#' )
#' tally( ~drinkstat, data=modHELP )
#'
#' # Method 2: Use .default for last level
#' modHELP <- transform(HELPrct, drinkstat = derivedFactor( 
#'   abstinent = i1 == 0,
#'   moderate = (i1<=1 & i2<=3 & sex=='female') |
#'      (i1<=2 & i2<=4 & sex=='male'),
#'   .ordered = TRUE,
#'   .method = "first",
#'   .default = "highrisk")
#' )
#' tally( ~drinkstat, data=modHELP )
#' 
#' # Method 3: use TRUE to catch any fall through slots
#' modHELP <- transform(HELPrct, drinkstat = derivedFactor( 
#'   abstinent = i1 == 0,
#'   moderate = (i1<=1 & i2<=3 & sex=='female') |
#'      (i1<=2 & i2<=4 & sex=='male'),
#'   highrisk=TRUE,
#'   .ordered = TRUE,
#'   .method = "first"
#'   )
#' )
#' tally( ~drinkstat, data=modHELP )
#' @export

derivedFactor <- function(..., 
                          .ordered=FALSE, 
                          .method = c("unique","first", "last"),
                          .debug = c("default", "always", "never"),
                          .sort = c("given","alpha"),
                          .default = NULL
) {
  .method <- match.arg( .method )
  .debug <- match.arg( .debug )
  
  if (! is.numeric(.sort) ) {
    .sort <- match.arg( .sort )
  }
  
  rules <- list(...)
  rules <- lapply( rules, as.logical )
  rulesM <- do.call(cbind, rules)
  
  noApplicableRule <- apply( rulesM, 1, function(x) all(!x) )
  undefined <- any( noApplicableRule, na.rm=TRUE )
  
  if (undefined) {
    defaultRule <- list( noApplicableRule )
    names(defaultRule) <- if (is.null(.default)) "other" else .default
    rules <- c(rules, defaultRule)
    rulesM <- do.call(cbind, rules)
  }
  
  lastApplicableRule  <- apply( rulesM, 1, 
                                function(x) max(which(x), na.rm=TRUE) )
  firstApplicableRule <- apply( rulesM, 1, 
                                function(x) min(which(x), na.rm=TRUE) )
  firstApplicableRule[firstApplicableRule == Inf] <- NA
  lastApplicableRule[lastApplicableRule == -Inf] <- NA
  
  multipleDefs <- any( lastApplicableRule != firstApplicableRule, na.rm=TRUE )
  troubles <- (multipleDefs && .method == "unique") || ( undefined && is.null(.default) )
  
  ruleApplied <- switch(.method,
                        "unique" = firstApplicableRule,
                        "first" = firstApplicableRule, 
                        "last" = lastApplicableRule
  )
  
  debug <- (.debug == "always") || (troubles && .debug=="default")
  
  if (debug) {
    cat("\n")
    if (multipleDefs) {
      print(tally(~ firstApplicableRule + lastApplicableRule, useNA="if") )
    } else {
      cat("Rules applied: ")
      print(tally(~ruleApplied, useNA="if"))
    }
    cat("\n")
  }
  
  if (.method == "unique" && any(firstApplicableRule != lastApplicableRule, na.rm=TRUE) ) {
    stop('Conflicting rules.  Do you want to use .method="first" or .method="last"?')
  }
  
  if (undefined && is.null(.default)) {
    stop("There are items with no TRUE rules and no default.")
  }
  
  levels <- names(rules)
  if (length(rules)  < 1) return(NULL)
  
  if (! is.numeric( .sort ) ) {
    order <- switch( .sort,
                     "alpha" = order(names(rules)),
                     "given" = 1:length(names(rules))
    ) 
  } else {
    order <- .sort
  }
  
  factor( names(rules)[ ruleApplied ], 
          ordered=.ordered, 
          levels = names(rules)[order] )
}
