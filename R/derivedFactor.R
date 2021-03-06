#' Create new variables from logicals
#' 
#' Utility functions for creating new variables from logicals describing the levels
#' 
#' @param \dots named logical "rules" defining the levels.
#' @param .method one of `"unique"`, `"first"`, and `"last"`.  
#' If `"unique"`, exactly one rule must be `TRUE` for each position.
#' If `"first"`, the first `TRUE` rule defines the level.
#' If `"last"`, the last `TRUE` rule defines the level.
#' @param .debug one of `"default"`, `"always"`, and `"never"`, indicating
#' whether debugging information should be printed.  If `"default"`, debugging 
#' information is printed only when multiple rules give conflicting definitions 
#' for some positions.  
#' @param .ordered a logical indicating whether the resulting factored should be ordered
#' Ignored if `.asFactor` is `FALSE`.
#' @param .sort One of `"given"` (the default) or `"alpha"` or 
#' a vector of integers the same length as the number of levels indicating the 
#' order in which the levels should appear in the resulting factor. 
#' Ignored if `.asFactor` is `FALSE`.
#' @param .default character vector of length 1 giving name of default level or 
#' `NULL` for no default.
#' @param .asFactor A logical indicating whether the returned value should be a factor.
#' 
#' @details
#' Each logical "rule" corresponds to a level in the resulting variable.  
#' If `.default` is defined, an implicit rule is added that is `TRUE` 
#' whenever all other rules are `FALSE`.
#' When there are multiple `TRUE` rules for a slot, the first or last such is used
#' or an error is generated, depending on the value of `method`.   
#' 
#' `derivedVariable` is designed to be used with [transform()] or 
#' [dplyr::mutate()] to add new 
#' variables to a data frame.  `derivedFactor`() is the same but that the 
#' default value for `.asFactor` is `TRUE`.  See the examples.
#' 
#' @examples
#' Kf <- mutate(KidsFeet, biggerfoot2 = derivedFactor(
#'                    dom = biggerfoot == domhand,
#'                    nondom = biggerfoot != domhand)
#'                    )
#' tally( ~ biggerfoot + biggerfoot2, data = Kf)
#' tally( ~ biggerfoot + domhand, data = Kf)
#' 
#' # Three equivalent ways to define a new variable
#' # Method 1: explicitly define all levels
#' modHELP <- mutate(HELPrct, drink_status = derivedFactor( 
#'   abstinent = i1 == 0,
#'   moderate = (i1>0 & i1<=1 & i2<=3 & sex=='female') |
#'      (i1>0 & i1<=2 & i2<=4 & sex=='male'),
#'   highrisk = ((i1>1 | i2>3) & sex=='female') | 
#'       ((i1>2 | i2>4) & sex=='male'),
#'   .ordered = TRUE)
#' )
#' tally( ~ drink_status, data = modHELP)
#'
#' # Method 2: Use .default for last level
#' modHELP <- mutate(HELPrct, drink_status = derivedFactor( 
#'   abstinent = i1 == 0,
#'   moderate = (i1<=1 & i2<=3 & sex=='female') |
#'      (i1<=2 & i2<=4 & sex=='male'),
#'   .ordered = TRUE,
#'   .method = "first",
#'   .default = "highrisk")
#' )
#' tally( ~ drink_status, data = modHELP)
#' 
#' # Method 3: use TRUE to catch any fall through slots
#' modHELP <- mutate(HELPrct, drink_status = derivedFactor( 
#'   abstinent = i1 == 0,
#'   moderate = (i1<=1 & i2<=3 & sex=='female') |
#'      (i1<=2 & i2<=4 & sex=='male'),
#'   highrisk=TRUE,
#'   .ordered = TRUE,
#'   .method = "first"
#'   )
#' )
#' tally( ~ drink_status, data = modHELP)
#' is.factor(modHELP$drink_status)
#' 
#' modHELP <- mutate(HELPrct, drink_status = derivedVariable( 
#'   abstinent = i1 == 0,
#'   moderate = (i1<=1 & i2<=3 & sex=='female') |
#'      (i1<=2 & i2<=4 & sex=='male'),
#'   highrisk=TRUE,
#'   .ordered = TRUE,
#'   .method = "first"
#'   )
#' )
#' is.factor(modHELP$drink_status)
#' @export

derivedVariable <- function(..., 
                          .ordered=FALSE, 
                          .method = c("unique","first", "last"),
                          .debug = c("default", "always", "never"),
                          .sort = c("given","alpha"),
                          .default = NULL,
                          .asFactor = FALSE
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
  
  lastApplicableRule  <- 
    apply( rulesM, 1, 
           function(x) if (all(is.na(x))) NA else max(which(x), na.rm=TRUE) )
  firstApplicableRule <- 
    apply( rulesM, 1, 
           function(x) if (all(is.na(x))) NA else min(which(x), na.rm=TRUE) )
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
 
  if (.asFactor) 
    factor( names(rules)[ ruleApplied ], 
            ordered=.ordered, 
            levels = names(rules)[order] )
  else 
    names(rules)[ruleApplied] 
}

#' @rdname derivedVariable
#' @export
#' 
derivedFactor <- function(..., .asFactor = TRUE) {
  derivedVariable(..., .asFactor = .asFactor)
}
