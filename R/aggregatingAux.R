utils::globalVariables(c('.'))

#' Convert formulas into standard shapes
#' 
#' These functions convert formauls into standard shapes, including by 
#' incorporating a groups argument.  
#' @rdname mosaicformula
#' @param formula a formula
#' @param groups a name used for grouping
#' @param max.slots an integer specifying the maximum number of slots for the resulting formula.  
#' An error results from trying to create a formula that is too complex.
#' @param envir the environment in which the resulting formula may be evaluated. 
#' May also be \code{NULL}, a list, a data frame, or a pairlist.
#' @param groups.first a logical indicating whether groups should be inserted 
#' ahead of the condition (else after).
#' 
#' @details
#' \code{mosaic_formula_q} uses nonstandard evaluation of \code{groups} that may be
#' necessary for use within other functions.  \code{mosaic_formula} is a wrapper
#' around \code{mosaic_formula_q} and quotes \code{groups} before passing it along. 
#' @examples
#' mosaic_formula( ~ x | z )
#' mosaic_formula( ~ x, groups=g ) 
#' mosaic_formula( y ~ x, groups=g )
#' # this is probably not what you want for interactive use.
#' mosaic_formula_q( y ~ x, groups=g )
#' # but it is for programming
#' foo <- function(x, groups=NULL) {
#'     mosaic_formula_q(x, groups=groups, envir=parent.frame())
#' }
#' foo( y ~ x , groups = g)
#' @export

mosaic_formula <- function( 
  formula, 
  groups=NULL, 
  envir=parent.frame(),
  max.slots=3,
  groups.first = FALSE
  ) 
{
  mosaic_formula_q( 
    formula=formula,
    groups=quote(groups),
    envir=envir,
    max.slots=max.slots,
    groups.first = groups.first)
}

#' Convert lazy objects into formulas
#' 
#' Convert lazy objects into a formula
#' 
#' @param lazy_formula an object of class \code{lazy}
#' @param envir an environment that will be come the environment of the returned formula
#' @return a formula
#' @details The expression of the lazy object is evaluated in its environment.  If the
#' result is not a formula, then the formula is created with an empty left hand side
#' and the expression on the right hand side.
#' 
#' @examples
#' formularise(lazyeval::lazy(foo))
#' formularise(lazyeval::lazy(y ~ x))
#' bar <- a ~ b
#' formularise(lazyeval::lazy(bar))
#' @export
 
formularise <- function(lazy_formula, envir = parent.frame()) {
  safe_formula <- 
    tryCatch(eval(lazy_formula$expr, lazy_formula$env),
             error = function(e) e)
  
  new_formula <- ~ placeholder
  new_formula[[2]] <- lazy_formula$expr
  environment(new_formula) <- envir  # lazy_formula$env
  
  if (inherits(safe_formula, "formula")) 
    safe_formula else new_formula
}


#' @rdname mosaicformula
#' @param ... additional arguments (currently ignored)
#' @export
mosaic_formula_q <- function( formula, 
                              groups = NULL, 
                              # envir = parent.frame(),
                              max.slots = 3,
                              groups.first = FALSE,
                              ...
) {
  lazy_groups <- lazyeval::lazy(groups)
  
  slots <- alist()
  if (groups.first) {
    slots <- c(slots, 
               lhs(formula), rhs(formula), 
               lazy_groups$expr, condition(formula))
  } else { 
    slots <- c(slots, 
               lhs(formula), rhs(formula), 
               condition(formula), lazy_groups$expr)
  }
  
  if (length(slots) > max.slots) {
    stop("Invalid formula specification.  Too many slots (",  
         length(slots), ">", max.slots, ").")
    return(NULL)
  }
  
  if (length(slots) == 1) {
    res <- ~ x
    res[[2]] <- slots[[1]]
  } else if (length(slots)==2) {
    res <- y ~ x 
    res[[2]] <- slots[[1]]
    res[[3]] <- slots[[2]]
  } else if (length(slots)==3) {
    res <- y ~ x | z
    res[[2]] <- slots[[1]]
    res[[3]][[2]] <- slots[[2]]
    res[[3]][[3]] <- slots[[3]]
  } else {
    res <- formula
  }
  environment(res) <- environment(formula)
  res
}


# evaluate a lazy object and return the unevaluated expression if the expression
# doesn't evaluate to an existing object.

safe_eval <- function(x) {
  tryCatch(lazyeval::lazy_eval(x), 
           error = function(e) x$expr)
}


.fetchFromDots <- function( dots, name, class='data.frame', n=1, default=NULL ) {
  result <- dots[[name]]
  if (is.null(result)) {
    if (length(result) < n) return(default)
    result <- dots[[n]]
    if (! inherits(result, 'class') ) result <- default
  }
  return(result)
}

#' Check if formula
#' 
#' @param x an object
#' @return TRUE for a formula, FALSE otherwise, even if evaluation throws an error
#'
#' @rdname mosaic-internal
#' @keywords internal

.is.formula <- function(x)  
  tryCatch( inherits(x, 'formula'), error = function(e) {FALSE} )

#' Check for simple formula
#'
#' @param x a formula
#'
#' @return TRUE if formula has no left-hand side or a simple right-hand side 
#' (e.g., \code{NULL}, ., 1,  or 0)
#'
#' @rdname mosaic-internal
#' @keywords internal
.is.simple.formula <-  function(x){
  inherits(x, "formula") &&
    (length(x)==2 || is.null(x[[3]]) ||
       (length(x[[3]])==1 &&
          ((is.numeric(x[[3]]) && (x[[3]]==0 || x[[3]]==1)) ||  (all.names(x[[3]]) %in% c(".")))))
}

# This could use a better name and a better desription



#' Extract simple part from formula
#'
#' @param x a formula
#'
#' @return simple part of formula or NULL if formula is not simple
#'
#' @rdname mosaic-internal
#' @keywords internal

.simple.part <- function(x) {
  if (! .is.simple.formula(x) ) {
    return(NULL) 
  } else {
    return(x[[2]])
  }
}

.flatten <- function(x) {
  result <- c()
  for (item in x) result <- c(result, item)
  return(result)
}


#' Aggregate for mosaic
#'
#' Compute function on subsets of a variable in a data frame.
#'
#' @rdname aggregatingAux
#' @return  a vector
#' @param formula a formula.  Left side provides variable to be summarized.  Right side and condition
#'                            describe subsets.  If the left side is empty, right side and condition are
#'                            shifted over as a convenience.
#' @param data a data frame.  
#' Note that the default is \code{data=parent.frame()}.  This makes it convenient to
#' use this function interactively by treating the working envionment as if it were 
#' a data frame.  But this may not be appropriate for programming uses.  
#' When programming, it is best to use an explicit \code{data} argument
#' -- ideally supplying a data frame that contains the variables mentioned
#' in \code{formula}.
#' @param FUN a function to apply to each subset 
#' @param groups grouping variable that will be folded into the formula (if there is room for it).  
#' This offers some additional flexibility in how formulas can be specified.
#' @param subset a logical indicating a subset of \code{data} to be processed.
#' @param drop a logical indicating whether unused levels should be dropped.
#' @param \dots additional arguments passed to \code{FUN}
#' @param .format format used for aggregation. \code{"default"} and \code{"flat"} are equivalent.  
#' @param .overall currently unused
#' @param .name a name used for the resulting object
#' @param .envir an environment in which to evaluate expressions 
#' @param .multiple a logical indicating whether FUN returns multiple values
#'
#' @examples
#' if (require(mosaicData)) {
#' maggregate( cesd ~ sex, HELPrct, FUN=mean )
#' # using groups instead
#' maggregate( ~ cesd, groups = sex, HELPrct, FUN=sd )
#' # the next four all do the same thing
#' maggregate( cesd ~ sex + homeless, HELPrct, FUN=mean )
#' maggregate( cesd ~ sex | homeless, HELPrct, FUN=sd )
#' maggregate( ~ cesd | sex , groups= homeless, HELPrct, FUN=sd )
#' maggregate( cesd ~ sex, groups = homeless, HELPrct, FUN=sd )
#' # this is unusual, but also works.
#' maggregate( cesd ~ NULL , groups = sex, HELPrct, FUN=sd )
#' }
#'
#' @export
maggregate <- 
  function(
    formula, 
    data=parent.frame(), 
    FUN, 
    groups=NULL, 
    subset, 
    drop=FALSE, 
    ...,
    .format=c('default', 'table', 'flat'), 
    .overall=mosaic.par.get("aggregate.overall"), 
    .multiple=FALSE, 
    .name = deparse(substitute(FUN)), 
    .envir = parent.frame () # if (is.list(data) || is.pairlist(data)) parent.frame() else baseenv() 
    ) {
 
  if (! inherits(data, c("environment", "data.frame")) ) {
    if (inherits(data, c("tbl")))
      stop ("Your tbl is not a data.frame.  Perhaps you need dplyr functions here.",
            call. = FALSE)
    else 
      stop("data must be an environment or data.frame.", call. = FALSE)
  }
      
  formula <- mosaic_formula_q(formula, groups = groups, envir = .envir)
  
  if (length(formula) == 2) { 
    return(FUN( eval(formula[[2]], data, .envir), ...))
  }
  
  dots <- list(...)
  groupName <- ".group"  # gets changed to something better later when possible.
  
  .format <- match.arg(.format)
  evalF <- evalFormula(formula, data=data)
  
  if (!missing(subset)) {
    subset <- eval(substitute(subset), data, environment(formula))
    if (!is.null(evalF$left))           evalF$left <- evalF$left[subset,]
    if (!is.null(evalF$right))         evalF$right <- evalF$right[subset,]
    if (!is.null(evalF$condition)) evalF$condition <- evalF$condition[subset,]
  }
  # this should now be standardized by the call to mosaic_formula_q() above.
  #  if ( is.null( evalF$left ) ) {
  #    evalF$left <- evalF$right
  #    evalF$right <- evalF$condition
  #    evalF$condition <- NULL
  #  }
  
  if ( is.null(evalF$left) || ncol(evalF$left) < 1 )  {
    if (ncol(evalF$right) > 1) warning("Too many variables in rhs; ignoring all but first.")
    if (.format=="table") {
      if (.multiple) stop ("table view unavailable for this function.")
      ldata <- evalF$right[,1,drop=FALSE]
      gdata <- group_by(data)
      res <- as.data.frame(
        dplyr::do(gdata, foo = FUN( as.data.frame(.)[, 1], ...) ) )
      names(res)[ncol(res)] <- gsub(".*::", "", .name)
      return(res)
      
      return(evalF$right[,1,drop=FALSE] %>% 
               group_by() %>%
               dplyr::do( do.call(FUN, list(evalF$right[,1], ...)) ) %>%
               as.data.frame()
      )
      #      return(plyr::ddply(evalF$right[,1,drop=FALSE], names(NULL),
      #                   function(x) do.call(FUN, list(evalF$right[,1], ...)) 
      #      )[,-1])  # remove the .id column since it is uninteresting here.
    }
    return( do.call(FUN, alist(evalF$right[,1], ...) ) )
  } else {
    if (ncol(evalF$left) > 1) warning("Too many variables in lhs; ignoring all but first.")
    if (.format=='table') {
      if (.multiple) stop ("table view unavailable for this function.")
      ldata <- joinFrames(evalF$left[,1,drop=FALSE], evalF$right, evalF$condition) 
      ldata$.var <- ldata[, 1]
      gdata <- do.call( group_by, c(list(ldata),  
                                    lapply(union(names(evalF$right), names(evalF$condition)),
                                           as.name )) )
      res <- as.data.frame(
        dplyr::do(gdata, foo = FUN( as.data.frame(.)[, 1], ...) ) )
      names(res)[ncol(res)] <- gsub(".*::", "", .name)
      #      res <-  plyr::ddply( 
      #        joinFrames(evalF$left[,1,drop=FALSE], evalF$right, evalF$condition), 
      #        union(names(evalF$right), names(evalF$condition)),
      #        function(x) do.call(FUN, list(x[,1], ...))
      #      )
      
    } else {
      res <- lapply( split( evalF$left[, 1], 
                            joinFrames(evalF$right, evalF$condition), 
                            drop=drop),
                     function(x) { do.call(FUN, alist(x, ...) ) }
      )
      groupName <- paste(c(names(evalF$right), names(evalF$condition)), collapse=".")
      
      if (! .multiple ) res <- unlist(res)
      
      if (! is.null(evalF$condition) ) {
        if (ncol(evalF$left) > 1) message("Too many variables in lhs; ignoring all but first.")
        res2 <- lapply( split( evalF$left[, 1], evalF$condition, drop=drop),
                        function(x) { do.call(FUN, alist(x, ...) ) }
        )
        groupName <- paste(names(evalF$condition), collapse=".")
        if (!.multiple) {
          res <- c( res, unlist(res2) )
        } else {
          res <- c(res, res2)
        }
      }
      if (.multiple) {
        result <- res
        res <- result[[1]]
        for (item in result[-1]) {
          res <- as.data.frame(rbind(res,item))
        }
        if ( nrow(res) == length(names(result)) ) {
          res[groupName] <- names(result)
        } else {
          res[groupName] <- rep(names(result), each=nrow(res) / length(names(result)) )
        }
        res <- res[, c(ncol(res), 1:(ncol(res) -1))]
      }
    }
  }
  w <- grep("V[[:digit:]]+",  names(res))
  if (length(w) == 1) {
    names(res)[w] <- gsub(".*:{2,3}", "", .name)
  } else {
    names(res)[w] <- paste0( gsub(".*:{2,3}", "", .name), 1:length(w) )
  }
  row.names(res) <- NULL
  return( res )
}

# for hanlding functions of two inputs
# under construction still

maggregate2 <- function(formula, data=parent.frame(), FUN, subset, 
                        overall=mosaic.par.get("aggregate.overall"), 
                        .format=c('default', 'table', 'flat'), drop=FALSE, 
                        .multiple=FALSE, 
                        groups=NULL, 
                        .name = deparse(substitute(FUN)), 
                        ...) {
  dots <- list(...)
  formula <- mosaic_formula_q(formula, groups=groups, as.environment(data))
  
  .format <- match.arg(.format)
  
  evalF <- evalFormula(formula, data=data)
  
  if (!missing(subset)) {
    subset <- eval(substitute(subset), data, environment(formula))
    if (!is.null(evalF$left))           evalF$left <- evalF$left[subset,]
    if (!is.null(evalF$right))         evalF$right <- evalF$right[subset,]
    if (!is.null(evalF$condition)) evalF$condition <- evalF$condition[subset,]
  }
  # this should now be standardized by the call to mosaic_formula_q() above.
  #  if ( is.null( evalF$left ) ) {
  #    evalF$left <- evalF$right
  #    evalF$right <- evalF$condition
  #    evalF$condition <- NULL
  #  }
  
  if ( is.null(evalF$left) || ncol(evalF$left) < 1 )  
    stop("formula must have lhs.")
  
  if (ncol(evalF$left) > 1) stop("Too many variables in lhs.")
  
  if (.format=='table') {
    if (.multiple) stop ("table view unavailable.")
    ldata <- joinFrames(evalF$left[,1,drop=FALSE], evalF$right, evalF$condition) 
    ldata$.var <- ldata[, 1]
    gdata <- do.call( group_by, c(list(ldata),  as.name(names(evalF$condition)) ) )
    res <- as.data.frame( dplyr::do(gdata, foo = FUN( as.data.frame(.)[,1], as.data.frame(.)[,2], ...) ) )
    names(res)[ncol(res)] <- gsub(".*::", "", .name)
  } else {
    res <- lapply( split( evalF$left[,1], 
                          joinFrames(evalF$right, evalF$condition), 
                          drop=drop),
                   function(x) { do.call(FUN, alist(x, ...) ) }
    )
    groupName <- paste(c(names(evalF$right), names(evalF$condition)), collapse=".")
    
    if (! .multiple ) res <- unlist(res)
    
    if (! is.null(evalF$condition) ) {
      if (ncol(evalF$left) > 1) message("Too many variables in lhs; ignoring all but first.")
      res2 <- lapply( split( evalF$left[,1], evalF$condition, drop=drop),
                      function(x) { do.call(FUN, alist(x, ...) ) }
      )
      groupName <- paste(names(evalF$condition), collapse=".")
      if (!.multiple) {
        res <- c( res , unlist(res2) )
      } else {
        res <- c(res, res2)
      }
    }
    if (.multiple) {
      result <- res
      res <- result[[1]]
      for (item in result[-1]) {
        res <- as.data.frame(rbind(res,item))
      }
      if ( nrow(res) == length(names(result)) ) {
        res[groupName] <- names(result)
      } else {
        res[groupName] <- rep(names(result), each=nrow(res) / length(names(result)) )
      }
      res <- res[, c(ncol(res), 1:(ncol(res) -1))]
    }
  }
  
  w <- grep("V[[:digit:]]+",  names(res))
  if (length(w) == 1) {
    names(res)[w] <- gsub(".*:{2,3}", "", .name)
  } else {
    names(res)[w] <- paste0( gsub(".*:{2,3}", "", .name), 1:length(w) )
  }
  row.names(res) <- NULL
  return( res )
}


