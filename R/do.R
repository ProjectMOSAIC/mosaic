tryCatch(utils::globalVariables(c('.row')), 
         error=function(e) message('Looks like you should update R.'))

#' Set seed in parallel compatible way
#'
#' When the parallel package is used, setting the RNG seed for reproducibility
#' involves more than simply calling \code{\link{set.seed}}. \code{set.rseed} takes
#' care of the additional overhead.
#'
#' @param seed seed for the random number generator
#' @details
#' If the \code{parallel} package is not on the search path, then \code{\link{set.seed}} is called.
#' If \code{parallel} is on the search path, then the RNG kind is set to \code{"L'Ecuyer-CMRG"},
#' the seed is set and \code{mc.reset.stream} is called.
#' 
#' @examples
#' # These should give identical results, even if the `parallel' package is loaded.
#' set.rseed(123); do(3) * resample(1:10, 2)
#' set.rseed(123); do(3) * resample(1:10, 2)
#' @export

set.rseed <- function(seed) {
  if ("package:parallel" %in% search()) {
    set.seed(seed, kind = "L'Ecuyer-CMRG")
    parallel::mc.reset.stream()
  } else {
    set.seed(seed) 
  }
}


#' Do Things Repeatedly
#' 
#' \code{do()} provides a natural syntax for repetition tuned to assist 
#' with replication and resampling methods.
#'
#' @rdname do
#' @param n  number of times to repeat 
#' 
#' @param object an object
#'
#' @param cull  function for culling output of objects being repeated.  If NULL,
#'   a default culling function is used.  The default culling function is 
#'   currently aware of objects of types
#'   \code{lme},
#'   \code{lm},
#'   \code{htest},
#'   \code{table},
#'   \code{cointoss}, and 
#'   \code{matrix}.
#'   
#' @param mode  target mode for value returned
#' 
#' @param algorithm a number usd to select the algorithm used.  Currently numbers below 1 
#'   use an older algorithm and numbers >=1 use a newer algorithm which is faster in some 
#'   situations.
#' @param parallel a logical indicating whether parallel computation should be attempted
#'   using the \pkg{parallel} package (if it is installed and loaded).
#' 
#' @param e1 an object (in cases documented here, the result of running \code{do})
#' @param e2 an object (in cases documented here, an expression to be repeated)
#' @param ... additional arguments
#' 
#' @note \code{do} is a thin wrapper around \code{Do} to avoid collision with
#'   \code{\link[dplyr]{do}} from the \pkg{dplyr} package.
#' @return \code{do} returns an object of class \code{repeater} which is only useful in
#'   the context of the operator \code{*}.  See the examples.
#' @author Daniel Kaplan (\email{kaplan@@macalaster.edu})
#'   and Randall Pruim (\email{rpruim@@calvin.edu})
#'
#' @seealso \code{\link{replicate}}, \code{\link{set.rseed}}
#' 
#' @examples
#' do(3) * rnorm(1)
#' do(3) * "hello"
#' do(3) * 1:4
#' do(3) * mean(rnorm(25))
#' if (require(mosaicData)) {
#'   do(3) * lm(shuffle(height) ~ sex + mother, Galton)
#'   do(3) * anova(lm(shuffle(height) ~ sex + mother, Galton))
#'   do(3) * c(sample.mean = mean(rnorm(25)))
#'   set.rseed(1234)
#'   do(3) * tally( ~sex|treat, data=resample(HELPrct))
#'   set.rseed(1234)  # re-using seed gives same results again
#'   do(3) * tally( ~sex|treat, data=resample(HELPrct))
#' }
#' @keywords iteration 
#' @export

do <- function(object, ...) {
  UseMethod("do")
}

#' @rdname do
#' @export
do.numeric <- function(object, ...) {
  Do(n=object, ...)
}

#' @rdname do
#' @export
do.default <- function(object, ...) {
  dplyr::do(object, ...)
}

#' @rdname do
#' @export
Do <- function(n=1L, cull=NULL, mode='default', algorithm=1.0, parallel=TRUE) {
	new( 'repeater', n=n, cull=cull, mode=mode, algorithm=algorithm, parallel=parallel)
}

#' @rdname mosaic-internal
#' @keywords internal
#' @details
#' \code{.make.data.frame} converts things to a data frame
#' @param x object to be converted
#' @return a data frame

.make.data.frame <- function( x ) {
	if (is.data.frame(x)) return(x)
	if (is.vector(x)) {
		nn <- names(x)
		result <- as.data.frame( matrix(x, nrow=1) )
		if (! is.null(nn) ) names(result) <- nn
		return(result)
	}
	return(as.data.frame(x))
	}

#' Nice names
#'
#' Convert a character vector into a similar character vector that would 
#' work better as names in a data frame by avoiding certain awkward characters
#'
#' @rdname nicenames
#' @param x a character vector
#' @return a character vector
#' @examples
#' nice_names( c("bad name", "name (crazy)", "a:b", "two-way") )
#' @export
 
nice_names <- function(x, unique=TRUE) {
	x <- gsub('%>%', '.result.', x)
	x <- gsub('\\(Intercept\\)','Intercept', x)
	x <- gsub('resample\\(','', x)
	x <- gsub('sample\\(','', x)
	x <- gsub('shuffle\\(','', x)
#	x <- gsub('\\(','.', x)
#	x <- gsub('-','.', x)
#	x <- gsub(':','.', x)
#	x <- gsub('\\)','', x)
#	x <- gsub(' ','.', x)
#	x <- gsub('^([0-9])','X\\1', x)
	return(make.names(x, unique = unique))
}



null2na <- function(x) if (is.null(x)) NA else x

#' Repeater objects
#'
#' Repeater objects can be used with the \code{*} operator to repeat
#' things multiple time using a different syntax and different output
#' format from that used by, for example, \code{\link{replicate}}.
#'
#' @rdname repeater-class
#' @name repeater-class
#' @seealso \code{\link{do}}
#' @section Slots:
#' \describe{
#'   \item{\code{n}:}{Object of class \code{"numeric"} indicating how many times to repeat something.}
#'   \item{\code{cull}:}{Object of class \code{"function"} that culls the ouput from each repetition.}
#'   \item{\code{mode}:}{Object of class \code{"character"} indicating the output mode 
#'   ('default', 'data.frame', 'matrix', 'vector', or 'list').  For most purposes 'default' (the default)
#'   should suffice.}
#'   \item{\code{algorithm}:}{an algorithm number.}
#'   \item{\code{parallel}:}{a logical indicating whether to attempt parallel execution.}
#' }
#' @exportClass repeater

setClass('repeater', 
	representation = representation(n='numeric', cull='ANY', mode='character', 
                                  algorithm='numeric', parallel='logical'),
	prototype = prototype(n=1, cull=NULL, mode="default", algorithm=1, parallel=TRUE)
)


# old version
if(FALSE) {
.merge_data_frames <- function(a, b) {
  a <- .make.data.frame(a)
  b <- .make.data.frame(b)
  if (nrow(b) < 1) return (a) 
  if (nrow(a) < 1) return (b) 

	a$mosaic_merge_id <- paste('A',1:nrow(a))
	b$mosaic_merge_id <- paste('B',1:nrow(b))
	result <- merge(a,b,all=TRUE)
	w <- which(names(result) == 'mosaic_merge_id')
	result <- result[, -w]
	return(result)
}
}


#' @rdname mosaic-internal
#' @keywords internal
#' @details \code{.merge_data_frames} is a wrapper around merge
#'
#' @param a a data frame
#' @param b a data frame
#'
#' @return a data frame 

.merge_data_frames = function(a,b) {
  a <- .make.data.frame(a)
  b <- .make.data.frame(b)
  if (nrow(b) < 1) return (a) 
  if (nrow(a) < 1) return (b) 
  missing.from.b = setdiff(names(a),names(b))
  missing.from.a = setdiff(names(b),names(a))
  for (var in missing.from.b) b[[var]] = NA
  for (var in missing.from.a) a[[var]] = NA
  rbind(a,b)
}


#' @rdname mosaic-internal
#' @keywords internal
#' @details 
#' \code{.squash_names} squashes names of a data frame into a single string
#'
#' @param object an object
#' @param sep a character
#'
#' @return a character vector

.squash_names <- function(object,sep=":") {
	if ( ncol(object) < 1 ) {return(rep("",nrow(object)))}

	result <- object[,1]
	if ( ncol(object) < 2 ) {return(as.character(result))}

	for (c in 2:ncol(object)) {
		result <- paste(result, as.character(object[,c]), sep=sep)
	}
	return(result)
		
}

# # @rdname mosaic-internal
# # @keywords internal
# # @details
# # \code{.cull_for_do} handles objects like models to do the right thing for \code{do}
# #
# # @return an object reflecting some of the information contained in \code{object}
# 
# .cull_for_do = function(object) {
#   if (inherits(object, "aov")) {
#     object <- anova(object)
#   }
#   if (inherits(object, "anova")) {
#     res <- as.data.frame(object)
#     res <- cbind (data.frame(source=row.names(res)), res)
#     names(res)[names(res) == "Df"] <- "df"
#     names(res)[names(res) == "Sum Sq"] <- "SS"
#     names(res)[names(res) == "Mean Sq"] <- "MS"
#     names(res)[names(res) == "F value"] <- "F"
#     names(res)[names(res) == "Pr(>F)"] <- "pval"
#     names(res)[names(res) == "Sum of Sq"] <- "diff.SS"
#     names(res)[names(res) == "Res.Df"] <- "res.df"
#     return(res)
#     return( data.frame(
#       SSTotal= sum(object$`Sum Sq`),
#       SSModel= object$`Sum Sq`[1],
#       SSError= object$`Sum Sq`[2],
#       MSTotal= sum(object$`Sum Sq`),
#       MSModel= object$`Mean Sq`[1],
#       MSError= object$`Mean Sq`[2],
#       F=object$`F value`[1],
#       dfModel=object$Df[1],
#       dfError=object$Df[2],
#       dfTotal=sum(object$Df)
#     ) )
#   }
#   if (inherits(object, 'table')){
#     result <- data.frame(object)
#     res <- result[[ncol(result)]]
#     nms <- as.character(result[[1]])
#     if (ncol(result) > 2) {
#       for (k in 2:(ncol(result)-1)) {
#         nms <- paste(nms, result[[k]],sep=".")
#       }
#     }
#     names(res) <- nms
#     return(res)
#   }
#   if (inherits(object, 'aggregated.stat')) {
#     result <- object
#     res <- as.vector(result[, "S"])  # ncol(result)]
#     names(res) <- paste( attr(object,'stat.name'), 
#                          .squash_names(object[,1:(ncol(object)-3),drop=FALSE]), sep=".")
#     return(res)
#   } #
#   if (inherits(object, 'lme')){ # for mixed effects models
#     result <- object
#     names(result) <- nice_names(names(result))
#     return( object$coef$fixed )
#   }
#   if (inherits(object,c('lm','groupwiseModel')) ) {
#     sobject <- summary(object)
#     Fstat <- sobject$fstatistic[1]
#     DFE <- sobject$fstatistic["dendf"]
#     DFM <- sobject$fstatistic["numdf"]
#     if (!is.null(Fstat)) {
#       names(Fstat) <- "F"
#       result <-  c(coef(object), sigma=sobject$sigma, 
#                    r.squared = sobject$r.squared, 
#                    Fstat,
#                    DFM,
#                    DFE)
#     } else {
#       result <-  c(coef(object), sigma=sobject$sigma, 
#                    r.squared = sobject$r.squared
#       )
#     }
#     names(result) <- nice_names(names(result))
#     return(result)
#   }
#   if (inherits(object,'htest')) {
#     if (is.null(object$conf.int)) {
#       result <-  data.frame( 
#         statistic = null2na(object$statistic), 
#         parameter = null2na(object$parameter),
#         p.value = null2na(object$p.value),
#         method = null2na(object$method),
#         alternative = null2na(object$alternative),
#         data = null2na(object$data.name)
#       )
#     } else {
#       result <-  data.frame( 
#         statistic = null2na(object$statistic), 
#         parameter = null2na(object$parameter),
#         p.value = null2na(object$p.value),
#         conf.level = attr(object$conf.int,"conf.level"),
#         lower = object$conf.int[1],
#         upper = object$conf.int[2],
#         method = null2na(object$method),
#         alternative = null2na(object$alternative),
#         data = null2na(object$data.name)
#       )
#     }
#     if ( !is.null(names(object$statistic)) ) 
#       names(result)[1] <-  names(object$statistic)
#     if ( !is.null(names(object$parameter)) ) 
#       names(result)[2] <- names(object$parameter)
#     return(result)
#   }
#   if (inherits(object, 'table') ) {
#     nm <- names(object)
#     result <-  as.vector(object)
#     names(result) <- nm
#     return(result)
#   }
#   if (inherits(object, 'cointoss')) {
#     return( c(n=attr(object,'n'), 
#               heads=sum(attr(object,'sequence')=='H'),
#               tails=sum(attr(object,'sequence')=='T'),
#               prop=sum(attr(object,'sequence')=="H") / attr(object,'n')
#     ) )
#   }
#   if (is.matrix(object) && ncol(object) == 1) {
#     nn <- rownames(object)
#     object <- as.vector(object)
#     if (is.null(nn)) {
#       names(object) <- paste('v',1:length(object),sep="")
#     } else {
#       names(object) <- nn
#     }
#     return(object)
#   }
#   return(object) 
# }

# #' @aliases print,repeater-method
#' @rdname do
#' @export
# setMethod("print",
#     signature(x = "repeater"),
#     function (x, ...) 
print.repeater <- function(x, ...) 
    {
  		message(paste('This repeats a command',x@n,'times. Use with *.'))
  		return(invisible(x))
    }
# )

.list2tidy.data.frame <- function (l) {
  
  # see if we really just have a vector
  ul <- unlist( l )
  if ( length(ul) == length(l) ) {
    result <- data.frame(..result.. = as.vector(ul))
    row.names(result) <- NULL
    if( !is.null(names(l[[1]])) ) names(result) <- names(l[[1]])
    return(result)
  }
  
  # if each element is a data frame with the same variables, combine them
  if ( all( sapply( l, is.data.frame ) ) ) {
    tryCatch( 
      return ( 
        transform( 
          do.call( rbind, lapply ( l, function(x) { transform(x, .row= 1:nrow(x)) }) ),
          .index = c(1, 1 + cumsum( diff(.row) != 1 )) 
        )
      ), error=function(e) {} 
    )
  }
  
  # If rbind() works, do it
  tryCatch(
    return ( as.data.frame( do.call( rbind, l) ) ),
    error=function(e) {} 
  )
  
  if (all (sapply(l, length) ) == length(l[[1]]) ) {
    result <-  as.data.frame( matrix( ul, nrow=length(l) ) )
    names(result) <- names(l[[1]])
    return(result)
  }
  
  # nothing worked.  Just return the list as is.
  return( l )
}

#' Cull objects used with do()
#' 
#' The \code{\link{do}} function facilitates easy repliaction for
#' randomization tests and bootstrapping (among other things).  Part of what
#' makes this particularly useful is the ability to cull from the objects
#' produced those elements that are useful for subsequent analysis. 
#' \code{cull_for_do} does this culling.  It is generic, and users
#' can add new methods to either change behavoir or to hanlde additional
#' classes of objects.
#' 
#' @param object an object to be culled
#' @param ... additional arguments (currently ignored)
#' 
#' @details When \code{do(n) * expression} is evaluated, \code{expression}
#' is evaluated \code{n} times to produce a list of \code{n} result objects.
#' \code{cull_for_do} is then applied to each element of this list to 
#' extract from it the information that should be stored.  For example,
#' when applied to a object of class \code{"lm"},
#' the default \code{cull_for_do} extracts the coefficients, coefficient
#' of determinism and the estimate for the variance.
#' 
#' @export 
#' @examples
#' cull_for_do(lm(length ~ width, data = KidsFeet))

cull_for_do <- function(object, ...) {
  UseMethod("cull_for_do")
}


#' @export 
cull_for_do.default <- function(object, ...) {
  object
}

#' @export 
cull_for_do.aov <- function(object, ...) {
  cull_for_do(anova(object))
}
  
#' @export 
cull_for_do.anova <- function(object, ...) {  
    res <- as.data.frame(object)
    res <- cbind (data.frame(source=row.names(res)), res)
    names(res)[names(res) == "Df"] <- "df"
    names(res)[names(res) == "Sum Sq"] <- "SS"
    names(res)[names(res) == "Mean Sq"] <- "MS"
    names(res)[names(res) == "F value"] <- "F"
    names(res)[names(res) == "Pr(>F)"] <- "pval"
    names(res)[names(res) == "Sum of Sq"] <- "diff.SS"
    names(res)[names(res) == "Res.Df"] <- "res.df"
    return(res)
    return( data.frame(
      SSTotal= sum(object$`Sum Sq`),
      SSModel= object$`Sum Sq`[1],
      SSError= object$`Sum Sq`[2],
      MSTotal= sum(object$`Sum Sq`),
      MSModel= object$`Mean Sq`[1],
      MSError= object$`Mean Sq`[2],
      F=object$`F value`[1],
      dfModel=object$Df[1],
      dfError=object$Df[2],
      dfTotal=sum(object$Df)
    ) )
}

#' @export 
cull_for_do.table <- function(object, ...) {
  result <- data.frame(object)
  res <- result[[ncol(result)]]
  nms <- as.character(result[[1]])
  if (ncol(result) > 2) {
    for (k in 2:(ncol(result)-1)) {
      nms <- paste(nms, result[[k]],sep=".")
    }
  }
  names(res) <- nms
  return(res)
}

#' @export 
cull_for_do.aggregated.stat <- function(object, ...) {
  result <- object
  res <- as.vector(result[, "S"])  # ncol(result)]
  names(res) <- paste( attr(object,'stat.name'), 
                       .squash_names(object[,1:(ncol(object)-3),drop=FALSE]), sep=".")
  return(res)
} 

#' @export 
cull_for_do.lme <- function(object, ...) {
  result <- object
  names(result) <- nice_names(names(result))
  return( object$coef$fixed )
}

#' @export 
cull_for_do.lm <- function(object, ...) {
  sobject <- summary(object)
  Fstat <- sobject$fstatistic[1]
  DFE <- sobject$fstatistic["dendf"]
  DFM <- sobject$fstatistic["numdf"]
  if (!is.null(Fstat)) {
    names(Fstat) <- "F"
    result <-  c(coef(object), sigma=sobject$sigma, 
                 r.squared = sobject$r.squared, 
                 Fstat,
                 DFM,
                 DFE)
  } else {
    result <-  c(coef(object), sigma=sobject$sigma, 
                 r.squared = sobject$r.squared
    )
  }
  names(result) <- nice_names(names(result))
  return(result)
}

#  @export 
# cull_for_do.groupwiseModel <- function(object, ...) {
#   sobject <- summary(object)
#   Fstat <- sobject$fstatistic[1]
#   DFE <- sobject$fstatistic["dendf"]
#   DFM <- sobject$fstatistic["numdf"]
#   if (!is.null(Fstat)) {
#     names(Fstat) <- "F"
#     result <-  c(coef(object), sigma=sobject$sigma, 
#                  r.squared = sobject$r.squared, 
#                  Fstat,
#                  DFM,
#                  DFE)
#   } else {
#     result <-  c(coef(object), sigma=sobject$sigma, 
#                  r.squared = sobject$r.squared
#     )
#   }
#   names(result) <- nice_names(names(result))
#   return(result)
# }
#   
  
#' @export 
cull_for_do.htest <- function(object, ...) {
  if (is.null(object$conf.int)) {
    result <-  data.frame( 
      statistic = null2na(object$statistic), 
      parameter = null2na(object$parameter),
      p.value = null2na(object$p.value),
      method = null2na(object$method),
      alternative = null2na(object$alternative),
      data = null2na(object$data.name)
    )
  } else {
    result <-  data.frame( 
      statistic = null2na(object$statistic), 
      parameter = null2na(object$parameter),
      p.value = null2na(object$p.value),
      conf.level = attr(object$conf.int,"conf.level"),
      lower = object$conf.int[1],
      upper = object$conf.int[2],
      method = null2na(object$method),
      alternative = null2na(object$alternative),
      data = null2na(object$data.name)
    )
  }
  if ( !is.null(names(object$statistic)) ) 
    names(result)[1] <-  names(object$statistic)
  if ( !is.null(names(object$parameter)) ) 
    names(result)[2] <- names(object$parameter)
  return(result)
}

#   if (inherits(object, 'table') ) {
#     nm <- names(object)
#     result <-  as.vector(object)
#     names(result) <- nm
#     return(result)
#   }

#' @export 
cull_for_do.cointoss <- function(object, ...) {
  return( c(n=attr(object,'n'), 
            heads=sum(attr(object,'sequence')=='H'),
            tails=sum(attr(object,'sequence')=='T'),
            prop=sum(attr(object,'sequence')=="H") / attr(object,'n')
  ) )
}

#' @export 
cull_for_do.matrix <- function(object, ...) {
  if (ncol(object) == 1) {
    nn <- rownames(object)
    object <- as.vector(object)
    if (is.null(nn)) {
      names(object) <- paste('v',1:length(object),sep="")
    } else {
      names(object) <- nn
    }
    return(object)
  }
}

#' @rdname do
#' @aliases *,repeater,ANY-method
#' @export

setMethod(
  "*",
  signature(e1 = "repeater", e2="ANY"),
  function (e1, e2) 
  {
    e2_lazy <- lazyeval::lazy(e2)
    #		e2unevaluated = substitute(e2)
    #		if ( ! is.function(e2) ) {
    #      frame <- parent.frame()
    #			e2 = function(){eval(e2unevaluated, envir=frame) }   
    #		}
    n = e1@n
    
    cull = e1@cull
    if (is.null(cull)) {
      cull <- cull_for_do
    }
    
    out.mode <- if (!is.null(e1@mode)) e1@mode else 'default'
    
    resultsList <- if( e1@parallel && "package:parallel" %in% search() ) {
      if (getOption("mosaic:parallelMessage", TRUE)) {
        message("Using parallel package.\n",
                "  * Set seed with set.rseed().\n", 
                "  * Disable this message with options(`mosaic:parallelMessage` = FALSE)\n")
      }
      parallel::mclapply( integer(n), function(...) { cull(lazyeval::lazy_eval(e2_lazy)) } )
    } else {
      lapply( integer(n), function(...) { cull(lazyeval::lazy_eval(e2_lazy)) } )
    }
    
    if (out.mode=='default') {  # is there any reason to be fancier?
      out.mode = 'data.frame'
    }
    
    result <- switch(out.mode, 
                     "list" = resultsList,
                     "data.frame" = .list2tidy.data.frame( resultsList ),
                     "matrix" = as.matrix( do.call( rbind, resultsList) ),
                     "vector" = unlist(resultsList)  
    ) 
    class(result) <- c(paste('do', class(result)[1], sep="."), class(result))
    if (inherits( result, "data.frame")) {
      alt_name <- as.character(e2_lazy$expr[[1]])
      names(result) <- nice_names(names(result))
      names(result)[names(result) == "..result.."] <- 
        if(nice_names(alt_name) == alt_name) alt_name else "result"
    }
    attr(result, "lazy") <- e2_lazy
    if (out.mode == "data.frame") attr(result, "culler") <- cull
    return(result)
  })
