tryCatch(utils::globalVariables(c('.row')), 
         error=function(e) message('Looks like you should update R.'))

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
#'   using the \pkg{parallel} package (if it is installed).
#' 
#' @param e1 an object (in cases documented here, the result of running \code{do})
#' @param e2 an object (in cases documented here, an expression to be repeated)
#' 
#' @param ... additional arguments
#' 
#' @note \code{do} is a thin wrapper around \code{Do} to avoid collision with
#'   \code{\link[dplyr]{do}} from the \pkg{dplyr} package.
#' @return \code{do} returns an object of class \code{repeater} which is only useful in
#'   the context of the operator \code{*}.  See the examples.
#' @author Daniel Kaplan (\email{kaplan@@macalaster.edu})
#'   and Randall Pruim (\email{rpruim@@calvin.edu})
#'
#' @seealso \code{\link{replicate}}
#' 
#' @examples
#' do(3) * rnorm(1)
#' do(3) * "hello"
#' do(3) * lm(shuffle(height) ~ sex + mother, Galton)
#' do(3) * anova(lm(shuffle(height) ~ sex + mother, Galton))
#' do(3) * 1:4
#' do(3) * mean(rnorm(25))
#' do(3) * c(sample.mean = mean(rnorm(25)))
#' do(3) * tally( ~sex|treat, data=resample(HELPrct))
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
 
nice_names <- function(x) {
	x <- gsub('\\(Intercept\\)','Intercept', x)
	x <- gsub('resample\\(','', x)
	x <- gsub('sample\\(','', x)
	x <- gsub('shuffle\\(','', x)
	x <- gsub('\\(','.', x)
	x <- gsub('-','.', x)
	x <- gsub(':','.', x)
	x <- gsub('\\)','', x)
	x <- gsub(' ','.', x)
	return(x)
}

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

#' @rdname mosaic-internal
#' @keywords internal
#' @details
#' \code{.cull_for_do} handles objects like models to do the right thing for \code{do}
# 
#' @return an object reflecting some of the information contained in \code{object}

.cull_for_do = function(object) {
  if (inherits(object, "aov")) {
    object <- anova(object)
  }
  if (inherits(object, "anova")) {
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
  if (any(class(object)=='table')){
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
	if (any(class(object)=='aggregated.stat')) {
		result <- object
		res <- as.vector(result[, "S"])  # ncol(result)]
		names(res) <- paste( attr(object,'stat.name'), 
						.squash_names(object[,1:(ncol(object)-3),drop=FALSE]), sep=".")
		return(res)
	} #
	if (any(class(object)=='lme')){ # for mixed effects models
		result <- object
		names(result) <- nice_names(names(result))
		return( object$coef$fixed )
	}
	if (inherits(object,c('lm','groupwiseModel')) ) {
		sobject <- summary(object)
		result <-  c( coef(object), sigma=sobject$sigma, r.squared = sobject$r.squared ) 
		names(result) <- nice_names(names(result))
		return(result)
	}
	if (any(class(object)=='htest') ) {
		result <-  data.frame( 
					  statistic = object$statistic, 
		              parameter = object$parameter,
					  p.value = object$p.value,
					  conf.level = attr(object$conf.int,"conf.level"),
					  lower = object$conf.int[1],
					  upper = object$conf.int[2],
					  method = object$method,
					  alternative = object$alternative,
					  data = object$data.name
					  )
		if ( ! is.null( names(object$statistic) ) ) names(result)[1] <- names(object$statistic)
		return(result)
	}
	if (any(class(object)=='table') ) {
		nm <- names(object)
		result <-  as.vector(object)
		names(result) <- nm
		return(result)
	}
	if (any(class(object)=='cointoss')) {
		return( c(n=attr(object,'n'), 
				heads=sum(attr(object,'sequence')=='H'),
				tails=sum(attr(object,'sequence')=='T'),
        prop=sum(attr(object,'sequence')=="H") / attr(object,'n')
				) )
	}
	if (is.matrix(object) && ncol(object) == 1) {
		nn <- rownames(object)
		object <- as.vector(object)
		if (is.null(nn)) {
			names(object) <- paste('v',1:length(object),sep="")
		} else {
			names(object) <- nn
		}
		return(object)
	}
	return(object) }

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
    result <- data.frame(result=as.vector(ul))
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


#' @rdname do
#' @aliases *,repeater,ANY-method
#' @export

setMethod("*",
    signature(e1 = "repeater", e2="ANY"),
    function (e1, e2) 
    {
		e2unevaluated = substitute(e2)
		if ( ! is.function(e2) ) {
      frame <- parent.frame()
			e2 = function(){eval(e2unevaluated, envir=frame) }   
		}
		n = e1@n

		cull = e1@cull
		if (is.null(cull)) {
			cull <- .cull_for_do
		}
    
		out.mode <- if (!is.null(e1@mode)) e1@mode else 'default'

    if (e1@algorithm >= 1) {
      resultsList <- if( e1@parallel && require(parallel) )
        parallel::mclapply( integer(n), function(...) { cull(e2()) } )
      else 
        lapply( integer(n), function(...) { cull(e2()) } )
          
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
		  names(result) <- nice_names(names(result))
	  }
      return(result)
    }
  
    ## pre 1.0 algorithm...
    message("Using older algorithm for do()")
    
		res1 = cull(e2())  # was (...)
		nm = names(res1)

		if (!is.null(e1@mode)) { 
			out.mode <- e1@mode 
		} else {
			out.mode <- 'list'

			if ( is.vector( res1) || is.data.frame(res1) ) {
				if (is.null(nm)) { 
					out.mode <- 'matrix' 
				} else {
					out.mode <- 'data.frame'
				}
			}
		}


		if ( out.mode == 'list' ) {
			result <- list()
			result[[1]] <- res1
			if (n < 2) return (res1) 
			for (k in 2:n) {
				result[[k]] <- cull(e2()) # was (...)
			}
			return(result)
		}

		if (out.mode == 'data.frame') {
			result <- .make.data.frame(res1)
      result$do.ind <- 1
      result$do.row <- 1:nrow(result)
			if (n>1) {
			  for (k in 2:n) {
			  	res2 <- .make.data.frame(cull(e2()))
          res2$do.ind <- k
          res2$do.row <- 1:nrow(res2)
				# print(res2)
				# result <- rbind( result, cull(e2()) ) 
				  result <- .merge_data_frames( result, res2)
			  }
			}
			# rownames(result) <- 1:nrow(result)
			# names(result) <- nm
      # mark result as having originated with do()
      if (all(result$do.row == 1)) { result[["do.row"]] <- NULL; result[["do.ind"]] <- NULL }
      
      class(result) <- c(paste('do', class(result)[1], sep="."), class(result))
			return(result)
		}

		result <- matrix(nrow=n,ncol=length(res1))
		result[1,] <- res1

		if (n > 1) {
			for (k in 2:n) {
				result[k,] <- cull(e2()) # was (...)
			}
		}

		if (dim(result)[2] == 1 & is.null(nm) ) result <- data.frame(result=result[,1]) 

    class(result) <- c(paste("do",class(result)[1], sep="."), class(result))
	if (inherits( result, "data.frame")) { 
		names(result) <- nice_names(names(result))
	}
    return(result)
	}
)
