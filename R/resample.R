##############################################
# coin toss
#

#' Tossing Coins
#'
#' These functions simplify simulating coin tosses for those (students primarily)
#' who are not yet familair with the binomial distributions or just like this syntax
#' and verbosity better.
#'
#' @rdname rflip
#' @return  for \code{rflip}, a cointoss object 

#' @param n the number of coins to toss
#' @param prob probability of heads on each toss
#' @param quiet a logical.  If \code{TRUE}, less verbose output is used.
#' @param verbose  a logical.  If \code{TRUE}, more verbose output is used.
#' 
#' @examples
#' rflip(10)
#' rflip(10, prob=1/6, quiet=TRUE)
#' do(5) * rflip(10)
#' @export

rflip <- function(n=1, prob=.5, quiet=FALSE, verbose = !quiet) {
	if ( ( prob > 1 && is.integer(prob) ) ) {  
		# swap n and prob
		temp <- prob
		prob <- n
		n <- temp
	}
	r <- rbinom(n,1,prob)
	result <- c('T','H')[ 1 + r ]
	heads <- sum(r)
	attr(heads,"n") <- n
	attr(heads,"prob") <- prob 
	attr(heads,"sequence") <- result
	attr(heads,"verbose") <- verbose
	class(heads) <- 'cointoss'
	return(heads)

	# return(structure(heads, heads=heads, n=n, prob=prob, sequence=result, verbose=verbose, 
	# class='cointoss'))

}


#' @rdname rflip
#' @param x an object 
#' @param \dots additional arguments
#' @export

print.cointoss <- function(x, ...) {
	heads <- as.numeric(x)
	other <- attributes(x)
	if (other$verbose) {
		cat(paste('\nFlipping ', 
              other$n, 
              ' coin', ifelse( other$n > 1, "s", ""), 
              ' [ Prob(Heads) = ', other$prob, ' ] ...\n', sep=""))
	}

	if (attributes(x)$verbose) {
			cat('\n')
			#print(other$sequence)
			cat(strwrap( paste(other$sequence, sep=" ")))
			cat('\n')
			cat(paste('\nNumber of Heads: ', heads, ' [Proportion Heads: ', heads/other$n, ']\n\n', sep=""))
	}
}

#' @rdname rflip
#' @return  for \code{nflip}, a numeric vector
#' @examples
#' as.numeric(rflip(10))
#' nflip(10)
#' @export

nflip <- function(n=1, prob=.5, ...) {
	as.numeric( rflip(n=n, prob=prob, ...) )
}


#' More Random Samples
#'
#' These functions simplify and unify sampling in various ways.
#'
#' @rdname resample
#'
#' @param x	Either a vector of one or more elements from which to choose, or a positive integer. 
#' @param size	a non-negative integer giving the number of items to choose.
#' @param replace	Should sampling be with replacement?
#' @param prob	A vector of probability weights for obtaining the elements of the vector being sampled.
#'
#' @details These functions are wrappers around \code{\link{sample}} providing different defaults and 
#' natural names.
#' @examples
#' # 100 Bernoulli trials -- no need for replace=TRUE
#' resample(0:1, 100)
#' tally(resample(0:1, 100))
#' if (require(mosaicData)) {
#' Small <- sample(KidsFeet, 10)
#' resample(Small)
#' tally(~ sex, data=resample(Small))
#' tally(~ sex, data=resample(Small))
#' # fixed marginals for sex
#' tally(~ sex, data=Small)
#' tally(~ sex, data=resample(Small, groups=sex)) 
#' # shuffled can be used to reshuffle some variables within groups
#' # orig.id shows where the values were in original data frame.
#' Small <- mutate(Small, 
#'    id1 = paste(sex,1:10, sep=":"),  
#'    id2 = paste(sex,1:10, sep=":"))
#' resample(Small, groups=sex, shuffled=c("id1","id2"))
#' }
#' @export

resample <- function(..., replace=TRUE) {
  sample(..., replace=replace)
}

#' @rdname resample
#' @export
#' @examples
#' deal(Cards, 13)    # A Bridge hand
 
deal <- function(...) {
  sample(...)
}

#' @rdname resample
#' @export
#' @examples
#' shuffle(Cards)

shuffle <- function(x, replace=FALSE, prob=NULL, groups=NULL, orig.ids=FALSE) 
{
	if (!is.null(groups)){
		return( .shuffle_within(x, groups=groups, replace=replace) )
	}
	return( sample(x, replace=replace, prob=prob, groups=groups) )
}



##############################################
# override base::sample with something fancier
#

#' @rdname resample
#' @export
sample <- function (x, size, replace=FALSE, ...) {
	UseMethod('sample') 
}

.shuffle_within = function( x, groups=NULL, replace=FALSE, prob=NULL, orig.ids=FALSE, ... ){
	if (is.null(groups)) {
		stop("Must specify groups to resample within.")
	}
	# force groups to have the right size, recycling as needed.
	if (is.null(dim(x))) {
		groups <- rep(groups, length.out=length(x))
	} else {
		groups <- rep(groups, length.out=nrow(x))
	}
	groups = as.factor(groups)
	flag = c()
	levs = levels(groups);
	for (lev in levs) { # k in 1:length(levs) ) {
		ids = which( groups==lev )
		if (length(ids)==1 ) { flag = c(lev) }
		rids = sample(ids, replace=replace, orig.ids=orig.ids) 
		if( is.null(dim(x))) {
			x[ ids] = x[rids]}
		else {
			if( is.data.frame(x) | is.matrix(x) ) {
				x[ids,] = x[rids,]
			} else {
				x[ids] = x[rids]
			}
		}
	}
	if (length(flag) > 0) {
		message <- paste ("The following groups had only 1 member and can't be shuffled: ", flag)
		warning(message)
	}
	return(x)
}


#' @rdname resample
#' @export

sample.default <- function(x, size, replace=FALSE, prob=NULL, 
                           groups=NULL, orig.ids=FALSE, ...) { 
  missingSize <- missing(size)
  haveGroups <- ! is.null(groups)
  if (length(x) == 1L && is.numeric(x) && x >= 1) {
    n <- x
    x <- 1:n
    if (missingSize)  size <- n
  } else {
    n <- length(x)
    if (missingSize) size <- length(x)
  }
  if (haveGroups && size != n) {
    warning("'size' is ignored when using groups.")
    size <- n
  } 
  ids <- 1:n
  
  if (haveGroups) {
    groups <- rep( groups, length.out=size)  # recycle as needed
    result <- aggregate( ids, by=list(groups), FUN=base::sample, 
                         simplify=FALSE,
                         replace=replace, prob=prob)
    result <- unlist(result$x)
    if (orig.ids) { nms <- ids[result] }
    result <- x[result]
    if (orig.ids) { names(result) <- nms }
    return(result)
	}
	result <- base::sample(x, size, replace=replace, prob=prob) 
	return(result)
}

#' @rdname resample
#' @param groups a vector (or variable in a data frame) specifying
#' groups to sample within. This will be recycled if necessary.
#' @param orig.ids  a logical; should origianal ids be included in returned data frame?
#' @param \dots additional arguments passed to 
#' \code{\link[base]{sample}}
#' or \code{\link[mosaic]{sample}}.
#' @param shuffled a vector of column names.  
#' these variables are reshuffled individually (within groups if \code{groups} is
#' specified), breaking associations among these columns.
#' examples.
#' @param fixed a vector of column names.  These variables are shuffled en masse,
#' preserving associations among these columns.
#' @param invisibly.return a logical, should return be invisible?
#' @param drop.unused.levels a logical, should unused levels be dropped?
#' @export

sample.data.frame <- function(x, size, replace = FALSE, prob = NULL, groups=NULL, 
                              orig.ids=TRUE, fixed=names(x), shuffled=c(),
                              invisibly.return = NULL, ...) {
  if( missing(size) ) size = nrow(x)
  if( is.null(invisibly.return) ) invisibly.return = size>50 
  shuffled <- intersect(shuffled, names(x))
  fixed <- setdiff(intersect(fixed, names(x)), shuffled)
  n <- nrow(x)
  ids <- 1:n
  groups <- eval( substitute(groups), x )
  newids <- sample(n, size, replace=replace, prob=prob, groups=groups, ...)
  origids <- ids[newids]
  result <- x[newids, , drop=FALSE]
  
  idsString <- as.character(origids)
  
  for (column in shuffled) {
    cids <- sample(newids, groups=groups[newids])
    result[,column] <- x[cids,column]
    idsString <- paste(idsString, ".", cids, sep="")
  }
  
  result <-  result[ , union(fixed,shuffled), drop=FALSE]
  if (orig.ids) result$orig.id <- idsString
  
  
  if (invisibly.return) { return(invisible(result)) } else {return(result)}
}

#' @rdname resample
#' @export

sample.matrix <- function(x, size, replace = FALSE, prob = NULL, groups=NULL, orig.ids=FALSE, ...) {
	if (! is.null(groups) ) {
		return(
			.shuffle_within(x, replace=replace, prob=prob, groups=groups, orig.ids=orig.ids)
		)
	}
	n <- nrow(x)
		ids <- base::sample(n, size, replace=replace, prob=prob)
		data <-  x [ ids, , drop=FALSE] 
		names(data) <- names(x)
		if (orig.ids) {
			attr(data,'orig.row') <- ids
		}
		if (length(ids) < 50) { return(data) } else {return(invisible(data))}
}

#' @rdname resample
#' @export
sample.factor <- function(x, size, replace = FALSE, prob = NULL, groups=NULL, orig.ids=FALSE, 
					drop.unused.levels = FALSE, ...) {
	if (! is.null(groups) ) {
		return(
			.shuffle_within(x, replace=replace, prob=prob, groups=groups, orig.ids=orig.ids)
		)
	}
	n <- length(x)
	ids <- base::sample(n, size, replace=replace, prob=prob)
	if (drop.unused.levels) {
		data <-  factor( x [ ids ] )
	} else {
		data <-  factor( x [ ids ], levels=levels(x) )
	}
	return(data)
}

#' @export
sample.lm <- 
  function(
    x, size, replace = FALSE, prob = NULL, groups=NULL, 
    orig.ids=FALSE, drop.unused.levels = FALSE, 
    parametric = FALSE, 
    transformation = NULL,
    ...) {
   
    if (!is.null(prob)) {
      warning("Unused argument: prob")
    }
    if (!is.null(groups)) {
      warning("Unused argument: groups")
    }
    
    if (! replace) {
      stop("Only resampling supported for linear model objects.")
    }
    # replace == TRUE
   
    orig_data <- eval( x$call[["data"]], environment(formula(x)) )
    dfx <- orig_data[, all.vars(formula(x))]
    complete_idx <- which(complete.cases(dfx))
    res <- dfx[complete_idx, ]
   
    if (! missing(size)) {
      if (size != nrow(res)) {
        stop ("Invalid value for `size'.")
      }
      
      warning("`size' is ignored when resampling an `lm' object.")
    }
    size <- nrow(res)
    
    res$resid <- resid(x)
    if (parametric) {
      res$new_resid <- rnorm(size, mean = 0, sd = summary(x)$sigma)
    } else {
      res$new_resid <- 
        (1 - 2 * rbinom(size, 1, 0.5)) * resample(resid(x))
    }
    res$new_response <- fitted(x) + res$new_resid
   
     
    if (is.null(transformation)) {
      transformation <- inferTransformation(formula(x))
    }
    res[[1]] <- do.call(transformation, list(res$new_response))
    # remove "scratch columns"
    res <- res %>% 
      select_(.dots = setdiff(names(res), c("resid", "new_resid", "new_response")))
    res
  }

inferTransformation <- function(formula) {
  transformation <- identity
  left <- lhs(formula)
  if (length(left) == 2) {       # foo ( stuff )
    if (is.name(left[[2]])) {    # stuff is a name
      transformation <- 
        switch( 
          as.character(left[[1]]),
          "log" = exp,
          "log10" = function(x) {10^x},
          "log2" = function(x) {2^x},
          "sqrt" = function(x) x^2,
          identity
        )
    }   # could have identity if stuff is not a name or foo is not a known function
    if (identical(transformation, identity)) {
      warning("You may need to specify transformation to get the desired results.")
    } 
  }
  if (length(left) > 2) {
    warning("You may need to specify transformation to get the desired results.")
  }
  transformation
}

#' Resample a Linear Model
#' 
#' Residual resampling from a linear model
#' 
#' @param model a linear model object produced using \code{\link{lm}}.
#' @param ...  additional arguments passed through to \code{\link{resample}}.
#' @param envir an environment in which to (re)evaluate the linear model.
#' @export
relm <- function(model, ..., envir = environment(formula(model))) {
  mcall <- model$call
  mcall[["data"]] <- resample(model, ...)
  eval(mcall, envir)
}

#' 
#' Simulate spinning a spinnner
#' 
#' This is essentially \code{rmultinom} with a different interface.
#' 
#' @param n number of spins of spinner
#' @param probs a vector of probabilities.  If the sum is not 1, the 
#' probabiliies will be rescaled.
#' @param labels a character vector of labels for the categories
#' @export
#' @examples
#' rspin(20, prob=c(1,2,3), labels=c("Red", "Blue", "Green"))
#' do(2) * rspin(20, prob=c(1,2,3), labels=c("Red", "Blue", "Green"))
 
rspin <- function(n, probs, labels=1:length(probs)) {
  if (any(probs < 0))
    stop("All probs must be non-negative.")
  
  probs <- probs/sum(probs)
  res <- t(rmultinom(1, n, probs)) %>% as.data.frame()
  names(res) <- labels
  res
}
