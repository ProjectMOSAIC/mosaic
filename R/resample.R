##############################################
# aliases
#
deal    <- function(x, size, replace=FALSE, prob=NULL, groups=NULL, orig.ids=FALSE) {
	sample(x, size, replace=replace, prob=prob, groups=groups, orig.ids=orig.ids )
}

resample <- function(x, size, replace=TRUE, prob=NULL, groups=NULL, orig.ids=FALSE, ...) {
	sample(x, size=size, replace=replace, prob=prob, groups=groups, orig.ids=orig.ids, ...)
}

shuffle <- function(x, replace=FALSE, prob=NULL, groups=NULL, orig.ids=FALSE) 
{
	if (!is.null(groups)){
		return( .shuffle_within(x, groups=groups, replace=replace) )
	}
	return( sample(x, replace=replace, prob=prob) )
}

##############################################
# coin toss
#

nflip <- function(n=1, prob=.5, ...) {
	as.numeric( rflip(n=n, prob=prob, ...) )
}

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


print.cointoss <- function(x, ...) {
	heads <- as.numeric(x)
	other <- attributes(x)
	if (other$verbose) {
		cat(paste('\nFlipping ', other$n, ' coins [ Prob(Heads) = ', other$prob, ' ] ...\n', sep=""))
	}

	if (attributes(x)$verbose) {
			cat('\n')
			#print(other$sequence)
			cat(strwrap( paste(other$sequence, sep=" ")))
			cat('\n')
			cat(paste('\nResult: ', heads, ' heads.\n\n', sep=""))
	}
}


##############################################
# override base::sample with something fancier
#
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



sample.default <- function(x, size, replace=FALSE, prob=NULL, groups=NULL, orig.ids=FALSE, ...) { 
	if (! is.null(groups) ) {
		if (! missing(size) ) warning("'size' is ignored when groups is non-null")
		return(.shuffle_within(x, replace=replace, prob=prob, groups=groups, 
			orig.ids=orig.ids))
	}

	result <- base::sample(x, size, replace=replace, prob=prob) 
	return(result)
}


sample.data.frame <- function(x, size, replace = FALSE, prob = NULL, groups=NULL, 
      orig.ids=TRUE, fixed=names(x), shuffled=c(),
      invisibly.return = NULL, ...) {
        if( missing(size) ) size = nrow(x)
        if( is.null(invisibly.return) ) invisibly.return = size>50 
	shuffled <- intersect(shuffled, names(x))
	fixed <- setdiff(intersect(fixed, names(x)), shuffled)
	n <- nrow(x)
	ids <- base::sample(n, size, replace=replace, prob=prob, ...)

	groups <- eval( substitute(groups), x )
	if (! is.null(groups) ) {
		ids <- base::sample(n, size, replace=replace, prob=prob, ...)
		groups <- groups[ids]
		idsString <- as.character(ids)
		xsub <- x[ids,]
		result <- x[ids,fixed]
		for (column in shuffled) {
			cids <- sample(ids, groups=groups)
			result[,column] <- x[cids,column]
			idsString <- paste(idsString, ".", cids, sep="")
		}
		if (orig.ids) result$orig.ids <- idsString
		return(result)
	}

	idsString <- as.character(ids)
	result <-  x [ ids, union(fixed,shuffled), drop=FALSE ] 
	for (column in shuffled) {
		cids <- sample(ids)
		result[,column] <- x[cids,column]
		idsString <- paste(idsString, ".", cids, sep="")
	}
	if (orig.ids) {
		result$orig.ids <- idsString
	}
	if (invisibly.return) { invisible(result) } else {return(result)}
}

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
		if (length(ids) < 50) { return(data) } else {invisible(data)}
}

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


