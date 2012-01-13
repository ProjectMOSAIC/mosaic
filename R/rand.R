#' Random Regressors
#' 
#' A utility function for producing random regressors with a specified
#' number of degrees of freedom.
#' @param df degrees of freedom, i.e., number of random regressors
#' @param rdist random distribution function for sampling
#' @param args arguments for \code{rdist} 
#' @param nr number of rows in resulting matrix.  This can often be omitted in
#'   the context of functions like \code{lm} where it is inferred from the data frame, 
#'   if one is provided.
#' @param seed seed for random number generation 
#' 
#' 
#' @return A matrix of random variates with \code{df} columns.  
#' In its intended use, the number of rows will be selected to match the 
#' size of the data frame supplied to \code{lm}
#' @author Daniel Kaplan (\email{kaplan@@macalester.edu})
#' and Randall Pruim (\email{rpruim@@calvin.edu})
#' 
#' @export
#' @examples
#' rand(2,nr=4)
#' rand(2,rdist=rpois, args=list(lambda=3), nr=4)
#' summary(lm( waiting ~ eruptions + rand(1), faithful))
#' 
#' @keywords distribution 
#' @keywords regression 
#' 

#
##########################################
# Random number generation in modeling: for use in model formulas
rand = function(df=1, rdist=rnorm, args=list(), nr, seed=NULL ){
	if(missing(nr)) {
		nr <- length(get( ls( envir=parent.frame())[1], envir=parent.frame()))
	}
	if (!is.null(seed)){
		set.seed(seed)
	}

	result <-  matrix( do.call( rdist, args=c(list(n=df*nr), args) ), nr=nr ) 
#	colnames(result) <- paste('rand',1:df,sep="")
	return(result)
}
