#' Delta method on data frames
#'
#' An expansion of the capabilities of \code{\link{deltaMethod}} from the 
#' \pkg{car} package.  

#' @rdname deltaMethod
#' @name deltaMethod
#' @aliases deltaMethod.data.frame
#' @importFrom car deltaMethod
#' @examples
#' if (require(mosaicData)) {
#'   C_p <- 4.182 / 60 # / 60 because measureing m in L/min
#'   exprforQ <- "(T.cold.out - T.cold.in) * C_p * m.cold"
#'   deltaMethod( HeatX[, c("T.cold.in","T.cold.out","m.cold")], exprforQ, c(1,1,.5) )
#'   # This is just wordier in this example, but would allow the uncertainties to vary 
#'   # from row to row.
#' 
#'   HeatX3 <- transform(HeatX, 
#'                     u.cold.in=1, u.cold.out=1, u.hot.in=1, u.hot.out=1, 
#'                     u.m.cold=0.5, u.m.hot=0.5)
#'   deltaMethod( HeatX3[, c("T.cold.in","T.cold.out","m.cold")], exprforQ, 
#'                HeatX3[, c("u.cold.in", "u.cold.out", "u.m.cold")])
#'   # Rather than specifying two data frames, we can use subsetting instead
#'   deltaMethod( HeatX3,  exprforQ, 
#'      estimates=c("T.cold.in","T.cold.out","m.cold"), 
#'      uncertainties=c("u.cold.in", "u.cold.out", "u.m.cold"))
#'   # Can also specify vcov. as a matrix or list of matrices:
#'   deltaMethod(HeatX[, c("T.cold.in","T.cold.out","m.cold")], exprforQ, 
#'     vcov. = diag(c(1,1,.5)^2) )
#'   deltaMethod(HeatX[, c("T.cold.in","T.cold.out","m.cold")], exprforQ, 
#'     vcov. = list( diag(c(1,1,.5)^2), diag(c(1,2,.8)^2) ) )
#' }
#'    
#' @param object a data frame containing measured quantities
#' @param g a quoted string that is describes the function of the parameter estimates to be 
#' evaluated; see \code{\link[car]{deltaMethod}} for details.
#' @param uncertainties a data frame with the same dimension as \code{object} or a numeric 
#' vector containing the uncertainties on each measured value
#' in \code{object} or a matrix providing a variance-covariance matrix for the uncertainties.
#' If a named vector, and \code{estimates} is \code{NULL}, the names of the vector will
#' be used for \code{estimates}.  This makes it possible to specify only 
#' \code{object}, \code{g}, and \code{uncertainties} to handle many situations.  
#' Alternatvely, if \code{estimates} is not \code{NULL}, then uncertainties may be a vector
#' of names or integers used to select columns from \code{object}.  There is one potentially ambiguous 
#' case: It is not possible to specify the uncertainties as a vector of integers if \code{estimates}
#' is not \code{NULL} -- such integers will be treated as column numbers for subsetting.
#' If \code{uncertaintites} is not a matrix, independece is assumed and the 
#' variance-covariance matrix is created under that assumption.
#' Matching of uncertainties to measured values is by position, so 
#' names are irrelevant.
#' Uncertainties will be converted into a covariance matrix assuming independence.
#' @param vcov. a covariance matrix or a list of covariance matrices.  Only one of 
#' \code{vcov.} and \code{uncertainties} may be defined.
#' @param func a quoted string used to annotate output. 
#' The default of func = g is usually appropriate.
#' @param constants This argument is a named vector whose elements are constants that are 
#' used in the f argument. This is needed only when the function is called from within 
#' another function to comply to R scoping rules. 
#' @param estimates a vector of column names or column numbers used to specify a subset of \code{object}
#' containing the measured/estimated quantities.
#' @param measurements an alternative name for \code{estimates}
#' @param ... additional arguments passed through to \code{deltaMethod} in the \pkg{car} 
#' package.
#' @seealso \code{deltaMethod} in the \pkg{car} package.
#' @export
 
deltaMethod.data.frame <- function(object, g, uncertainties, estimates=measurements, func=g, constants=c(), 
                                   measurements=NULL, vcov., ...) {

  missingArgs <- c(u=missing(uncertainties), v= missing(vcov.))
  if (! sum(missingArgs) == 1 ) {
    stop("Exactly one of uncertainty and vcov. must be specified.")
  }
  
  if (! missingArgs['u'] && is.vector(uncertainties) ) {
    if ( is.null(names(uncertainties)) ) {
      if (is.integer(uncertainties) || is.character(uncertainties) ) {
        uncertainties <- subset(object, select=estimates)
      }
    } else {
      if (is.null(estimates))  estimates <- names(uncertainties)
    }
  }

  if (!is.null (estimates)) {
    estimateData <- subset(object, select=estimates)
  } else {
    estimateData <- subset(object, select=intersect(all.vars(parse(text=g)), names(object)))
  }
  
  if (! missingArgs['v']) {
    if (! is.list(vcov.) ) { 
      vcov. <- lapply(seq_len(nrow(estimateData)), function(...) { vcov. } ) 
    }
    if (length(vcov.) != nrow(estimateData)) {
      vcov. <- lapply( rep( (1:length(vcov.)), length.out = nrow(estimateData) ),
                       function(i) vcov.[[i]] )
      warning("Recycling vcov. -- Is that what you wanted?")
    }
    
    res <- 
      do.call(
        "rbind",
        lapply( seq_len(nrow(estimateData)), 
                function(r) { cbind( estimateData[r,], 
                                     car::deltaMethod( unlist(estimateData[r,]), g=g, 
                                                       vcov.=vcov.[[r]], 
                                                       func=func, constants=constants, ...)
                )
                }
        ) ) 
    attr(res,"vcov") <- vcov.
    return(res)
  } 
  
  if (is.data.frame(uncertainties)) {
    if (ncol(estimateData) != ncol(uncertainties) ) 
      stop( "estimates and uncertainties are not of equal width")
    message("Converting uncertainties to a covariance matrix assuming independence ...")
    combined <- cbind( estimateData, uncertainties)
    w <- ncol(estimateData)
    return( 
      do.call(
        "rbind",
        lapply( seq_len(nrow(combined)), 
                function(r) { cbind( combined[r,], 
                                     car::deltaMethod( unlist(combined[r,1:w]), g=g, 
                                                       vcov.=diag(unlist(combined[r, (w+1):(2*w)])^2), 
                                                       func=func, constants=constants, ...)
                )
                }
        ) ) )
  } 

  if (! is.numeric(uncertainties) ) stop("I don't know what to do with that kind of uncertainties")
  
  if (!is.matrix(uncertainties)) {
    uncertainties <- diag(uncertainties^2)
    message("Converting uncertainties to a covariance matrix assuming independence ...")
  }
  
  do.call( "rbind", lapply( seq_len(nrow(estimateData)), 
         function(r) { cbind( estimateData[r,], 
                              car::deltaMethod( unlist(estimateData[r,]), g=g, vcov.=uncertainties, 
                                                func=func, constants=constants, ...)) }
  ))
}

