#' Delta method on data frames
#'
#' An expansion of the capabilities of \code{\link{deltaMethod}} from the 
#' \pkg{car} package.  

#' @rdname deltaMethod
#' @name deltaMethod
#' @export
#' @examples
#' C_p <- 4.182 / 60 # / 60 because measureing m in L/min
#' exprforQ <- "(T.cold.out - T.cold.in) * C_p * m.cold"
#' deltaMethod( HeatX[, c("T.cold.in","T.cold.out","m.cold")], exprforQ, c(1,1,.5) )
#' # This is just wordier in this example, but would allow the uncertainties to vary 
#' # from row to row.
#' 
#' HeatX3 <- transform(HeatX, 
#'                     u.cold.in=1, u.cold.out=1, u.hot.in=1, u.hot.out=1, 
#'                     u.m.cold=0.5, u.m.hot=0.5)
#' deltaMethod( HeatX3[, c("T.cold.in","T.cold.out","m.cold")], exprforQ, 
#'              HeatX3[, c("u.cold.in", "u.cold.out", "u.m.cold")])
#' # Rather than specifying two data frames, we can use subsetting instead
#' deltaMethod( HeatX3,  exprforQ, 
#'    estimates=c("T.cold.in","T.cold.out","m.cold"), 
#'    uncertainties=c("u.cold.in", "u.cold.out", "u.m.cold"))
#'    
#' @param object a data frame containing measured quantities
#' @param g a quoted string that is describes the function of the parameter estimates to be 
#' evaluated; see \code{\link[car]{deltaMethod}} for details.
#' @param uncertainties a data frame with the same dimension as \code{object} or numeric vector 
#' of length \code{ncol(object)} containing the uncertainties on each measured value
#' in \code{object} or a matrix providing a variance-covariance matrix for the uncertainties.  
#' Alternatvely, if \code{estimates} is not \code{NULL}, then uncertainties may be a vector
#' of names or integers used to select columns from \code{object}.  There is one potentially ambiguous 
#' case. It is not possible to specify the uncertainties as a vector of integers if \code{estimates}
#' is not \code{NULL} -- such integers will be treated as column numbers for subsetting.
#' If \code{uncertaintites} is not a matrix, independece is assumed and the 
#' variance-covariance matrix is created under that assumption.
#' Matching of uncertainties to measured values is by position, so 
#' names are irrelevant.
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
 
deltaMethod.data.frame <- function(object, g, uncertainties, estimates=measurements, func=g, constants=c(), 
                                   measurements=NULL, ...) {
#  if (! require(car) ) stop( "You must install the car package to use deltaMethod()." )
  if (! require(plyr) ) stop( "You must install the plyr package to use deltaMethod() on a data frame." )

  
  if (!is.null (estimates)) {
    estimateData <- subset(object, select=estimates)
    if (is.vector(estimates) && (is.integer(uncertainties) || is.character(uncertainties) ) ) {
      uncertainties <- subset(object, select=estimates)
    }
  } else {
    estimateData <- object
  }
  
  
  if (is.data.frame(uncertainties)) {
    if (ncol(estimateData) != ncol(uncertainties) ) stop( "Data frames are not of equal width")
    combined <- cbind( estimateData, uncertainties)
    w <- ncol(estimateData)
    return( ddply( combined, names(combined), 
         function(d) { car::deltaMethod( unlist(d[1,1:w]), g=g, vcov.=diag(unlist(d[1, (w+1):(2*w)])^2), 
                                    func=func, constants=constants, ...) }
  ) )
  } 

  if (! is.numeric(uncertainties) ) stop("I don't know what to do with that kind of .vcov")
  
  if (!is.matrix(uncertainties)) {
    uncertainties <- diag(uncertainties^2)
    message("Converting uncertainties to a var-covar matrix assuming independence ...")
  }
  
  ddply( estimateData, names(estimateData), 
         function(x) { car::deltaMethod( unlist(x[1,]), g=g, vcov.=uncertainties, func=func, constants=constants, ...) }
  )
}

