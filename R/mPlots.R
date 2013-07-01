

tryCatch(utils::globalVariables( c('x','y','color','size','logx','logy')),
         error=function(e) message('Looks like you should update R.'))

#' Extract data from a data frame using a formula interface
#' 
#' Uses the full model syntax.
#' 
#' @param data a data frame
#' @param formula a formula. 
#'   The right-hand side selects variables;  
#'   the left-hand side, if present, is used to set row names. 
#'   A \code{.} on the right-hand side
#'   indicates to use all variables not in the LHS.
#' @param intercept a logical indicating whether to include the intercept in the
#'   model default: FALSE (no intercept)
#' @examples
#' getVarFormula(~wt + mpg, data=mtcars)
#' @export

getVarFormula <- function(formula, data=parent.frame(), intercept=FALSE){
  x <- model.matrix(formula, data=data)
  attr(x, "assign") <- NULL
  attr(x, "contrasts") <- NULL
  kill.ind <- which( colnames(x)=="(Intercept)")
  if ( !intercept & length(kill.ind) > 0 ) x <- x[, -kill.ind]
  if ( length(formula) > 2 ) {
    lhsvar <- all.vars( formula[[2]] )
    rnames <- as.character( data[[lhsvar]] )
    rownames(x) <- rnames
  }
  return(x)
}

#' Interactive plotting interfaces 
#'
#' These functions provide a menu selection system (via \pkg{manipulate}) so that 
#' different aspects of a plot can be selected interactively.  The \code{ggplot2}
#' command for generating the plot currently being displayed can be copied to the console,
#' whence it can be copied to a document for later direct, non-interactive use.
#' 
#' @rdname mPlots
#' @aliases mPlots, mScatter 
#' @param data a data frame containing the variables that might be used in the plot.
#' @param system which graphics system to use (initially) for plotting (\pkg{ggplot2} 
#' or \pkg{lattice}).  A check box will allow on the fly change of plotting system.
#' @param show a logical, if \code{TRUE}, the code will be displayed each time the plot is 
#' changed.
#' @return Nothing.  Just for side effects.  
#' @export


mScatter <- function(data, system=c("lattice", "ggplot2"), show=FALSE) {
  .require_manipulate()
  .try_require(c("ggplot2","lattice"))
  system <- match.arg(system)
  keyDefault <- ifelse ( system == "lattice", "none", "right" )
  df <- substitute(data)
  nm <- .varsByType(head(data))
  # nm$q is the quantitative variables.
  snames <- .NAprepend(nm$all)
  cnames <- .NAprepend(nm$c)
  mnames <- list("none", linear="linear", "smoother")
  lnames <- list("none","top","right","left",
                 "N (lattice)" = "N", "NE (lattice)" = "NE", 
                 "E (lattice)" = "E", "SE (lattice)" = "SE", 
                 "S (lattice)" = "S", "SW (lattice)" = "SW", 
                 "W (lattice)" = "W", "NW (lattice)" = "NW")
  sysnames <- list("ggplot2","lattice")
  manipulate( { p<-.doScatter(df, show, system=system, x=x, y=y, color=color, size=size,
                             facet=facet, logx=logx, logy=logy, model=model, key=key) },
             show = button("Show Expression"),
             system = picker(sysnames, initial=system, label="Graphics System"),
             x = picker(nm$q, initial=nm$q[[1]], label="x axis"),
             y = picker(nm$q, initial=nm$q[[2]], label="y axis"),
             color = picker(snames, initial="none ", label="Color"),
             size = picker(snames, initial="none ", label="Size (ggplot only)"),
             facet = picker(cnames, initial="none ", label="Facets"),
             logx = checkbox(label="log x-axis"),
             logy = checkbox(label="log y-axis"),
             key = picker(lnames, label="key", initial=keyDefault),
             model = picker(mnames, initial="none", label="Model")
  )
}

# Utilities
# Pull out the names of the quantitative and categorical variables in a data frame

.varsByType = function(data) {
  # Utility function for converting a vector of names into a list.
  v2list <- function(nms) {
    res = list()
    res[nms] <- nms
    return(res)
  }
  nm = names(data)
  type = nm
  for (k in 1:length(data)) type[k] <- class(data[[k]])
  numberNames <- v2list(nm[type %in% c("integer", "numeric")])
  factorNames <- v2list(nm[type %in% c("factor", "character", "logical", "ordered")])
  return( list( c=factorNames, q=numberNames, all=v2list(nm) ) )
}
# Prepend a list with NA for optional items
.NAprepend <- function(L) {
  c(list(`none `=NA), L)
}

#
# Converting a vector of names into a list.
.v2list <- function(nms) {
  res = list()
  res[nms] <- nms
  return(res)
}


.doScatter <- function(data, show=FALSE, 
					  system=c('ggplot2','lattice'), 
					  x=NA, y=NA, color=NA, 
					  size=NA, facet=NA, logx=FALSE, 
					  logy=FALSE, model="", key="right")
{
  system <- match.arg(system)
  vals <- list(data=data, x=x, y=y, color=color, size=size, 
			   facet=facet, logx=logx, logy=logy, model=model, key=key)

  s <- .scatterString(vals, system)
  if (show) cat(paste(s, "\n"))
  p <- eval(parse(text=s))
  print(p)
  return(p)
}

# Scatter plots
.scatterString <- function(s, system=c('ggplot2', 'lattice'))
{
  #  res <- paste("ggplot(data=", s$data, ")", sep="")
  #    res<-paste(res, "+geom_point(aes(x=", s$x, ", y=", s$y, "))", sep="")
  system=match.arg(system)
  if (system == "ggplot2") {
	  res <- paste("ggplot(data=", s$data, ", aes(x=", s$x, ", y=", s$y, "))", sep="")
	  res <- paste(res, "+geom_point()", sep="")
	  if (!is.null(s$color) && !is.na(s$color))
		res<-paste(res, "+aes(colour=", s$color, ")", sep="")
	  if (!is.null(s$size) && !is.na(s$size))
		res<-paste(res, "+aes(size=", s$size, ")", sep="")
	  if (s$logx)
		res <- paste(res, "+scale_x_log10()", sep="")
	  if (s$logy)
		res <- paste(res, "+scale_y_log10()", sep="")
	  if (!is.null(s$facet) && !is.na(s$facet)) # why do I need both?
		res<-paste(res, "+facet_wrap(~", s$facet, ", ncol=4)", sep="")
	  if (s$model=="linear")
		res <- paste(res, "+ stat_smooth(method=lm)")
	  if (s$model=="smoother")
		res <- paste(res, "+ stat_smooth(method=loess)") 
    if (s$key %in% c('none','top','bottom','left','right')) {
      res <- paste(res, '+ theme(legend.position="', s$key, '")', sep="")
    }  
    
  } else {
	  res <- paste( "xyplot( ", s$y , " ~ ", s$x, sep="")
	  if (!is.null(s$facet) && !is.na(s$facet)) # why do I need both?
		  res <- paste(res, " | ", s$facet)
	  res <- paste(res, ", data=", s$data, sep="")
	  if (!is.null(s$color) && !is.na(s$color))
		res<-paste(res, ", groups=", s$color, sep="")
	  scales <- character(0)
	  if (s$logx)
		scales <- "x=list(log=TRUE)"
	  if (s$logy)
		scales <- c(scales, "y=list(log=TRUE)")
	  scales <- paste(scales, collapse=", ")
	  if (nchar(scales) > 0) {
		  scales <- paste(", scales=list(", scales, ")", sep="")
		  res <- paste( res, scales, sep="" )
	  }
	  if (s$model=="linear")
		res <- paste(res, ', type=c("p","r")', sep ="")
	  if (s$model=="smoother")
		res <- paste(res, ', type=c("p","smooth")', sep ="")
    if (s$key %in% c('top','bottom','left','right')) {
      res <- paste(res, ', auto.key=list(space="', s$key, '")', sep="")
    }
    if (s$key %in% c('N','NE','E','SE','S','SW','W','NW')) {
      dir2pos <- list(
        'N'  = 'c(.5,1)', 
        'NE' = 'c(1,1)', 
        'E'  = 'c(1,.5)', 
        'SE' = 'c(1,0)', 
        'S'  = 'c(.5,0)', 
        'SW' = 'c(0,0)', 
        'W'  = 'c(0,.5)', 
        'NW' = 'c(0,1)'
        ) 
      res <- paste(res, ', auto.key=list(corner=', dir2pos[[s$key]], ')', sep="")
    }

	  res <- paste(res, ")", sep="")
  }
  cat(res)
  
  return(res)
}

