

tryCatch(utils::globalVariables( c('x','y','color','size','logScales','key','nbins', 'plotType', 'flipCoords')),
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


# Utilities
# Pull out the names of the quantitative and categorical variables in a data frame

.varsByType = function(data) {
  vnames = names(data)
  type = vnames
  for (k in 1:length(data)) type[k] <- class(data[[k]])
  numberNames <- .v2list(vnames[type %in% c("integer", "numeric")])
  factorNames <- .v2list(vnames[type %in% c("factor", "character", "logical", "ordered")])
  return( list( c=factorNames, q=numberNames, all=.v2list(vnames) ) )
}
# Prepend a list with NA for optional items
.NAprepend <- function(L) {
  c(list(`none `=NA), L)
}

#
# Converting a vector of names into a list.
.v2list <- function(names) {
  res = list()
  res[names] <- names
  return(res)
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
#' @param default default type of plot to create.
#' @param system which graphics system to use (initially) for plotting (\pkg{ggplot2} 
#' or \pkg{lattice}).  A check box will allow on the fly change of plotting system.
#' @param show a logical, if \code{TRUE}, the code will be displayed each time the plot is 
#' changed.
#' @return Nothing.  Just for side effects.  
#' @export
#' 
mPlot <- function(data, 
  default=plotType,
  system=c('lattice','ggplot2'),
  show=FALSE) 
{
  if (missing(default))  default <- 'scatter' 
  plotTypes <- c('scatter', 'boxplot', 'violin', 'histogram', 
                'density', 'frequency polygon', 'xyplot')
  default <- match.arg(default, plotTypes)
  system <- match.arg(system)
  dataName <- substitute(data)
  if (default == 'xyplot') default <- 'scatter'
  if (default %in% c('scatter','boxplot','violin')) {
    return( 
      eval(parse(text=paste("mScatter(", dataName, ", default=default, system=system, show=show)") ) ) 
    )
  }
    return( 
      eval(parse(text=paste("mUniplot(", dataName, ", default=default, system=system, show=show)") ) ) 
    )
}
                  

#' @export

mScatter <- function(data, default = c('scatter','boxplot','violin'),
                     system=c("lattice", "ggplot2"), show=FALSE) {

  .require_manipulate()
  .try_require(c("ggplot2","lattice"))
  system <- match.arg(system)
  keyDefault <- ifelse ( system == "lattice", "none", "right" )
  df <- substitute(data)
  variables <- .varsByType(head(data))
  # variables$q is the quantitative variables.
  plotnames <- list("scatter", "boxplot", "violin")
  snames <- .NAprepend(variables$all)
  cnames <- .NAprepend(variables$c)
  mnames <- list("none", linear="linear", "smooth")
  lnames <- list("none","top","right","left",
                 "N (lattice)" = "N", "NE (lattice)" = "NE", 
                 "E (lattice)" = "E", "SE (lattice)" = "SE", 
                 "S (lattice)" = "S", "SW (lattice)" = "SW", 
                 "W (lattice)" = "W", "NW (lattice)" = "NW")
  sysnames <- list("ggplot2","lattice")
  manipulate( { .doScatter(df, variables, show=show, system=system, x=x, y=y, plotType=plotType, 
                           flipCoords = flipCoords, color=color, size=size, facet=facet, 
                           logScales=logScales, model=model, key=key) },
             show = button("Show Expression"),
             system = picker(sysnames, initial=system, label="Graphics System"),
             plotType = picker(plotnames, initial=default, label="Type of plot      "),
             x = picker(variables$all, initial=variables$q[[1]], label="any variable (x)   "),
             y = picker(variables$q, initial=variables$q[[2]],   label="quant. variable (y)"),
             flipCoords = checkbox(label="Flip coordinates"),
             color = picker(snames, initial="none ", label="Color"),
             size = picker(snames, initial="none ", label="Size (ggplot only)"),
             facet = picker(cnames, initial="none ", label="Facets"),
             logScales = picker(list("none","x","y","both"), initial="none", label="log scales"),
#             logx = checkbox(label="log x-axis"),
#             logy = checkbox(label="log y-axis"),
             key = picker(lnames, label="key", initial=keyDefault),
             model = picker(mnames, initial="none", label="Model")
  )
}

.doScatter <- function(dataName, variables, show=FALSE, 
					  system=c('ggplot2','lattice'), 
            plotType=c('scatter','boxplot','violin'),
					  x=NA, y=NA, color=NA, 
					  size=NA, facet=NA, logScales='none', flipCoords=FALSE,
					  model="", key="right")
{
  system <- match.arg(system)
  plotType <- match.arg(plotType)
  vals <- list(dataName=dataName, x=x, y=y, color=color, size=size, plotType=plotType, 
               flipCoords=flipCoords, facet=facet, logScales=logScales , model=model, key=key)
  
  s <- .scatterString(vals, system, variables=variables)
  if (show) cat(paste("\n", s, "\n"))
  p <- eval(parse(text=s))
  print(p)
  return(invisible(p))
}

# Scatter plots
.scatterString <- function(s, system=c('ggplot2', 'lattice'), variables)
{
  #  res <- paste("ggplot(data=", s$data, ")", sep="")
  #    res<-paste(res, "+geom_point(aes(x=", s$x, ", y=", s$y, "))", sep="")
  geom <- c(scatter="geom_point()", boxplot="geom_boxplot()", violin="geom_violin()")
  system=match.arg(system)
  s$logx <- s$logScales %in% c('both','x')
  s$logy <- s$logScales %in% c('both','y')
  if (s$plotType %in% c('boxplot','violin') &&  (s$x %in% variables$q) ) {
    s$x <- paste('ntiles(', s$x,')', sep="")
  }
  if (system == "ggplot2") {
	  res <- paste("ggplot(data=", s$dataName, ", aes(x=", s$x, ", y=", s$y, "))", sep="")
	  res <- paste(res, " + ", geom[s$plotType], " ", sep="")
	  if (!is.null(s$color) && !is.na(s$color))
		res<-paste(res, " + aes(colour=", s$color, ")", sep="")
	  if (!is.null(s$size) && !is.na(s$size))
		res<-paste(res, " + aes(size=", s$size, ")", sep="")
	  if (s$logx) res <- paste(res, " + scale_x_log10()", sep="")
	  if (s$logy) res <- paste(res, " + scale_y_log10()", sep="")
	  if (!is.null(s$facet) && !is.na(s$facet)) # why do I need both?
		res<-paste(res, " + facet_wrap(~", s$facet, ", ncol=4)", sep="")
	  if (s$model=="linear") res <- paste(res, " + stat_smooth(method=lm)")
	  if (s$model=="smooth") res <- paste(res, " + stat_smooth(method=loess)") 
    if (s$key %in% c('none','top','bottom','left','right')) {
      res <- paste(res, ' + theme(legend.position="', s$key, '")', sep="")
    }  
    if ( s$flipCoords) {
      res <- paste(res, ' + coord_flip()', sep="")
    }
    
  } else {
    plotname <- c(scatter='xyplot', boxplot='bwplot', violin='bwplot')
    if (s$flipCoords) {
	    res <- paste( plotname[s$plotType], "( ", s$x , " ~ ", s$y, sep="")
    } else {
	    res <- paste( plotname[s$plotType], "( ", s$y , " ~ ", s$x, sep="")
    }
    if (!is.null(s$facet) && !is.na(s$facet)) # why do I need both?
      res <- paste(res, " | ", s$facet)
    res <- paste(res, ", data=", s$dataName, sep="")
    if (s$plotType == 'violin')
      res <- paste(res, ", panel=panel.violin", sep="")
    
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
	  if (s$model=="smooth")
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
  return(res)
}

#' @rdname mPlots  
#' @export

mUniplot <- function(data, default=c('histogram','density', 'frequency polygon'),
                     system=c("lattice", "ggplot2"), show=FALSE) {
  .require_manipulate()
  .try_require(c("ggplot2","lattice"))
  system <- match.arg(system)
  default <- match.arg(default)
  keyDefault <- ifelse ( system == "lattice", "none", "right" )
  df <- substitute(data)
  plotnames <- list("histogram", "density", "frequency polygon")
  
  variables <- .varsByType(head(data))
  # variables$q is the quantitative variables.
  snames <- .NAprepend(variables$all)
  cnames <- .NAprepend(variables$c)
  lnames <- list("none","top","right","left",
                 "N (lattice)" = "N", "NE (lattice)" = "NE", 
                 "E (lattice)" = "E", "SE (lattice)" = "SE", 
                 "S (lattice)" = "S", "SW (lattice)" = "SW", 
                 "W (lattice)" = "W", "NW (lattice)" = "NW")
  sysnames <- list("ggplot2","lattice")
  manipulate( { .doUniplot(df, variables=variables, show=show, system=system, plotType=plotType, x=x, 
                           nbins=nbins, color=color, 
                           facet=facet, 
                           model=model, key=key) },
              show = button("Show Expression"),
              system = picker(sysnames, initial=system, label="Graphics system"),
              plotType = picker(plotnames, initial = default, label="Plot type"),
              x = picker(variables$q, initial=variables$q[[1]], label="x axis"),
              # y = picker(variables$q, initial=variables$q[[2]], label="y axis"),
              nbins = slider(2, 100, initial=25, label="Number of bins"),
              color = picker(snames, initial="none ", label="Color"),
#              size = picker(snames, initial="none ", label="Size (ggplot only)"),
              facet = picker(cnames, initial="none ", label="Facets"),
#              logx = checkbox(label="log x-axis"),
#              logy = checkbox(label="log y-axis"),
              key = picker(lnames, label="key", initial=keyDefault)
  )
}


.doUniplot <- function(dataName, variables=variables, show=FALSE, 
                       system=c('ggplot2','lattice'), 
                       plotType=c('histogram', 'densityplot', 'frequency polygon'),
                       x=NA, 
                       nbins=nbins, color=NA, 
                       # size=NA, 
                       facet=NA, 
                       # logx=FALSE, logy=FALSE, 
                       model="", key="right")
{
  system <- match.arg(system)
  plotType <- match.arg(plotType)
  vals <- list(dataName=dataName, plotType=plotType, x=x, nbins = nbins, 
               color=color, 
               # size=size, 
               facet=facet, 
               # logx=logx, logy=logy, 
               key=key)
  
  s <- .uniplotString(vals, system, variables)
  if (show) cat(paste("\n", s, "\n"))
  p <- eval(parse(text=s))
  print(p)
  return(invisible(p))
}

# 1-variable plots
.uniplotString <- function(s, system=c('ggplot2', 'lattice'), variables)
{
  geom <- c(`histogram`='', `densityplot`=', geom="line"',    `frequency polygon`=', geom="line"')
  stat <- c(`histogram`='', `densityplot`=', stat="density"', `frequency polygon`=', stat="bin"')
  
  system=match.arg(system)
  adjust <- 10 / s$nbins
  binwidth <- eval( parse( text= paste("range( ~", s$x, ", data=", s$dataName,")"))) / s$nbins
  if (system == "ggplot2") {
    res <- paste("qplot( data=", s$dataName, ", x=", s$x, sep="")
    res <- paste(res, geom[s$plotType], stat[s$plotType], sep="")
    if (s$plotType %in% c('histogram', 'frequency polygon')) {
      res <- paste(res, ", binwidth=", signif(binwidth,2), sep="")
    } else {
      res <- paste(res, ", adjust=", signif(adjust,2), sep="")
    }
    res <- paste( res, " )")
    if (!is.null(s$color) && !is.na(s$color))
      res<-paste(res, " + aes(colour=", s$color, ")", sep="")
#    if (!is.null(s$size) && !is.na(s$size))
#      res<-paste(res, " + aes(size=", s$size, ")", sep="")
#    if (s$logx)
#      res <- paste(res, " + scale_x_log10()", sep="")
#    if (s$logy)
#      res <- paste(res, " + scale_y_log10()", sep="")
    if (!is.null(s$facet) && !is.na(s$facet)) # why do I need both?
      res<-paste(res, " + facet_wrap(~", s$facet, ", ncol=4)", sep="")
    if (s$key %in% c('none','top','bottom','left','right')) {
      res <- paste(res, ' + theme(legend.position="', s$key, '")', sep="")
    }  
    
  } else {
    plotName <- c(`histogram`='histogram', `densityplot`='densityplot', `frequency polygon`='freqpolygon')
    res <- paste( plotName[s$plotType], "( ", " ~ ", s$x, sep="")
    if (!is.null(s$facet) && !is.na(s$facet)) # why do I need both?
      res <- paste(res, " | ", s$facet)
    res <- paste(res, ", data=", s$dataName, sep="")
    if (!is.null(s$color) && !is.na(s$color))
      res<-paste(res, ", groups=", s$color, sep="")
    if (s$plotType %in% c('histogram', 'frequency polygon')) {
      res <- paste(res, ", width =", signif(binwidth,2), sep="")
    } else {
      res <- paste(res, ", adjust=", signif(adjust,2), sep="")
    }
    scales <- character(0)
#    if (s$logx)
#      scales <- "x=list(log=TRUE)"
#    if (s$logy)
#      scales <- c(scales, "y=list(log=TRUE)")
#    scales <- paste(scales, collapse=", ")
#    if (nchar(scales) > 0) {
#      scales <- paste(", scales=list(", scales, ")", sep="")
#      res <- paste( res, scales, sep="" )
#    }
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
  
  return(res)
}
