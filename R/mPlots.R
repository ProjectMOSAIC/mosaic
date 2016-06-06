

tryCatch(utils::globalVariables( c('picker','button','slider','checkbox','x','y','color','size','logScales','key','nbins', 'plotType', 'flipCoords', 'group', 'projection')),
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
  numberNames <- .v2list(vnames[sapply(data, function(x) inherits(x, c("integer", "numeric", "POSIXct")))])
  factorNames <- .v2list(vnames[sapply(data, function(x) inherits(x, c("factor", "character", "logical", "ordered")))])
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

#' Interactive plotting 
#'
#' These functions provide a menu selection system (via \pkg{manipulate}) so that 
#' different aspects of a plot can be selected interactively.  
#' The \pkg{ggplot2} or \pkg{lattice}
#' command for generating the plot currently being displayed can be copied to the 
#' console, whence it can be copied to a document for later direct, non-interactive 
#' use.
#' 
#' Only \code{mPlot} is required by end users.  The other plotting functions 
#' are dispatched based on the value of \code{default}.  Furthermore, \code{\link{mplot}} 
#' will dispatch \code{mPlot} when provided a data frame.
#' 
#' @details
#' Currently maps are only supported in \pkg{ggplot2} and not in \pkg{lattice}.
#' 
#' @rdname mPlotting
#' @aliases mPlot
#' @param data a data frame containing the variables that might be used in the plot.
#' Note that for maps, the data frame must contain coordinates of the polygons 
#' comprising the map and a variable for determining which corodiantes are part
#' of the same region.  See \code{\link{sp2df}} for one way to create such
#' a data frame.  Typically \code{\link{merge}} will be used to combine the map
#' data with some auxilliary data to be displayed as fill color on the map, although
#' this is not necessary if all one wants is a map.
#' @param format a synonym for \code{default}.
#' @param default default type of plot to create; one of 
#' \code{"scatter"},
#' \code{"jitter"},
#' \code{"boxplot"},
#' \code{"violin"},
#' \code{"histogram"},
#' \code{"density"},
#' \code{"frequency polygon"},
#' \code{"xyplot"}, 
#' or
#' \code{"map"}.  Unique prefixes suffice.
#' @param system which graphics system to use (initially) for plotting (\pkg{ggplot2} 
#' or \pkg{lattice}).  A check box will allow on the fly change of plotting system.
#' @param show a logical, if \code{TRUE}, the code will be displayed each time the plot is 
#' changed.
#' @param title a title for the plot
#' @param \dots additional arguments 
#' @param lazy_data a formula describing the data.  It is unlikely that most users will
#' need to change this from its default value.  It is hear to facilitate the 
#' \code{mplot()} wrapper.
#' @return Nothing.  Just for side effects.  
#' @examples
#' \dontrun{
#' mPlot(HELPrct, format="scatter")
#' mPlot(HELPrct, format="density")
#' }
#' @export

mPlot <- function(data, 
  format,
  default = format,
  system=c('lattice','ggplot2'),
  show=FALSE, 
  title="",
  ...,
  lazy_data = NULL)
{
  
  if (is.null(lazy_data)) lazy_data <- lazyeval::f_capture(data) 
  plotTypes <- c('scatter', 'jitter', 'boxplot', 'violin', 'histogram', 
                'density', 'frequency polygon', 'xyplot', 'map')
  
  if (missing(default) & missing(format)) {
    choice <- 
      menu(title = "Choose a plot type.",
           choices = c(
             "1-variable (histogram, density plot, etc.)",
             "2-variable (scatter, boxplot, etc.)", 
             "map")
      )
    default <- c("histogram", "scatter", "map") [choice]
  }
  default <- match.arg(default, plotTypes)
  system <- match.arg(system)
  if (default == 'xyplot') default <- 'scatter'
  if (default %in% c('scatter','jitter','boxplot','violin')) {
    return( 
      mScatter(lazy_data, default = default, system = system, show = show, title = title)
    )
  }
  if (default == "map") {
    return(
      mMap(lazy_data, default = default, system = system, show = show, title = title)
    )
  }
  return( 
    mUniplot(lazy_data, default = default, system = system, show = show, title = title)
  )
}

#' @rdname mPlotting
#' @export

mMap <- function(lazy_data, default = 'map',
        system="ggplot2", 
        show=FALSE, title=title, ...) {
  
  .require_manipulate_namespace()
  # system <- "ggplot2" # only handling ggplot2 for now.
  system <- match.arg(system)
  keyDefault <- ifelse ( system == "lattice", "none", "right" )
  data <- lazyeval::f_eval(lazy_data)
  variables <- .varsByType(head(data))
  latid <- min(c(grep("lat", names(variables$q)), length(variables$q)))
  longid <- min(c(grep("lon", names(variables$q)), length(variables$q)))
  groupid <- min(c(grep("group", names(variables$all)), length(variables$all)))
  
  snames <- .NAprepend(variables$all)
  cnames <- .NAprepend(variables$c)
  pnames <- list("mercator", 
                 "sinusoidal", 
                 "cylequalarea, lat0=0", 
                 "cylindrical", 
                 "rectangular, lat0=0", 
                 "gall, lat0=0", 
                 "mollweide", 
                 "gilbert", 
                 "azequidistant", 
                 "azequalarea", 
                 "gnomonic", 
                 "perspective, dist=5", 
                 "orthographic", 
                 "stereographic", 
                 "laue", 
                 "fisheye, n=5", 
                 "newyorker, r=5", 
                 "conic, lat0=0", 
                 "simpleconic, lat0=-20, lat1=20", 
                 "lambert, lat=-20, lat1=20",
                 "albers, lat=-20, lat1=20", 
                 "bonne, lat0=0", 
                 "polyconic", 
                 "aitoff", 
                 "lagrange", 
                 "bicentric, lon0=30",
                 "elliptic, lon0=-90", 
                 "globular", 
                 "vandergrinten", 
                 # "eisenlohr", 
                 "guyou", 
                 "square",
                 "tetra", 
                 "hex", 
                 "harrison, dist=5, angle=0", 
                 "trapezoidal, lat0=-50, lat1=50", 
                 "lune, lat=60, angle=30", 
                 "mecca, lat0=0",
                 "homing, lat0=0", 
                 "sp_mercator", 
                 "sp_albers, lat0=-50, lat1=50")
  lnames <- list("none","top","right","left",
                 "N (lattice)" = "N", "NE (lattice)" = "NE", 
                 "E (lattice)" = "E", "SE (lattice)" = "SE", 
                 "S (lattice)" = "S", "SW (lattice)" = "SW", 
                 "W (lattice)" = "W", "NW (lattice)" = "NW")
  sysnames <- list("ggplot2","lattice")
  manipulate::manipulate( { .doMap(lazy_data, variables, show=show, system=system, 
                       x=x, y=y, 
                       color=color, 
                       group=group,
                       projection=projection,
                       facet=facet, 
                       key=key,
                       title=title) },
              show = manipulate::button("Show Expression"),
              # system = manipulate::picker(sysnames, initial=system, label="Graphics System"),
              x = manipulate::picker(variables$q, initial=variables$q[[longid]], label="longitude (x)"),
              y = manipulate::picker(variables$q, initial=variables$q[[latid]], label="latitude (y)"),
              group = manipulate::picker(variables$all, initial=variables$all[[groupid]], label="region"),
              color = manipulate::picker(snames, initial="none ", label="Color"),
              facet = manipulate::picker(cnames, initial="none ", label="Facets"),
              projection = manipulate::picker(pnames, initial="mercator", label="Projection"),
              key = manipulate::picker(lnames, label="key", initial=keyDefault)
  )
}


.doMap<- function(lazy_data, variables, show=FALSE, 
                  system=c('ggplot2'), 
                  x=NA, y=NA, 
                  color=NA, 
                  group=group,
                  projection=projection,
                  facet=NA,
                  key="right",
                  title="")
{
  system <- match.arg(system)
  if (regexpr(",", projection) > 0) {
    projection <- sub(",", '", ', projection)
  } else {
    projection <- paste(projection, '"', sep="")
  }
  projection <- paste('"',projection, sep="")
  vals <- list(dataName = deparse(lazyeval::f_rhs(lazy_data)), 
               x = x, y = y, 
               color = color, 
               group = group,
               facet = facet, 
               projection = projection,
               key = key,
               title = title)
  
  s <- .mapString(vals, system, variables = variables)
  if (show) cat(paste("\n", s, "\n"))
  p <- eval(parse(text = s))
  print(p)
  return(invisible(p))
}

# maps 
.mapString <- function(s, system=c('ggplot2', 'lattice'), variables)
{
  #  res <- paste("ggplot(data=", s$data, ")", sep="")
  #    res<-paste(res, "+geom_point(aes(x=", s$x, ", y=", s$y, "))", sep="")
  geom <- "geom_polygon()"
  system=match.arg(system)
  if (system == "ggplot2" || TRUE) {
    res <- paste("ggplot(data=", s$dataName, 
                 ",  aes(x=", s$x, ", y=", s$y, 
                 ", group=",  s$group, "))", sep="")
    res <- paste(res, " + ", geom, " ", sep="")
    if (!is.null(s$color) && !is.na(s$color))
      res<-paste(res, " + aes(fill=", s$color, ")", sep="")
    if (!is.null(s$facet) && !is.na(s$facet)) # why do I need both?
      res<-paste(res, " + facet_wrap(~", s$facet, ", ncol=4)", sep="")
    res <- paste( res, '+ coord_map(', s$projection, ')', sep="" )
    if (s$key %in% c('none','top','bottom','left','right')) {
      res <- paste(res, ' + theme(legend.position="', s$key, '")', sep="")
    }  
    res <- paste( res, '+ labs(title="', s$title, '")', sep="")
    res <- paste( res, '+ theme(',
                  'panel.background = element_rect("transparent"),',
                  'plot.background = element_rect("transparent"),',
                  'panel.grid.major = element_line(size = 0),',
                  'panel.grid.minor = element_line(size = 0),',
                  'axis.title = element_blank(),',
                  'axis.line = element_blank(),', 
                  'axis.text = element_blank(),',
                  'axis.ticks = element_blank() )'
                  )
  } 
  return(res)
}

#' @rdname mPlotting
#' @export

mScatter <- function(lazy_data, default = c('scatter','jitter','boxplot','violin','line'),
                     system=c("lattice", "ggplot2"), show=FALSE, title="") {

  .require_manipulate_namespace()
  system <- match.arg(system)
  default <- match.arg(default)
  keyDefault <- ifelse ( system == "lattice", "none", "right" )
  data <- lazyeval::f_eval(lazy_data)
  variables <- .varsByType(head(data))
  # variables$q is the quantitative variables.
  plotnames <- list("scatter", "jitter","boxplot", "violin", "line")
  snames <- .NAprepend(variables$all)
  if (length(variables$all) < 2 || length(variables$q) < 1) {
    stop("data must have at least 2 variables, at least one of which is quantitative")
  }
  cnames <- .NAprepend(variables$c)
  mnames <- list("none", linear="linear", "smooth")
  lnames <- list("none","top","right","left",
                 "N (lattice)" = "N", "NE (lattice)" = "NE", 
                 "E (lattice)" = "E", "SE (lattice)" = "SE", 
                 "S (lattice)" = "S", "SW (lattice)" = "SW", 
                 "W (lattice)" = "W", "NW (lattice)" = "NW")
  sysnames <- list("ggplot2","lattice")
  manipulate::manipulate( { .doScatter(lazy_data, variables, show=show, system=system, x=x, y=y, plotType=plotType, 
                           flipCoords = flipCoords, color=color, size=size, facet=facet, 
                           logScales=logScales, model=model, key=key, title=title) },
             show = manipulate::button("Show Expression"),
             system = manipulate::picker(sysnames, initial=system, label="Graphics System"),
             plotType = manipulate::picker(plotnames, initial=default, label="Type of plot      "),
             x = if (length(variables$q) >= 2) 
               manipulate::picker(variables$all, initial=variables$q[[2]], label="any variable (x)   ")
             else 
               manipulate::picker(variables$all, initial=variables$c[[1]], label="any variable (x)   ")
               ,
             y = manipulate::picker(variables$q, initial=variables$q[[1]],   label="quant. variable (y)"),
             flipCoords = manipulate::checkbox(label="Flip coordinates"),
             color = manipulate::picker(snames, initial="none ", label="Color"),
             size = manipulate::picker(snames, initial="none ", label="Size (ggplot only)"),
             facet = manipulate::picker(cnames, initial="none ", label="Facets"),
             logScales = manipulate::picker(list("none","x","y","both"), initial="none", label="log scales"),
#             logx = manipulate::checkbox(label="log x-axis"),
#             logy = manipulate::checkbox(label="log y-axis"),
             key = manipulate::picker(lnames, label="key", initial=keyDefault),
             model = manipulate::picker(mnames, initial="none", label="Model")
  )
}

.doScatter <- function(lazy_data, variables, show=FALSE, 
					  system=c('ggplot2','lattice'), 
            plotType=c('scatter','jitter','boxplot','violin','line'),
					  x=NA, y=NA, color=NA, 
					  size=NA, facet=NA, logScales='none', flipCoords=FALSE,
					  model="", key="right", title=title)
{
  system <- match.arg(system)
  plotType <- match.arg(plotType)
  vals <- list(dataName = deparse(lazyeval::f_rhs(lazy_data)), x=x, y=y, color=color, size=size, 
               plotType=plotType, flipCoords=flipCoords, facet=facet, 
               logScales=logScales , model=model, key=key, title=title)
  
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
  geom <- c(scatter="geom_point()", jitter="geom_jitter()", boxplot="geom_boxplot()", 
            violin="geom_violin()", line="geom_line()")
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
    res <- paste(res, ' + labs(title="', s$title, '")', sep="")
    if ( s$flipCoords) {
      res <- paste(res, ' + coord_flip()', sep="")
    }
    
  } else {
    plotname <- c(scatter='xyplot', jitter='xyplot', boxplot='bwplot', violin='bwplot', line="xyplot")
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
    res <- paste(res, ', main="', s$title, '"', sep="")
    if (s$plotType == "jitter") {
      res <- paste(res, ', jitter.x=TRUE, jitter.y=TRUE', sep="")
    }
    if (s$plotType == "line" && ! s$model %in% c("linear", "smooth")) {
      res <- paste(res, ', type="l"', sep="")
    }
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
	    res <- paste(res, if (s$plotType == "line") ', type=c("l","r")' else ', type=c("p","r")', sep ="")
	  if (s$model=="smooth")
	    res <- paste(res, if (s$plotType == "line") ', type=c("l","smooth")' else ', type=c("p","smooth")', sep ="")
	  if (s$key %in% c('top','bottom','left','right')) {
	    res <- paste(res, 
	                 ', auto.key=list(space="', s$key, '"',
                   if (s$plotType == "line") ", lines=TRUE, points=FALSE" else "",
                   if (s$key %in% c("top")) ", columns=3" else "",
	                 ')', sep="")
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
      res <- paste(res, ', auto.key=list(',
                   'corner=', dir2pos[[s$key]], 
                   if (s$plotType == "line") ", lines=TRUE, points=FALSE" else "",
                   if (s$key %in% c("N", "S")) ", columns=3" else "",
                   ")",
                   sep="")
    }

	  res <- paste(res, ")", sep="")
  }
  return(res)
}

#' @rdname mPlotting  
#' @export

mUniplot <- function(lazy_data, default=c('histogram','density', 'frequency polygon'),
                     system=c("lattice", "ggplot2"), show=FALSE, title="") {
  .require_manipulate_namespace()
  system <- match.arg(system)
  default <- match.arg(default)
  keyDefault <- ifelse ( system == "lattice", "none", "right" )
  data <- lazyeval::f_eval(lazy_data)
  plotnames <- list("histogram", "density", "frequency polygon")
  
  variables <- .varsByType(head(data))
  # variables$q is the quantitative variables.
  snames <- .NAprepend(variables$all)
  cnames <- .NAprepend(variables$c)
  if (length(variables$q) < 1) stop("data must have at least 1 quantitative variable")
  lnames <- list("none","top","right","left",
                 "N (lattice)" = "N", "NE (lattice)" = "NE", 
                 "E (lattice)" = "E", "SE (lattice)" = "SE", 
                 "S (lattice)" = "S", "SW (lattice)" = "SW", 
                 "W (lattice)" = "W", "NW (lattice)" = "NW")
  sysnames <- list("ggplot2","lattice")
  manipulate::manipulate( 
    { .doUniplot(lazy_data, variables=variables, show=show, system=system, plotType=plotType, x=x, 
                 nbins=nbins, color=color, 
                 facet=facet, 
                 model=model, key=key, title=title) },
    show = manipulate::button("Show Expression"),
    system = manipulate::picker(sysnames, initial=system, label="Graphics system"),
              plotType = manipulate::picker(plotnames, initial = default, label="Plot type"),
              x = manipulate::picker(variables$q, initial=variables$q[[1]], label="x axis"),
              # y = manipulate::picker(variables$q, initial=variables$q[[2]], label="y axis"),
              nbins = manipulate::slider(2, 100, initial=25, label="Number of bins"),
              color = manipulate::picker(snames, initial="none ", label="Color"),
#              size = manipulate::picker(snames, initial="none ", label="Size (ggplot only)"),
              facet = manipulate::picker(cnames, initial="none ", label="Facets"),
#              logx = manipulate::checkbox(label="log x-axis"),
#              logy = manipulate::checkbox(label="log y-axis"),
              key = manipulate::picker(lnames, label="key", initial=keyDefault)
  )
}


.doUniplot <- function(lazy_data, variables=variables, show=FALSE, 
                       system=c('ggplot2','lattice'), 
                       plotType=c('histogram', 'densityplot', 'frequency polygon'),
                       x=NA, 
                       nbins=nbins, color=NA, 
                       # size=NA, 
                       facet=NA, 
                       # logx=FALSE, logy=FALSE, 
                       model="", key="right",
                       title="")
{
  system <- match.arg(system)
  plotType <- match.arg(plotType)
  vals <- list(dataName = deparse(f_rhs(lazy_data)), plotType=plotType, x=x, nbins = nbins, 
               color=color, 
               # size=size, 
               facet=facet, 
               # logx=logx, logy=logy, 
               key=key,
               title=title)
  
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
  gggeoms <- c(`histogram`='geom_histogram', 
               `densityplot`='geom_density',    
               `frequency polygon`='geom_freqpoly')
  
  system=match.arg(system)
  adjust <- 10 / s$nbins
  binwidth <- eval( parse( text= paste("diff(range( ~", s$x, ", data=", s$dataName,", na.rm=TRUE))"))) / s$nbins
  if ( any(is.na(binwidth)) || any(is.nan(binwidth)) ) binwidth <- NULL
  if (system == "ggplot2") {
    res <- paste0("ggplot( data = ", s$dataName, ", aes(x = ", s$x, "))", sep="")
    
    params <- if (s$plotType %in% c('histogram', 'frequency polygon')) {
      paste("binwidth=", signif(binwidth,2), sep="")
    } else {
      paste("adjust=", signif(adjust,2), sep="")
    }
    res <- paste0(res, " + " , gggeoms[s$plotType], "(", params, ")")
    
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
    res <- paste(res, ' + labs(title="', s$title, '")', sep="")
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
    res <- paste(res, ', main="', s$title, '"', sep="")
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
