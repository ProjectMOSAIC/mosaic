#' Plot a regression model
#' 
#' Visualize a regression model amid the data that generated it. 
#' 
#' @param x A model of type \code{lm} or \code{glm}
#'
#' @param \dots additional arguments
#' 
#' @return A lattice graphics object
#' 
#' @seealso \code{\link{plotPloints}, \link{plotFun}}
#' @export
#' @examples
#' mod = lm( width ~ length + sex + length*sex, data=KidsFeet)
#' plotModel(mod)
#' 
#' 
#' 
# 

# https://github.com/rpruim/mosaic/issues/40

# ------------------------------------------------------------------------------
# TODO
# plotting correct regression lines? (how to check?) (test on other data sets)
# work with no categories
# doesn't recognize interaction terms

# account if variables/model doesn't exist
# other error accounting
# does the ... for xyplot work?

# if categorical variable isn't a factor, thinks it's numeric

# should plot legend for line colors
# doesn't plot other lines if we run out of colors - maybe do automatic color generation in intervals?
# change to prettier color set?
# add warning if more than x # of categories? (too many lines/legend too big)?
# add ... to xyplot
# works with glm?
# ------------------------------------------------------------------------------
# QUESTIONS
# how does modelFunc know to replace mod in formula but not when calling it?
# how can we "fill in" interaction terms for modelFunc ?
#-------------------------------------------------------------------------------

require(mosaic)
data(KidsFeet)
mod = lm(width ~ length + birthyear, data=KidsFeet)

require(mosaic)
weights = c(83,  82,  97,  92,  91,  94,  75,  97,  202, 150, 110, 102, 143, 155, 121, 109, 115, 117, 123, 130, 142, 155, 167, 189, 210, 240, 270, 350)
heights = c(4.1, 4.1, 4.2, 4.5, 4.3, 4.4, 4.4, 4.3, 4.6, 4.7, 4.9, 4.9, 4.8, 5.0, 5.2, 5.1, 5.4, 5.3, 5.5, 5.5, 5.6, 5.7, 5.9, 6.1, 6.4, 6.2, 6.0, 6.3)
genders = c("F", "F", "F", "M", "F", "F", "F", "F", "M", "F", "F", "F", "F", "F", "F", "O", "F", "F", "O", "O", "O", "F", "M", "M", "O", "M", "M", "M")
happiness = c("H", "H", "H", "H", "S", "S", "H", "S", "S", "H", "S", "H", "S", "H", "H", "S", "S", "S", "S", "S", "H", "S", "S", "S", "S", "S", "S", "H")
humans = data.frame("weights"=weights, "heights"=heights, "genders"=genders, "happiness"=happiness)
mod = lm(weights ~ heights + genders + happiness, data=humans)

require(mosaic)
require(Stat2Data)
data(Day1Survey)
head(Day1Survey)
mod = lm(Reading ~ Sex + Texting + Class, data=Day1Survey)

# -------------------------------------------------------------------
# CODE
# -------------------------------------------------------------------

CATEGORICAL_REPLACE_STR = "#%#" # chosen b/c no regex chars

plotModel <- function(mod, data=parent.frame(), ...) {
  # make sure model is class lm or glm
  if (!(class(mod) %in% c("lm", "glm"))) {
    stop(paste("plotModel takes object of class lm or glm, not", class(mod)))
  }
  
  # determine number of each variable type (categorical or quantitative)
  var.classes = attr(terms(mod), "dataClasses")
  response.name = as.character(lhs(terms(mod)))
  numeric.names = setdiff(names(which(var.classes == "numeric")), response.name)
  
  # get palette to color each regression line
  palette = trellis.par.get("superpose.symbol")$col
  palette = rep(palette, 5) # duplicate for now
  
  # if one numeric explanatory variable, draw 2D plot
  if (length(numeric.names) == 1) {
    numeric.name = numeric.names[1]
    modelFunc = makeFun(mod)
    myplot = xyplot(as.formula(paste(response.name, "~", numeric.name)), data=mod$model) # plot data
    categories = expand.grid(mod$xlevels) # all category combinations
    
    if (nrow(categories) == 0) {
      return(myplot)
    }
    
    template = getFormulaTemplate(attr(terms(mod), "dataClasses"))
    
    # plot parallel regression lines for each category combination
    for (row in 1:nrow(categories)) {
      formStr = template
      for (col in 1:ncol(categories)) {
        category = as.character(categories[row, col])
        formStr = sub(CATEGORICAL_REPLACE_STR, category, formStr)
      }
      form = as.formula(formStr)
      
      # plot regression line of formula
      myplot = plotFun(form, plot=myplot, col=palette[row], add=TRUE)
    }
    
  } else if (length(numeric.names) == 2) {
    # if 2 numeric explanatory variables, draw 3D plot
    stop("Currently no support for 3D plots with 2 numeric variables.")
  } else if (length(numeric.names) > 2) {
    # too many numeric variables - plot is not visualizable in 3D
    stop("Cannot visualize model in 3D space if more than 2 numeric variables")
  }
  
  # display plot
  return(myplot)
}

getFormulaTemplate = function(vars) {
  # takes all numeric and categorical explanatory variables
  # returns template to fill in categorical variables for formula, e.g.
  # "numeric", "factor", "factor" -> "modelFunc(mod, '%#%', '%#%') ~ mod"
  vars = vars[2:length(vars)] # ignore response variable
  vars[vars=="numeric"] = "mod"
  vars[vars=="factor"] = paste("'", CATEGORICAL_REPLACE_STR, "'", sep="")
  template = paste(vars,  collapse=", ")
  template = paste("modelFunc(", template, ") ~ mod", sep="")
  return(template)
}

