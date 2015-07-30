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
# 
# https://github.com/rpruim/mosaic/issues/40
# ------------------------------------------------------------------------------
# TODO
#
# work with interaction terms
# error accounting (variables don't exis etc.)
# draw optional legend for line colors
# generate enough colors for all regression lines
# test with glm
#
# error accounting
#
# variables in model don't exist
# model doesn't exist
# not enough colors for all regression lines
# categorical variables aren't factors
# warning for too many categories, graph will be crowded with lines
# ------------------------------------------------------------------------------

plotModel <- function(mod, data=parent.frame(), ...) { UseMethod("plotModel") }

# plotModel.default <- function(mod, data=parent.frame(), ...) {
#   # make sure model is class lm or glm
#   if (!(class(mod) %in% c("lm", "glm"))) {
#     stop(paste("plotModel takes object of class lm or glm, not", class(mod)))
#   }
#   
#   # determine number of each variable type (categorical or quantitative)
#   var.classes = attr(terms(mod), "dataClasses")
#   response.name = as.character(lhs(terms(mod)))
#   numeric.names = setdiff(names(which(var.classes == "numeric")), response.name)
#   
#   # get palette to color each regression line
#   palette = trellis.par.get("superpose.symbol")$col
#   palette = rep(palette, 5) # HACK - need way to generate new colors
#   
#   # if one numeric explanatory variable, draw 2D plot
#   if (length(numeric.names) == 1) {
#     numeric.name = numeric.names[1]
#     modelFunc = makeFun(mod)
#     myplot = xyplot(as.formula(paste(response.name, "~", numeric.name)), data=mod$model, ...) # plot data
#     categories = expand.grid(mod$xlevels) # all category combinations
#     
#     if (nrow(categories) == 0) {
#       return(myplot)
#     }
#     
#     template = getFormulaTemplate(attr(terms(mod), "dataClasses"))
#     
#     # plot parallel regression lines for each category combination
#     for (row in 1:nrow(categories)) {
#       formStr = template
#       for (col in 1:ncol(categories)) {
#         category = as.character(categories[row, col])
#         formStr = sub(CATEGORICAL_REPLACE_STR, category, formStr)
#       }
#       form = as.formula(formStr)
#       
#       # plot regression line of formula
#       myplot = plotFun(form, plot=myplot, col=palette[row], add=TRUE)
#     }
#     
#   } else if (length(numeric.names) == 2) {
#     # if 2 numeric explanatory variables, draw 3D plot
#     stop("Currently no support for 3D plots with 2 numeric variables.")
#   } else if (length(numeric.names) > 2) {
#     stop("Cannot visualize model in 3D space if more than 2 numeric variables")
#   }
#   
#   # display plot
#   return(myplot)
# }
# 
# 
# plotModel.lm <- function(mod, data=parent.frame(), ...) {
#   # determine number of each variable type (categorical or quantitative)
#   var.classes = attr(terms(mod), "dataClasses")
#   response.name = as.character(lhs(terms(mod)))
#   numeric.names = setdiff(names(which(var.classes == "numeric")), response.name)
#   
#   # get palette to color each regression line
#   palette = trellis.par.get("superpose.symbol")$col
#   palette = rep(palette, 5) # HACK - need way to generate new colors
#   
#   # if one numeric explanatory variable, draw 2D plot
#   if (length(numeric.names) == 1) {
#     numeric.name = numeric.names[1]
#     modelFunc = makeFun(mod)
#     myplot = xyplot(as.formula(paste(response.name, "~", numeric.name)), data=mod$model, ...) # plot data
#     categories = expand.grid(mod$xlevels) # all category combinations
#     
#     if (nrow(categories) == 0) {
#       return(myplot)
#     }
#     
#     template = getFormulaTemplate(attr(terms(mod), "dataClasses"))
#     
#     # plot parallel regression lines for each category combination
#     for (row in 1:nrow(categories)) {
#       formStr = template
#       for (col in 1:ncol(categories)) {
#         category = as.character(categories[row, col])
#         formStr = sub(CATEGORICAL_REPLACE_STR, category, formStr)
#       }
#       form = as.formula(formStr)
#       
#       # plot regression line of formula
#       myplot = plotFun(form, plot=myplot, col=palette[row], add=TRUE)
#     }
#     
#   } else if (length(numeric.names) == 2) {
#     # if 2 numeric explanatory variables, draw 3D plot
#     stop("Currently no support for 3D plots with 2 numeric variables.")
#   } else if (length(numeric.names) > 2) {
#     stop("Cannot visualize model in 3D space if more than 2 numeric variables")
#   }
#   
#   # display plot
#   return(myplot)
# }
# 
# CATEGORICAL_REPLACE_STR = "#%#" # chosen b/c no regex chars
# 
# getFormulaTemplate = function(vars) {
#   # takes all numeric and categorical explanatory variables
#   # returns template to fill in categorical variables for formula, e.g.
#   # "numeric", "factor", "factor" -> "modelFunc(mod, '%#%', '%#%') ~ mod"
#   vars = vars[2:length(vars)] # ignore response variable
#   vars[vars=="numeric"] = "mod"
#   vars[vars=="factor"] = paste("'", CATEGORICAL_REPLACE_STR, "'", sep="")
#   template = paste(vars,  collapse=", ")
#   template = paste("modelFunc(", template, ") ~ mod", sep="")
#   return(template)
# }
# 
