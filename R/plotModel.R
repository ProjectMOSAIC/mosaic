#' Plot a regression model
#' 
#' @description Visualize a regression model amid the data that generated it. 
#' 
#' @param mod A model of type \code{\link{lm}} or \code{\link{glm}}
#' @param \dots arguments passed to \code{\link{xyplot}} or \code{\link{plot3d}}
#'
#' @details The goal of this function is to assist with visualization
#' of statistical models. Namely, to plot the model on top of the data
#' from which the model was fit. 
#' 
#' To do this we need to understand the geometry upon which the model 
#' was fit. \code{\link{plotModel}} will attempt to determine from a model object
#' how many quantitative and categorical explanatory variables the model 
#' contains. 
#' 
#' If the model was fit on zero quantiative variables (e.g. ANOVA), then
#' the repsonse variable is plotted against a constant, and horizontal 
#' lines represents the various levels of the model. 
#' 
#' If the model was fit on one quantitative variable (e.g. SLR), then
#' a scatter plot is drawn, and the model is realized as parallel or
#' non-parallel lines, depending on whether interaction terms are present.
#' 
#' If the model was fit on two quantitative variables (e.g. MLR), then
#' if \code{\link{rgl}} is present, the data points are drawn in 3-space,
#' and the model is realized as a series of planes. 
#' 
#' Currently, only linear regression models and 
#' generalized linear regression models are supported. 
#' 
#' Interaction terms are supported for lines, but not planes. 
#' 
#' Polynomial terms are currently unsupported. 
#'
#' @return A lattice graphics object, or in the case of a 3D model, nothing
#' but an RGL window will pop up.
#' 
#' @seealso \code{\link{plotPloints}, \link{plotFun}}
#' @author Ben Baumer, Galen Long
#' @export
#' @examples
#' 
#' require(mosaic)
#' mod = lm( mpg ~ 1, data=mtcars)
#' plotModel(mod)
#' 
#' # not work -- not sure why -- should work...
#' mod = lm( mpg ~ factor(cyl), data=mtcars)
#' plotModel(mod, auto.key=TRUE)
#' 
#' # SLR
#' mod = lm( mpg ~ wt, data=mtcars)
#' plotModel(mod, pch = 19)
#' 
#' # parallel slopes
#' mod = lm( mpg ~ wt + factor(cyl), data=mtcars)
#' plotModel(mod, auto.key=TRUE)
#' 
#' # multiple categorical vars
#' mod = lm( mpg ~ wt + factor(cyl) + factor(vs) + factor(am), data=mtcars)
#' plotModel(mod, auto.key=TRUE)
#' 
#' # interaction
#' mod = lm( mpg ~ wt + factor(cyl) + wt*factor(cyl), data=mtcars)
#' plotModel(mod)
#' 
#' # polynomial terms
#' # not work -- doesn't understand that wt^2 is a squared term
#' mod = lm( mpg ~ wt + I(wt^2), data=mtcars)
#' plotModel(mod)
#' 
#' # 3D model
#' mod <- lm( mpg ~ wt + hp, data = mtcars)
#' plotModel(mod)
#' 
#' # parallel planes
#' mod <- lm( mpg ~ wt + hp + factor(cyl) + factor(vs), data = mtcars)
#' plotModel(mod)
#' 
#' # interaction planes
#' # not work -- need better logic in function
#' mod <- lm( mpg ~ wt + hp + wt * factor(cyl), data = mtcars)
#' plotModel(mod)
#' 
#' # GLM
#' mod <- glm(vs ~ wt, data=mtcars, family = 'binomial')
#' plotModel(mod)
#' 
#' # GLM with interaction
#' mod <- glm(vs ~ wt + factor(cyl), data=mtcars, family = 'binomial')
#' plotModel(mod)


# ------------------------------------------------------------------------------
# TODO
#
# support for polynomial terms
# support for interaction in planes
# ... pass-through to xyplot()
# improved efficiency by avoiding ladd() / add = TRUE
# 
# ------------------------------------------------------------------------------

plotModel <- function(mod, ...) { UseMethod("plotModel") }

#' @export
plotModel.default <- function(mod, ...) {
  plotModel(parseModel(mod))
}

#' @export
plotModel.mhyperplanes <- function(mod, ...) {
  stop("Sorry, but you can't plot in higher-dimensional space.")
}

#' @export
plotModel.mplanes <- function(mod, ...) {
  # do something with rgl
  if (!require(rgl)) {
    stop("Please install rgl for 3D support")
  }
  xName = mod$numericNames[1] 
  yName = mod$numericNames[2] 
  zName = mod$responseName 
  # get better default axis labels
  # if (is.null(match.arg("xlab"))) { xlab = "mod$numericNames[1]"; }
  plot3d(x = mod$model[, xName], 
         y = mod$model[, yName], 
         z = mod$model[, zName], 
         ...)
  
  # all combinations of categorical levels
  categories = expand.grid(mod$xlevels) 
  
  coefs <- coef(mod)
  levels <- coefs[!names(coefs) %in% c(xName, yName)]
  
  # sort of a hack -- ideally there is a better solution
  varNames <- gsub("factor|\\(|\\)", "", attr(terms(mod), "term.labels"))
  # drop all interactions, since the linear terms should be there
  varNames <- varNames[!grepl(":", varNames)]
  
  categories <- cbind(x = rep(0, nrow(categories)), y = rep(0, nrow(categories)), categories)
  names(categories) <- varNames
  intercepts <- predict(mod, newdata = categories)
  
  if (nrow(categories) == 0) {
    planes3d(coefs[xName], coefs[yName], -1, coefs["(Intercept)"], alpha = 0.5, col = "lightgray")
  } else {
    for (intercept in intercepts) {
      planes3d(coefs[xName], coefs[yName], -1, intercept, alpha = 0.5, col = "lightgray")
    }
  }
}

#' @export
plotModel_old.mlines <- function(mod, ...) {
  # plot data
  myplot <- xyplot(as.formula(paste(mod$responseName, "~", mod$numericNames[1])), data = mod$model, ...) 
  
  # convert the model into a function
  modelFunc <- makeFun(mod)
  
  # all combinations of categorical levels
  categories <- expand.grid(mod$xlevels) 
  
  levels <- apply(categories, MARGIN = 1, paste, collapse = ",")
  levelForms <- paste0("modelFunc(x, ", levels, ") ~ x")
  
  if (nrow(categories) == 0) {
    myplot <- plotFun(as.formula(modelFunc(x) ~ x), plot = myplot, add = TRUE)
  } else {
    for (i in 1:length(levelForms)) {
      myplot <- plotFun(as.formula(levelForms[i]), plot = myplot, add = TRUE, col = i)
    }
  }
  
  # display plot
  return(myplot)
}

#' @export
plotModel2 <- function(mod, ...) { UseMethod("plotModel2") }

#' @export
plotModel2.default <- function(mod, ...) {
  plotModel2(parseModel(mod))
}

#' @export
plotModel.mlines <- function(mod, ...) {
  
  # convert the model into a function
  modelFunc <- makeFun(mod)
  
  # all combinations of categorical levels
  categories <- expand.grid(mod$xlevels) 
 
  if (nrow(categories) < 1L) {
    levelFuns <- list(modelFunc)
  } else {
    levels <- apply(categories, MARGIN = 1, paste, collapse = ",")
    levelForms <- 
      sapply(paste0('modelFunc(x, "', paste(levels, sep='", "'), '") ~ x'), as.formula)
    levelFuns <- lapply(levelForms, 
                        function(form) { 
                          f <- makeFun(form)
                          environment(f)$modelFunc <- modelFunc
                          environment(f)$mod <- mod
                          f
                        })
  }
  
  # create panel function
  mypanel <- 
    function(x, y, ...) {
      panel.xyplot(x, y, ...)
      panel.plotFun1(levelFuns)
    }
  
  xyplot(as.formula(paste(mod$responseName, "~", mod$numericNames[1])), 
           data = mod$model, ...,
           panel = mypanel) 
}


#' @export
plotModel.mpoints <- function(mod, ...) {
  # plot data
  myplot <- xyplot(as.formula(paste(mod$responseName, "~ 1")), data = mod$model, ...) 
  
  # remove the extra class
  class(mod) <- setdiff(class(mod), "mpoints")
  
  # conver the model into a function
  modelFunc = makeFun(mod)
  
  # all combinations of categorical levels
  categories = expand.grid(mod$xlevels) 
  
  levels <- apply(categories, MARGIN = 1, paste, collapse = ",")
  levelForms <- paste0("modelFunc(", levels, ") ~ x")
  
  if (nrow(categories) == 0) {
    myplot <- plotFun(as.formula(modelFunc(x) ~ x), plot = myplot, add = TRUE)
  } else {
    for (i in 1:length(levelForms)) {
      # not sure why this doesn't work...
      myplot <- plotFun(as.formula(levelForms[i]), plot = myplot, add = TRUE, col = i)
    }
  }
  
  myplot
}

parseModel <- function(x) {
  # determine number of each variable type (categorical or quantitative)
  varClasses <- attr(terms(x), "dataClasses")
  x$responseName <- as.character(lhs(terms(x)))
  # need to do something more clever here to recognize polynomial terms
  x$numericNames <- setdiff(names(which(varClasses == "numeric")), x$responseName)
  x$catNames <- setdiff(names(which(varClasses != "numeric")), x$responseName)
  if (length(x$numericNames) == 0) {
    modClass <- "mpoints"
  } else if (length(x$numericNames) == 1) {
    modClass <- "mlines"
  } else if (length(x$numericNames) == 2) {
    modClass <- "mplanes"
  } else if (length(x$numericNames) > 2) {
    modClass <- "mhyperplanes"
  }
  class(x) <- c(modClass, class(x))
  return(x)
}


