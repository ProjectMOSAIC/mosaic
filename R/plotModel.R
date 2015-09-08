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
#' The primary plot type is a scatter plot.  The x-axis can be assigned
#' to one of the predictors in the model.  Additional predictors are thought
#' of as co-variates.  The data and fitted curves are partitioned by
#' these covariates.  When the number of components to this partition is large,
#' a random subset of the fitted curves is displayed to avoid visual clutter.
#' 
#' If the model was fit on one quantitative variable (e.g. SLR), then
#' a scatter plot is drawn, and the model is realized as parallel or
#' non-parallel lines, depending on whether interaction terms are present.
#' 
#' Eventually we hope to support 3-d visualizations of models with 2 quantitative
#' predictors using \code{\link{rgl}}.
#' 
# If \code{key} indicates two quantitative variables, then
# if \code{\link{rgl}} is present, the data points are drawn in 3-space,
# and the model is realized as a series of planes.
#' 
#' Currently, only linear regression models and 
#' generalized linear regression models are supported. 
#' 
#' @return A lattice or ggplot2 graphics object.
#' 
# or in the case of a 3D model, nothing
# but an RGL window will pop up.
#' 
#' @seealso \code{\link{plotPloints}, \link{plotFun}}
#' @author Ben Baumer, Galen Long, Randall Pruim
#' @export
#' @examples
#' 
#' require(mosaic)
#' 
#' mod <- lm( mpg ~ factor(cyl), data = mtcars)
#' plotModel(mod)
#' 
#' # SLR
#' mod <- lm( mpg ~ wt, data = mtcars)
#' plotModel(mod, pch = 19)
#' 
#' # parallel slopes
#' mod <- lm( mpg ~ wt + factor(cyl), data=mtcars)
#' plotModel(mod)
#' 
#' # multiple categorical vars
#' mod <- lm( mpg ~ wt + factor(cyl) + factor(vs) + factor(am), data = mtcars)
#' plotModel(mod)
#' plotModel(mod, key = ~am)
#' 
#' # interaction
#' mod <- lm( mpg ~ wt + factor(cyl) + wt:factor(cyl), data = mtcars)
#' plotModel(mod)
#' 
#' # polynomial terms
#' mod <- lm( mpg ~ wt + I(wt^2), data = mtcars)
#' plotModel(mod)
#' 
#' # GLM
#' mod <- glm(vs ~ wt, data=mtcars, family = 'binomial')
#' plotModel(mod)
#' 
#' # GLM with interaction
#' mod <- glm(vs ~ wt + factor(cyl), data=mtcars, family = 'binomial')
#' plotModel(mod)

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

# ------------------------------------------------------------------------------
# TODO
#
# detect and pass through 3-d models
# support for interaction in planes
# 
# ------------------------------------------------------------------------------

plotModel <- function(mod, ...) { UseMethod("plotModel") }

#' @export
plotModel.default <- function(mod, ...) {
  plotModel(parseModel(mod), ...)
}

plotModel.parsedModel <- 
  function(x, key=1, ..., max.levels = 9L, system=c("lattice", "ggplot2")) {
    
    system <- match.arg(system)
    
    if (inherits(key, "formula")) {
      key <- all.vars(rhs(key))
    }
    
    if (length(key) > 1L) 
      stop("Only one key variable allowed (so far).")
    
    if (length(x$varTypes) < 2L) 
      stop("Only models with explanatory variables can be plotted (so far).")
    
    key <- x$varTypes[-1][key]
    formula <- y ~ x
    formula[[2]] <- as.name(x$responseName)
    formula[[3]] <- as.name(names(key))
    other_data <- x$data[, -1, drop=FALSE]
    other_data[[names(key)]] <- NULL
    categories <- expand.grid(lapply(other_data, unique))
    if (nrow(categories) > max.levels) { 
      warning("Randomly sampling some of the ", 
              nrow(categories), 
              " levels of the fit function for you.",
              call. = FALSE) 
      categories <- categories %>% sample_n(max.levels)
    }
    
    # convert the model into a function
    modelFunc <- makeFun(mod)
    
    if (key == "continuous") {
      p <- xyplot(
        formula, 
        data = x$data
      )
      x_points <- seq(p$x.limits[1], p$x.limits[2], length.out = 40)
    } else {
      x_points <- unique( x$data[[names(key)]] )
    }
    if( nrow(categories) > 0L) {
      # categories[["id"]] <- factor(1:nrow(categories))
      categories <- categories %>% mutate(id = interaction(categories))
      other_data <- other_data %>% inner_join(categories)
      point_data <- x$data %>% inner_join(categories)
      line_data <- bind_rows(lapply(1:length(x_points), function(x) categories))
    } else {
      point_data <- x$data 
      point_data[["id"]] <- factor("1")
      line_data <- point_data
    }
    line_data[["x"]] <-
      line_data[[names(key)]] <- 
      rep(x_points, times = nrow(categories))
    
    line_data[["y"]] <- 
      line_data[[x$responseName]] <- 
      predict(x$model, newdata=line_data, type="response")
    
    mypanel <- 
      function(x, y, line_data, ...) {
        panel.xyplot(x, y, type = "p", ...)
        line_data <- line_data %>% arrange(id, x)
        grid::grid.polyline(
          x = line_data$x, 
          y = line_data$y, 
          default.units = "native",
          id = as.numeric(line_data$id),
          gp = gpar(col = trellis.par.get("superpose.line")$col)
        )
      }
    
    if (system == "ggplot2") {
      ggplot() +
        geom_point(aes_string(y = x$responseName, x = names(key), colour="id"), size=1.2,
                   data = point_data) +
        geom_line (aes_string(y = x$responseName, x = names(key), colour="id"), size=0.5,
                   data = line_data)
    } else {
      xyplot(formula, 
             data = point_data,
             line_data = line_data,
             groups = id,
             ...,
             panel = mypanel) 
    } 
    
    
  }

#' @export
plotModel0.mhyperplanes <- function(mod, ...) {
  stop("Sorry, but you can't plot in higher-dimensional space.")
}

#' @export
plotModel0.mplanes <- function(mod, ...) {
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
plotModel0_old.mlines <- function(mod, ...) {
  # plot data
  myplot <- xyplot(as.formula(paste(mod$responseName, "~", mod$numericNames[1])), 
                   data = mod$call$data, ...) 
  
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
plotModel0 <- function(mod, ...) { UseMethod("plotModel0") }

#' @export
plotModel0.default <- function(mod, ...) {
  plotModel0(parseModel0(mod))
}

#' @export
plotModel0.mlines <- function(mod, ...) {
  
  # convert the model into a function
  modelFunc <- makeFun(mod)
  
  # all combinations of categorical levels
  categories <- expand.grid(mod$xlevels) 
  
  if (nrow(categories) < 1L) {
    levelFuns <- list(modelFunc)
  } else {
    levels <- apply(categories, MARGIN = 1, paste, collapse = '", "')
    levelForms <- 
      sapply(paste0('modelFunc(x, "', levels,  '") ~ x'), as.formula)
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
      panel.plotFun1a(levelFuns)
    }
  
  xyplot(as.formula(paste(mod$responseName, "~", mod$numericNames[1]), env=parent.frame()), 
         data = eval(mod$call$data, parent.frame()), ...,
         panel = mypanel) 
}


#' @export
plotModel0.mpoints <- function(mod, ...) {
  # plot data
  myplot <- xyplot(as.formula(paste(mod$responseName, "~ 1")), data = mod$model, ...) 
  
  # remove the extra class
  # class(mod) <- setdiff(class(mod), "mpoints")
  
  # convert the model into a function
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

parseModel0 <- function(x) {
  data <- eval(x$call$data, environment(x$call$formula))[modelVars(x)]
  fit <- makeFun(x)
  inputs <- head(formals(fit), -2)  # remove ... and transform
  
  discreteOrContinuous <- function(x) {
    if (is.factor(x) || length(unique(x)) < 4L) "discrete" else "continuous"
  }
  
  # determine number of each variable type (categorical or quantitative)
  x$varTypes <- lapply(data, discreteOrContinuous) 
  # varTypes <- attr(terms(x), "dataClasses")[modelVars(x)]
  x$responseName <- as.character(lhs(terms(x)))
  x$numericNames <- names(which(x$varTypes == "continuous")) # , x$responseName)
  x$catNames <- names(which(x$varTypes == "discrete")) # , x$responseName)
  
  # cases (w, x are cont; a, b are disc):
  #   y ~ x + other
  #     xyplot (with groups for other)
  #   y ~ x + y + other
  #     3d plot (groups?)
  #   y ~ a + other
  #     xyplot with jitter (with groups for other)
  #   y ~ a + b + other
  #     xyplot with jitter and facets? (and groups for other)
  
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

parseModel <- function(x) {
  res <- list(model = x)
  res$data <- eval(x$call$data, environment(x$call$formula))[all.vars(x$call$formula)]
  res$fit <- makeFun(x)
  inputs <- head(formals(res$fit), -2)  # remove ... and transform
  
  discreteOrContinuous <- function(n, data, model) {
    if (is.factor(data[[n]])    || 
        is.character(data[[n]]) ||
        is.logical(data[[n]])   ||
        paste0("factor(", n, ")" ) %in% names(attr(terms(model), "dataClasses"))
    )
      "discrete" else "continuous"
  }
  
  # determine number of each variable type (categorical or quantitative)
  res$varTypes <- sapply(names(res$data), function(n) discreteOrContinuous(n, res$data, x)) 
  names(res$varTypes) <- names(res$data)
  
  # varTypes <- attr(terms(x), "dataClasses")[modelVars(x)]
  res$responseName <- as.character(lhs(terms(x)))
  res$numericNames <- names(which(x$varTypes == "continuous")) # , x$responseName)
  res$catNames <- names(which(x$varTypes == "discrete")) # , x$responseName)
  
  # cases (w, x are cont; a, b are disc):
  #   y ~ x + other
  #     xyplot (with groups for other)
  #   y ~ x + y + other
  #     3d plot (groups?)
  #   y ~ a + other
  #     xyplot with jitter (with groups for other)
  #   y ~ a + b + other
  #     xyplot with jitter and facets? (and groups for other)
  
  class(res) <- c("parsedModel")
  return(res)
}
