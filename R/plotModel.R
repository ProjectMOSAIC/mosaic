utils::globalVariables(c(".group", ".color"))

#' Plot a regression model
#' 
#' @description Visualize a regression model amid the data that generated it. 
#' 
#' @param mod A model of type \code{\link{lm}} or \code{\link{glm}}
#' @param \dots arguments passed to \code{\link{xyplot}} or \code{rgl::plot3d}.
#'
#' @section Caution:
#' This is still underdevelopment.  The API is subject to change, and some
#' use cases may not work yet.  Watch for improvments in subsequent versions of the package.
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
#' predictors using the \code{rgl} package.
#' 
# If \code{key} indicates two quantitative variables, then
# if the \code{rgl} package is available, the data points are drawn in 3-space,
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
#' @seealso \code{\link{plotPoints}}, \code{\link{plotFun}}
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
# key = ~ x | z for faceting one covariate?
# 
# ------------------------------------------------------------------------------

plotModel <- function(mod, ...) { UseMethod("plotModel") }

#' @export
plotModel.default <- function(mod, ...) {
  plotModel(parseModel(mod), ...)
}

plotModel.parsedModel <- 
  function(x, key=1, formula = NULL, ..., auto.key = NULL, drop = TRUE, max.levels = 9L, system=c("lattice", "ggplot2")) {
    
    system <- match.arg(system)
    
    if (inherits(key, "formula")) {
      key <- all.vars(rhs(key))
    }
    
    if (length(key) > 1L) 
      stop("Only one key variable allowed (so far).")
    
    if (length(x$varTypes) < 2L) 
      stop("Only models with explanatory variables can be plotted (so far).")
    
    key <- x$varTypes[-1][key]
    if (is.null(formula)) {
      formula <- y ~ x
      formula[[2]] <- as.name(x$responseName)
      formula[[3]] <- as.name(names(key))
    }
    other_data <- x$data[, -1, drop=FALSE]
    other_data[[names(key)]] <- NULL
    
    discretePart <- function(data, varTypes = x$varTypes) {
      res <- data[ , intersect(names(data), names(varTypes[varTypes == "discrete"])), drop=FALSE]
      if (ncol(res) == 0L) res$blank <- ""
      res
    }
    
    nonEmpty <- function(data, varTypes = x$varTypes) {
        if (ncol(data) == 0L) data$blank <- ""
        data
      }
    
    
    if (prod(dim(discretePart(other_data))) > 0) {
    x$data <-
      x$data %>%
      mutate(
        .color = interaction(discretePart(other_data)),
        .group = interaction(nonEmpty(other_data))
      )
    } else {
      x$data <- 
        x$data %>% 
        mutate(.color = " ", .group = " ")
    }
    
    some_levels <- function(x) {
      if (is.numeric(x)) 
        return( sort(unique(ntiles(x, 3, format = "median"))) )
      return(sort(unique(x)))
    }
    
    levels_all <- expand.grid(lapply(other_data, some_levels))
    levels_shown <- levels_all
    if (nrow(levels_all) > max.levels) { 
      warning("Randomly sampling some of the ", 
              nrow(levels_all), 
              " levels of the fit function for you.",
              call. = FALSE) 
      levels_shown <- levels_all %>% sample_n(max.levels)
    }
    
    # convert the model into a function
    modelFunc <- makeFun(x$model)
    
    if (key == "continuous") {
      p <- xyplot( formula,  data = x$data )
      x_points <- seq(p$x.limits[1], p$x.limits[2], length.out = 40)
    } else {
      x_points <- unique( x$data[[names(key)]] )
    }
    
    if( prod(dim(levels_shown)) > 0L) {
      if (is.null(auto.key)) 
        auto.key <- list(points = TRUE, lines=TRUE, 
                         columns = min(length(unique(x$data$.color)), 3))
      # categories[["id"]] <- factor(1:nrow(categories))
      if (prod(dim(levels_all)) > 0) {
      levels_all <- 
        levels_all %>% 
        mutate(
          .color = interaction(discretePart(levels_all), drop = drop),
          .group = interaction(nonEmpty(levels_all), drop = drop)
          )
      levels_shown <- 
        levels_shown %>% 
        mutate( 
          .color = interaction(discretePart(levels_shown), drop = drop),
          .group = interaction(nonEmpty(levels_shown), drop = drop)
          )
      } else {
        levels_all <- 
          levels_all %>% 
          mutate(.color = " ", .group = " ") 
        levels_shown <- 
          levels_shown %>% 
          mutate(.color = " ", .group = " ") 
      }
        
      other_data <- suppressMessages(other_data %>% left_join(levels_all))
      point_data <- x$data # suppressMessages(x$data %>% left_join(levels_all))
      line_data <- bind_rows(lapply(1:length(x_points), function(x) levels_shown)) 
      line_data[["x"]] <-
        line_data[[names(key)]] <- 
          rep(x_points, times = nrow(levels_shown))
    } else {
      if (is.null(auto.key)) auto.key <- FALSE
      point_data <- x$data 
      point_data[[".color"]] <- factor(" ")
      point_data[[".group"]] <- factor(" ")
      line_data <- data.frame(x = x_points, .color=factor(" "), .group=factor(" ")) 
      line_data[[names(key)]] <- x_points
    }
    
    line_data[["y"]] <- 
      line_data[[x$responseName]] <- 
      predict(x$model, newdata=line_data, type="response")
    
    mypanel <- 
      function(x, y, line_data, ...) {
        panel.xyplot(x, y, type = "p", ...)
        line_data <- line_data %>% arrange(.color, .group, x)
        ncolors <- length(unique(line_data$.color))
        ngroups <- length(unique(line_data$.group))
        grid::grid.polyline(
          x = line_data$x, 
          y = line_data$y, 
          default.units = "native",
          id = as.numeric(line_data$.group),
          gp = gpar(
            col = trellis.par.get("superpose.line")$col[rep(1:ncolors, each=ngroups/ncolors)]
          )
        )
      }
    
    if (system == "ggplot2") {
      ggplot() +
        geom_point(aes_string(y = x$responseName, x = names(key), colour=".color", group=".group"), size=1.2,
                   data = point_data %>% droplevels()) +
        geom_line (aes_string(y = x$responseName, x = names(key), colour=".color", group = ".group"), size=0.5,
                   data = line_data %>% droplevels())
    } else {
      xyplot(formula, 
             data = point_data %>% droplevels(),
             line_data = line_data %>% droplevels(),
             groups = .color,
             auto.key = auto.key,
             ...,
             panel = mypanel) 
    } 
  }

# revise this later.
plotModel_planes <- function(pmod, key, ...) {
  # do something with rgl
  if (!requireNamespace("rgl")) {
    stop("Please install rgl for 3D support")
  }
  xName = names(key)[1] # pmod$numericNames[1] 
  yName = names(key)[2] # pmod$numericNames[2] 
  zName = pmod$responseName 
  # get better default axis labels
  # if (is.null(match.arg("xlab"))) { xlab = "mod$numericNames[1]"; }
  rgl::plot3d(x = pmod$data[, xName], 
              y = pmod$data[, yName], 
              z = pmod$data[, zName], 
              ...)
  
  # Now need to add the planes...

}

# original version of above
plotModel0.mplanes <- function(mod, ...) {
  # do something with rgl
  if (!requireNamespace("rgl")) {
    stop("Please install rgl for 3D support")
  }
  xName = mod$numericNames[1] 
  yName = mod$numericNames[2] 
  zName = mod$responseName 
  # get better default axis labels
  # if (is.null(match.arg("xlab"))) { xlab = "mod$numericNames[1]"; }
  rgl::plot3d(x = mod$model[, xName], 
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
    rgl::planes3d(coefs[xName], coefs[yName], -1, coefs["(Intercept)"], alpha = 0.5, col = "lightgray")
  } else {
    for (intercept in intercepts) {
      rgl::planes3d(coefs[xName], coefs[yName], -1, intercept, alpha = 0.5, col = "lightgray")
    }
  }
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
  
  class(res) <- c("parsedModel")
  return(res)
}
