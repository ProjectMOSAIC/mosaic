utils::globalVariables(c(".group", ".color", ".cond", "mypanel"))

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
#' plotModel(mod, mpg ~ am)
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
#' mod <- lm( mpg ~ wt + hp + wt * factor(cyl), data = mtcars)
#' plotModel(mod)
#' plotModel(mod, system="g") + facet_wrap( ~ cyl )
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
  function(x, formula = NULL, ..., auto.key = NULL, drop = TRUE, 
           max.levels = 9L, system=c("lattice", "ggplot2")) {
    
    system <- match.arg(system)
  
    if (length(x$varTypes) < 2L) 
      stop("Only models with explanatory variables can be plotted.")
    
    if (! inherits(formula, "formula")) { 
      key <- x$varTypes[-1][1]
      formula <- y ~ x
      formula[[2]] <- as.name(x$responseName)
      formula[[3]] <- as.name(names(key))
    }
   
    # variable groups
    #  * keyVar = x-axis variable
    #  * coVars = other rhs vars
    #  * condVars = variables in condition
    #  * restVars: other variables in the model
    
    key <- all.vars(rhs(formula))[1]
    coVars <- intersect( setdiff(all.vars(rhs(formula)), key), names(x$varTypes) )
    condVars <- intersect( all.vars(condition(formula)), names(x$varTypes) )
    other_data <- x$data[, -1, drop=FALSE]
    other_data[[key]] <- NULL
    restVars <- setdiff( names(other_data), union(coVars, condVars) )
    discreteVars <- names(x$varTypes[x$varTypes == "discrete"])
    
    nonEmpty <- function(data, varTypes = x$varTypes) {
        if (ncol(data) == 0L) data$blank <- ""
        data
      }
    
    discretePart <- function(data, varTypes = x$varTypes) {
      data[ , intersect(names(data), names(varTypes[varTypes == "discrete"])), drop=FALSE]
    }
    
    my_interaction <- function( data, drop = TRUE, sep = ":", lex.order = TRUE ) {
      if (ncol(data) == 0L) {
        factor(rep("", nrow(data)))
      } else {
        interaction( data, drop = drop, sep = sep, lex.order = TRUE )
      }
    }
      

# Some aux variables:
#   .color: one for each combination of discrete co-vars
#       this determines the color of the lines
#   .group: one for each combination of co-vars (selected levels of contintuous too)
#       this determins which points are connected to make curves
#   .cond:  one for each combination of discrete cond-vars
#       this determines which lines show up in which panels
    
    point_data <- 
      x$data %>%
      mutate(
        .color = my_interaction(x$data[, intersect(discreteVars, restVars), 
                                       drop = FALSE]),
        .group = my_interaction(x$data[, c(coVars, restVars), drop = FALSE]),
        .cond = my_interaction(x$data[, condVars, drop = FALSE])
      )
   
    
# create data for lines
    # utility function 
    some_levels <- function(x) {
      if (is.numeric(x)) 
        return( sort(unique(ntiles(x, 3, format = "median"))) )
      return(sort(unique(x)))
    }
   
    # a few values of each explanatory variable
    
    levels_all <- expand.grid(lapply(other_data, some_levels))
    levels_shown <- levels_all
    
    # convert the model into a function
    modelFunc <- makeFun(x$model)
   
    # eplanatory variable
    
    if (x$varTypes[key] == "continuous") {
      p <- xyplot( formula,  data = x$data )
      x_points <- seq(p$x.limits[1], p$x.limits[2], length.out = 40)
    } else {
      x_points <- unique( x$data[[key]] )
    }
    
    if (length(restVars) > 0L && is.null(auto.key)) {
      auto.key <- 
        list(points = TRUE, lines=TRUE, 
             columns = min(length(unique(point_data$.color)), 3))
    } else { 
      if (is.null(auto.key)) auto.key <- FALSE
    }
    
    if (nrow(levels_shown) == 0L) {
      levels_shown <- data.frame( matrix(nrow = length(x_points), ncol=0) )
    }
      
    line_data <- bind_rows(lapply(1:length(x_points), function(x) levels_shown)) 
    
    line_data[["x"]] <-
      line_data[[key]] <- 
      rep(x_points, times = nrow(levels_shown))
    
    line_data[["y"]] <- 
      line_data[[x$responseName]] <- 
      predict(x$model, newdata=line_data, type="response")
    
    line_data <- 
      line_data %>%
      mutate(
        .color = my_interaction(line_data[, restVars, drop = FALSE]),
        .group = my_interaction(line_data[, c(coVars, restVars, condVars), drop = FALSE]),
        .cond = my_interaction(line_data[, condVars, drop = FALSE])
      )
    
    
    mypanel <- 
      function(x, y, line_data, point_data, 
               group.number = NULL, group.value = NULL, type = "p",
               ...) {
        panel.xyplot(x, y, type = "p", ...)
        line_data <- 
          line_data %>% 
          arrange(.color, .group, x) 
        if (! is.null(group.value) ) {
          line_data <- line_data %>% filter(as.numeric(.cond) == packet.number())
        }
        ncolors <- length(unique(line_data$.color))
        ngroups <- length(unique(line_data$.group))
        grid::grid.polyline(
          x = line_data$x, 
          y = line_data$y, 
          default.units = "native",
          id = as.numeric(line_data$.group),
          gp = gpar(
            col = trellis.par.get("superpose.line")$col[1:base::max(as.numeric(line_data$.color))]
          )
        )
      }
    if (system == "ggplot2") {
      ggplot() +
        geom_point(aes_string(y = x$responseName, x = key, colour=".color", group=".group"), size=1.2,
                   data = point_data %>% droplevels()) +
        geom_line (aes_string(y = x$responseName, x = key, colour=".color", group = ".group"), size=0.5,
                   data = line_data %>% droplevels())
    } else {
      xyplot(formula, 
             data = point_data %>% droplevels(),
             line_data = line_data %>% droplevels(),
             point_data = point_data %>% droplevels(),
             groups = .color,
             auto.key = auto.key,
             ...,
             panel = panel.superpose,
             panel.groups = mypanel) 
#       xyplot( formula, data = point_data,
#               groups = .group,
#               auto.key = TRUE,
#               ...) +
#         latticeExtra::as.layer(
#           xyplot(formula, data = line_data,
#                  type = 'l',
#                  groups = .group,
#                  auto.key = TRUE,
#                  ...
#           ))
#                  
      
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
