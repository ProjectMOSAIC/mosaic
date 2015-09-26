#' Groupwise models
#'  
#' Construct a model based on groupwise means or proportions
#' 
#' @rdname gwm
#' 
#' @param formula A formula.  The left-hand side specifies the response variable 
#' over which the mean or proportion will be taken.  The right-hand side gives 
#' the explanatory variables, separated by \code{+}.  Means or proportions are
#' computed for every combination of the levels of the explanatory variables.
#' 
#' @param data A data frame in which to evaluate variables in \code{formula}.  
#' If omitted, refer.  If not specified, variables 
#' will be taken from the current environment.
#' 
#' @param drop Logical flag indicating whether to drop unoccupied groups.  
#' Default \code{FALSE}.  NOT YET IMPLEMENTED.
#' 
#' @param \dots Additional arguments; currently ignored.
#' 
#' @return \code{mm} returns an object of class \code{groupwiseModel}.  The functions 
#' \code{fitted.values}, \code{residuals}, \code{coefficients}, and \code{summary} 
#' are useful for extracting various features of the value returned by \code{mm}
#' 
#' @details 
#' \code{gwm} (groupwise model) is a sort of training function for 
#' \code{lm}, meant to provide a basis for discussing inference and introducing 
#' resampling in a simple, intuitive setting 
#' of groupwise means or proportions.  \code{lm} provides a better, more general facility. 
#' When using \code{lm} to recreate the results of \code{gwm}, include all the 
#' interaction terms (i.e., use \code{*} instead of \code{+}) and remove the 
#' intercept term.  See the examples.
#' 
#' @seealso 
#' \code{\link{lm}}, 
#' \code{\link{do}}
#' @export
#' @examples
#' 
#' gwm( wage ~ sex, data=CPS85 )
#' gwm( wage ~ sex + married, data = CPS85 )
#' # The same model, fit using lm() instead
#' lm( wage ~ sex * married - 1, data = CPS85)
#' do(5) * gwm( wage ~ sex + married, data = resample(CPS85))
#' mod <- gwm( width ~ domhand, data=KidsFeet)
#' summary(mod)
#' resid(mod)
#' fitted(mod)

gwm <- function(formula, data = parent.frame(), drop = FALSE, ...) {
  # if response categorical, a proportion, listing the response
  # if response quantitative, a mean but don't list the response
  orig.call <- match.call()
  response_var <- formula[[2]]
  response <- eval(response_var, envir=data)
  ResponseData <- data.frame(x = response) 
  names(ResponseData) <- as.character(response_var)
  group_vars <- all.vars(formula[[3]])
  if (is.numeric(response)) {
    grouping <- if (length(group_vars) <= 0 ) {
      group_vars <- c()
    }
    Res <- 
      data %>%
      group_by_(.dots = group_vars) %>%
      summarise(model_value = mean(response_var, na.rm=TRUE)) %>%
      ungroup()
    observed <- data[[as.character(response_var)]]
    
  } else {
    Res <- data.frame(tally(formula, data=data, format="proportion"))
    names(Res)[ncol(Res)] <- "model_value" 
    if (length(all.vars(formula[[3]])) == 0) {
      # the constant model
      # get rid of any pseudo variables added by tally
      keepers <- names(Res) %in% c(names(data), "model_value")
      Res <- Res[,keepers]
    }
    observed <- rep(1, nrow(data))
  }
  
  # Compute fitted, and residuals
  if (length(group_vars) > 0L) {
    fitted.values <- suppressMessages(left_join(data, Res))$model_value
  } else {
    fitted.values <- rep(Res$model_value, ncol(data))
  }
 
  residuals <- observed - fitted.values
  
  structure(
    list(coefficients = Res, 
         residuals = residuals,
         fitted.values = fitted.values,
         response = observed,
         data = data,
         call = orig.call,
         type = ifelse(is.numeric(response), "mean", "probability")
    ),
    class = "groupwiseModel"
  )
}

#' @export
print.groupwiseModel <-
  function (x, ..., digits = max(3, getOption("digits") - 3)) 
{
  cat("\nGroupwise Model Call:\n", paste(deparse(x$call), sep = "\n", 
                                         collapse = "\n"), "\n\n", sep = "")
  print(format(x$coefficients, digits = digits))
  cat("\n")
  invisible(x)
  }


# summary() doesn't do anything exciting at the moment.  This is just a placeholder
# for future functionality.

#' @export
summary.groupwiseModel <- function( object, ...) {
  structure(
    list(model = object),
    class = "summary.groupwiseModel"
  )
}


#' @export
print.summary.groupwiseModel <- function( x, ...) {
  print(x$model)
}

#' @export
predict.groupwiseModel <- function( object, newdata = NULL, ... ) {
  if (is.null(newdata)) 
    return(object$fitted.values)
 
  if (ncol(object$coefficients) >= 2L) {
    fitted.values <- 
      suppressMessages(left_join(newdata, object$coefficients))$model_value
  } else {  # only happens for null model with quant. reponse
    fitted.values <- rep(object$coefficients$model_value, ncol(newdata))
  } 
}

#' Mean Squared Prediction Error
#' 
#' Mean Squared Prediction Error
#' 
#' @param model a model produced by \code{lm}, \code{glm}, or \code{gwm}.
#' @param data a data frame
#' 
#' @export
#' @examples
#' HELP <- HELPrct %>% sample_frac(.3)
#' MSPE( gwm( age ~ sex, data = HELP), HELPrct)
#' MSPE( gwm( age ~ 1, data = HELP), HELPrct)
#' MSPE( gwm( age ~ sex + homeless, data = HELP), HELPrct)
#' MSPE( gwm( sex ~ 1, data = HELP), HELPrct)
#' MSPE( gwm( sex ~ homeless, data = HELP), HELPrct)
#' MSPE( gwm( sex ~ homeless + substance, data = HELP), HELPrct)

MSPE <- function(model, data){
  #  was <- options("warn")
  #  on.exit(options(warn = was))
  options(warn = -3)
  formula <- model$call[[2]]
  actual <- eval(formula[[2]], envir = data)
  # adjust for categorical response
  if (!is.numeric(actual)) { actual <- 1}
  model_vals <- predict(model, newdata = data)
  stats::var(actual - model_vals, na.rm=TRUE)
}

#' @export
cull_for_do.groupwiseModel <- function( object, ... ) {
  object$coefficients
}