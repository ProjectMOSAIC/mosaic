require(dplyr)

group_mod <- function(formula, data = parent.frame()) {
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
# ==========================

print.groupwiseModel <-
  function (x, ..., digits = max(3, getOption("digits") - 3)) 
{
  cat("\nGroupwise Model Call:\n", paste(deparse(x$call), sep = "\n", 
                                         collapse = "\n"), "\n\n", sep = "")
  print(format(x$coefficients, digits = digits))
  cat("\n")
  invisible(x)
}

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
#' @examples
#' HELP <- HELPrct %>% sample_frac(.3)
#' MSPE( group_mod( age ~ sex, data = HELP), HELPrct)
#' MSPE( group_mod( age ~ 1, data = HELP), HELPrct)
#' MSPE( group_mod( age ~ sex + homeless, data = HELP), HELPrct)
#' MSPE( group_mod( sex ~ 1, data = HELP), HELPrct)
#' MSPE( group_mod( sex ~ homeless, data = HELP), HELPrct)
#' MSPE( group_mod( sex ~ homeless + substance, data = HELP), HELPrct)
#' 
MSPE <- function(mod, test_data){
  #  was <- options("warn")
  #  on.exit(options(warn = was))
  options(warn = -3)
  # get the model values from predict()
  # and the actual values from the test_data
  formula <- mod$call[[2]]
  actual <- eval(formula[[2]], envir = test_data)
  if (!is.numeric(actual)) { actual <- 1}
  mod_vals <- predict(mod, newdata = test_data)
  # get rid of missing model values
  stats::var(actual - mod_vals, na.rm=TRUE)
}
