#' Logit and inverse logit functions
#' 
#' Logit and inverse logit functions
#'
#' @param x  a numeric vector 
#' 
#' @return For \code{logit} the value is 
#' \deqn{log(x/(1 - x))}
#' 
#' For \code{ilogit} the value is 
#' \deqn{exp(x)/(1 + exp(x))}
#' 
#' 
#' @examples
#' p <- seq(.1, .9, by=.10)
#' l <- logit(p); l
#' ilogit(l)
#' ilogit(l) == p
#' @export

logit <- function(x) 
{
	log(x/(1 - x))
}

#' @rdname logit
#' @export

ilogit <- function (x) 
{
    exp(x)/(1 + exp(x))
}

