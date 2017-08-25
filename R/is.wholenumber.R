#' Check for whole number values
#'
#' Unlike [is.integer()], which checks the type of argument is `integer`, 
#' this function checks whether the value of the argument is an integer
#' (within a specified tolerance).
#'
#' @param x a vector
#' @param tol a numeric tolerance
#' @return a logical vector indicating whether `x` has a whole number value
#' @details
#' This function is borrowed from the examples for [is.integer()]
#'
#' @examples
#' is.wholenumber(1)
#' all(is.wholenumber(rbinom(100,10,.5)))
#' is.wholenumber((1:10)/2)
#' @export

is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
