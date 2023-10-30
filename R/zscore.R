#' Compute z-scores
#' 
#' Compute z-scores
#' @param x a numeric vector
#' @param na.rm a logical indicating whether missing values should be removed
#' @export
#' @examples
#' data(penguins, package = "palmerpenguins")
#' penguins |> 
#'   group_by(species) |> 
#'   mutate(zbill_length_mm = zscore(bill_length_mm, na.rm = TRUE)) |> 
#'   head()
zscore <- function( x, na.rm=getOption("na.rm", FALSE) ) {
  ( x - mean(x, na.rm=na.rm)) / sd(x, na.rm=na.rm)
}

