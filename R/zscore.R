#' Compute z-scores
#' 
#' Compute z-scores
#' @param x a numeric vector
#' @param na.rm a logical indicating whether missing values should be removed
#' @export
#' @examples
#' iris %>% 
#'   group_by(Species) %>% 
#'   mutate(zSepal.Length = zscore(Sepal.Length)) %>% 
#'   head()
zscore <- function( x, na.rm=getOption("na.rm", FALSE) ) {
  ( x - mean(x, na.rm=na.rm)) / sd(x, na.rm=na.rm)
}

