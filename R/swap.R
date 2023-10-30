#' Swap values among columns of a data frame
#' 
#' Swap values among columns of a data frame
#' 
#' @param data a data frame
#' @param which a formula or an integer or character vector specifying columns in 
#' `data`
#' @details `swap` is not a particularly speedy function.  It is intended primarily
#' as an aid for teaching randomization for paired designs.  Used this way, the number of
#' randomizations should be kept modest (approximately 1000) unless you are very patient.
#' 
#' @export
#' @examples
#' if (require(tidyr)) {
#'   Sleep2 <- sleep |> spread( key=group, val=extra )
#'   names(Sleep2) <- c("subject", "drug1", "drug2")
#'   swap(Sleep2, drug1 ~ drug2)
#'   mean( ~(drug1 - drug2), data=Sleep2)
#'   do(3) * mean( ~(drug1 - drug2), data=Sleep2 |> swap(drug1 ~ drug2) ) 
#' } 
 
swap <- function(data, which) {
 
  if (inherits(which, "formula")) {
    which <- all.vars(which)
    bad <- setdiff(which, names(data))
    if (length(bad) >= 1L) {
      stop( "formula contains variables not in data: ", bad[1] )
    }
  }
  
  if (is.character(which)) {
    which <- match(which, names(data))
  }
  

  if (! is.integer(which) ) stop("Value of `which' doesn't make sense to me.")
  
  if (length(which) <= 1) {
    message("Nothing to swap.  Returning data unchanged.")
    return(data)
  }
  
  subdata <- data[, which]
  n <- nrow(data)
  idx <- do(n) * shuffle(1:ncol(subdata))
  replacement <- data.frame(
    lapply( 1:ncol(subdata), function(i) subdata[cbind(1:n, idx[,i])] )
  )
  res <- data
  res[, which] <- replacement
  
  res
}

# probably don't need this anymore
fetchByCol <- function(data, cols) {
  res <- data[[ cols[1] ]]
  
  for(i in (1:length(res))) {
    res[i] <- data[ i, cols[i] ]
  }
  res
}
