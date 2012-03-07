#' Display results from permutation tests
#' 
#' A function to display results from permutation tests 
#' 
#' @rdname plotNullDist
#' @param data dataframe containing estimates of null distribution from a permutation test
#' @param observed value that was observed from the data
#' @param alternative alternative hypothesis to use to compute p-value (one of "greater", "less" or "two-sided", default is "greater")
#' @param offset proportion of space to leave on both sides of the x axis
#' @return p-value
#' @author Nicholas Horton (\email{nhorton@@smith.edu})
#' @seealso \code{\link{do}}, \code{\link{compareMean}}, \code{\link{compareProportion}} and \code{\link{shuffle}}
#' @keywords resampling
#' @export
#' @examples
#' # calculate the observed difference
#' mean(age ~ sex, data=HELPrct)
#' obs <- compareMean(age ~ sex, data=HELPrct); obs
#' # calculate the null distribution through permutation of the group labels
#' nulldist <- do(100) * compareMean(age ~ shuffle(sex), 
#'   data=HELPrct) 
#' plotNullDist(nulldist, obs)
plotNullDist = function(data, observed, alternative="greater", offset=.15) {
  if (alternative %in% c("greater", "less", "two-sided") == FALSE) {
    stop("alternative must be one of \"greater\", \"less\", or \"two-sided\"")
  }
  values = c(data$result, observed)
  xlimvals = c(min(values)-range(values)*offset,
               max(values)+range(values)*offset)
  if (alternative=="greater") {
    extreme = data$result >= observed
  } else if (alternative=="less") {
    extreme = data$result <= observed
  } else if (alternative=="two-sided") {
    extreme = abs(data$result) >= abs(observed)
  }
  pvalue = sum(extreme)/length(data$result)
  mytitle = paste("observed difference=", round(observed, 3),
                  ", p-value=", round(pvalue, 3), sep="")
  print(xhistogram(~ result, xlim=xlimvals, groups=extreme,
                   data=data, xlab="difference under the null", main=mytitle))
  ladd(panel.abline(v=observed, col="red", lwd=2))
  return(pvalue)
}
