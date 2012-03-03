#' Display results from permutation tests
#' 
#' A function to display results from permutation tests 
#' 
#' @rdname displayPermute
#' @param data dataframe containing results from a permutation test
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
#' obs = compareMean(age ~ sex, data=HELPrct); obs
#' mean(age ~ sex, data=HELPrct)
#' # calculate the permutation distribution
#' permute = do(100) * compareMean(age ~ shuffle(sex), 
#'   data=HELPrct) 
#' displayPermute(permute, obs)
displayPermute = function(data, observed, alternative="greater", offset=.15) {
  if (alternative %in% c("greater", "less", "two-sided") == FALSE) {
    stop("alternative must be one of \"greater\", \"less\", or \"two-sided\"")
  }
  values = c(permute$result, observed)
  xlimvals = c(min(values)-range(values)*offset,
               max(values)+range(values)*offset)
  if (alternative=="greater") {
    extreme = permute$result >= obs
  } else if (alternative=="less") {
    extreme = permute$result <= obs
  } else if (alternative=="two-sided") {
    extreme = abs(permute$result) >= abs(obs)
  }
  pvalue = sum(extreme)/length(permute$result)
  mytitle = paste("observed difference=", round(observed, 3),
                  ", p-value=", round(pvalue, 3), sep="")
  print(xhistogram(~ result, xlim=xlimvals, groups=extreme,
                   data=data, xlab="difference under the null", main=mytitle))
  ladd(panel.abline(v=observed, col="red", lwd=2))
  return(pvalue)
}
