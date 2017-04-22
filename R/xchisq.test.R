#' Augmented Chi-squared test
#' 
#' This augmented version of \code{\link{chisq.test}} provides more verbose
#' output.
#'
#' @param x,y,correct,p,rescale.p,simulate.p.value,B as in \code{\link{chisq.test}}, but 
#' \code{x} may also be a formula, in which case \code{x} is replaced by \code{tally(x, data)}
#' prior to the call to \code{\link{chisq.test}()}.
#' @param data a data frame for use when \code{x} is a formula.
#' 
#' @seealso \code{\link{chisq.test}()} 
#' 
#' @examples
#' # Physicians' Health Study data
#' phs <- cbind(c(104,189),c(10933,10845)) 
#' rownames(phs) <- c("aspirin","placebo") 
#' colnames(phs) <- c("heart attack","no heart attack") 
#' phs 
#' xchisq.test(phs) 
#' xchisq.test(sex ~ substance, data = HELPrct)
#' @export

xchisq.test <-
function (x, y=NULL, correct=TRUE, p = rep(1/length(x), length(x)),
          rescale.p=FALSE, simulate.p.value=FALSE, B=2000, data = environment(x)) 
{
  if (inherits(x, "formula")) {
    orig <- chisq.test(tally(x, data = data), correct=correct, p = p, 
                       rescale.p = rescale.p, 
                       simulate.p.value=simulate.p.value, B=B)
  } else {
    
    orig <- chisq.test(x=x, y=y, correct=correct, p = p, 
                       rescale.p = rescale.p, 
                       simulate.p.value=simulate.p.value, B=B)
  }
    if (is.matrix(orig$observed)) {
        dd <- dim(orig$observed)
    }
    else {
        dd <- c(1, length(orig$observed))
    }
    obs <- surround(orig$observed, " ", " ", digits = 2, nsmall = 2)
    exp <- surround(orig$expected, "(", ")", digits = 2, nsmall = 2)
    contribution <- (orig$observed - orig$expected) ^ 2 / orig$expected
	if (correct && all(dd == c(2,2)) && ! simulate.p.value) {
		# use continuity correction
		contrib2 <- (abs(orig$observed - orig$expected) - .5)^2 / orig$expected
		contribution <- pmin(contribution, contrib2)
	}
    contrib <- surround(contribution, "[", "]", digits = 2, nsmall = 2)
    resid <- surround(orig$residuals, "<", ">", digits = 2, nsmall = 2)
    blank <- rep(" ", prod(dd))
    result <- c(obs, exp, contrib, resid, blank)
    dim(result) <- c(dd, 5)
    print(orig)
    for (i in 1:dd[1]) {
        for (j in 1:5) {
            cat(result[i, , j])
            cat("\n")
        }
    }
    cat("key:\n")
    cat("\tobserved\n")
    cat("\t(expected)\n")
    cat("\t[contribution to X-squared]\n")
    cat("\t<Pearson residual>\n")
    orig$contribution <- contribution
    return(invisible(orig))
}
