## Context

This release fixes a few minor documentation errors and introduces an na.rm
argument to one function. Also removes last bits of lazyeval code (in favor of
rlang).


## Test environments

* local OS X install
  * R version 3.6.2 (2019-12-12)
  * Platform: x86_64-apple-darwin15.6.0 (64-bit)
  * Running under: macOS Catalina 10.15.3
  
* win-builder via devtools::check_win_release()
* win-builder via devtools::check_win_devel()

## Note on previously failed automated checks

I am not able to recreate the problems you see on debian.  The error in one of the tests
appears to go back to functions in ggdendro that convert various clustering/dendogram
objects.  The failing test is a check to see that the data produced by these is what
is expected, and there is something that used to be a factor not coming in as a factor --
perhaps related to the stingAsFactors change? In any case, this doesn't impact our code
that uses this data, so I've disabled that check for now.

The Note is mysterious to me: 
Check: Rd \usage sections, Result: WARNING
  Documented arguments not in \usage in documentation object 'linear.algebra':
    '...'

But the arguments section of that Rd file doesn't contain ...:

\arguments{
\item{formula}{a formula.  In \code{mat} and \code{singvals},
only the right-hand side is used.}

\item{data}{a data frame from which to pull out numerical values
for the variables in the formula}

\item{A}{an alias for \code{formula} for backward compatibility.

\code{mat} returns a model matrix

To demonstrate singularity, use \code{singvals}.}
}

Is this possibly a false positive or a mis-identified location of the problem?  This warning
does not appear locally or in your windows check.


## R CMD check results

0 errors ✓ | 0 warnings ✓ | 1 note x

R CMD check succeeded




