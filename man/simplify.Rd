\name{.polyExp}
\alias{.makePoly}
\alias{.polyExp}
\alias{.polyExp.num}
\title{Takes a call and returns its polynomial coefficients}
\usage{
  .polyExp(tree, .x., params, iterate = 1)

  .polyExp.num(tree, .x.)

  .makePoly(form, poly)
}
\arguments{
  \item{tree}{A call that will be parsed and simplified
  recursively}

  \item{.x.}{the variable name with respect to which the
  polynomial should be most simplified}

  \item{params}{All names of free variables.  If there are
  no free variables, the value should be ""}

  \item{iterate}{The number of times the call is nested.
  Default and proper value when called from the outside is
  1}

  \item{poly}{output of .polyExp()}

  \item{form}{original formula - provides information on
  which variable the polynomial was reduced with respect
  to.}
}
\value{
  A list containing a list, \code{coeffs}, of coefficients
  ordered high to low (i.e. the list (2,3,4) would
  correspond to the polynomial 2*x^2+3*x+4 ) and value,
  \code{pow}, indicating the order of the polynomial. If
  the expression is not a polynomial, this method returns
  an empty list or an error.

  A list containing a list, \code{coeffs}, of coefficients
  ordered high to low (i.e. the list (2,3,4) would
  correspond to the polynomial 2*x^2+3*x+4 ) and value,
  \code{pow}, indicating the order of the polynomial. If
  the expression is not a polynomial, this method returns
  an empty list or an error.

  A formula whose left hand side is a polynomial that fits
  the description given with the input poly.
}
\description{
  Takes a call and returns its polynomial coefficients

  Takes a call and returns its polynomial coefficients as
  numerics.

  Method for putting a polynomial together given the
  coefficients and power from .polyExp()
}
\details{
  Will work on any call as long as it can be reduced to a
  polynomial with respect the the variable and each of the
  parameters.  Operates recursively, reducing each of the
  coefficients with respect to the extra parameters in
  turn.  Calls .polyExp.num when all remaining coefficients
  are numeric to reduce the expression more fully.

  works with the same structure as .polyExp() but will
  return only if all coefficients reduce to numeric values.
}
\examples{
mosaic:::.polyExp(lhs((2*x+x^3+1)^3+x~x), "x", "")
mosaic:::.polyExp(lhs((3+a*x)^2+a^2*(x+2)~x), "x", "a")
mosaic:::.polyExp(lhs((a+b*x)^3~x), "x", c("a", "b"))
}

