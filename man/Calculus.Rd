\name{D}
\alias{antiD}
\alias{D}
\title{Derivative and Anti-derivative operators}
\usage{
  D(expr, ..., ..h.. = NULL, symbolic = TRUE,
    numerical = !symbolic, method = c("center", "right"))

  antiD(expr, from = 0, to = NULL, ...)
}
\arguments{
  \item{expr}{a formula. The right side specifies the
  variable(s) with which to carry out the integration or
  differentiation.  On the left side should be an
  expression or a function that returns a numerical vector
  of the same length as its argument.  The expression can
  contain unbound variables.}

  \item{..h..}{horizontal distance between points used for
  secant slope calculation in \code{D()}.  This is used
  only if a symbolic derivative is not possible or if
  \code{numerical=TRUE}.  The odd name, \code{..h..}, is
  there to avoid conflicts with unbound variables in the
  formula f.}

  \item{symbolic}{a logical indicating whether symbolic
  differentiation should be attempted}

  \item{numerical}{opposite of symbolic available for
  convenience}

  \item{method}{For first-order numerical derivatives,
  whether to use a centered difference or a right-facing
  difference.}

  \item{\dots}{Default values to be given to unbound
  variables in the expression \code{expr}. See examples.}

  \item{from}{Default value for the lower bound of the
  interval of integration.  This can be set at the time the
  integral function is invoked.}

  \item{to}{Default value for the upper bound of the
  interval of integration. This can be set at the time the
  integral function is invoked (and usually is).}
}
\value{
  For derivatives, the return value is a function of the
  variable(s) of differentiation, as well as any other
  symbols used in the expression.  Thus, \code{D(A*x^2 +
  B*y ~ x + y)} will compute the mixed partial with respect
  to x then y (that is, \eqn{\frac{d^2
  f}{dy\;dx}}{d2f/dydx}).  The returned value will be a
  function of x and y, as well as A and B.  In evaluating
  the returned function, it's best to used the named form
  of arguments, to make sure that you have the order right.

  For anti-derivatives, a decorated function (eventually
  this will be an object of a new class).
}
\description{
  Operators for computing derivatives and anti-derivatives
  as functions.
}
\details{
  \code{D} attempts to find a symbolic derivative for
  simple expressions, but will provide a function that is a
  numerical derivative if the attempt at symbolic
  differentiation is unsuccessful.  The symbolic derivative
  can be of any order (although the expression may become
  unmanageably complex).  The numerical derivative is
  limited to first or second-order partial derivatives
  (including mixed partials).

  \code{antiD} always returns a numerical integral.

  For integrals, the return value is a function NOT of the
  variable (only one!) of integration, but of \code{to} and
  \code{from}, the upper and lower bounds of the interval
  of integration.

  Since the return value is a function, the numerical value
  of the integral or derivative can be found by evaluating
  that function against its inputs.
}
\note{
  A major redesign of these functions is planned that will
  (1) refactor code so that the objects involved are S4
  objects, and (2) allow \code{antiD} to return a single
  value if both \code{to} and \code{from} are specified.
}
\examples{
D(sin(t) ~ t)
D(A*sin(t) ~ t )
D(A*sin(2*pi*t/P) ~ t, A=2, P=10) # default values for parameters.
f <- D(A*x^3 ~ x + x, A=1) # 2nd order partial -- note, it's a function of x
f(x=2)
f(x=2,A=10) # override default value of parameter A
g = D(f(x=t, A=1)^2 ~ t)  # note: it's a function of t
g(t=1)
gg <- D(f(x=t,A=B)^2 ~ t, B=10)  # note: it's a function of t and B
gg(t=1)
gg(t=1, B=100)
F <- antiD( A*exp(-k*t^2 ) ~ t, A=1, k=0.1)
F(from=-Inf, to=0)
F(from=-Inf, to=Inf)
}
\author{
  Daniel Kaplan (\email{kaplan@macalester.edu})
}
\keyword{calculus}

