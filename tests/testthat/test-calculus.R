context('Testing calculus functions: D(), antiD(), etc.')

test_that("stats::D still accessible", {
  expect_equivalent( 
		mosaic::D( expression(sin(cos(x + y^2))), "x" ), 
		 stats::D( expression(sin(cos(x + y^2))), "x" )
	) 
  expect_equivalent( 
		mosaic::D( expression(sin(cos(x + y^2))), "x" ), 
		        D( expression(sin(cos(x + y^2))), "x" )
	) 
})

test_that("a function is created", {
  expect_true(is.function(D(sin(x) ~ x))) #symbolic Deriv
  expect_true(is.function(symbolicD(sin(x) ~ x))) #symbolic Deriv
  expect_true(is.function(numD(pt(x,3)*sin(x) ~ x))) #numerical deriv
  expect_true(is.function(antiD(pt(x,3)*sin(x) ~ x))) #antiD
})

test_that("default parameters are used", {
  g <- D( a*x^2 ~ x, a=10 )
  expect_that(g(1:10), equals(10*2*(1:10),tol=.0001) )
  expect_that(g(1:10,a=5), equals(5*2*(1:10),tol=.0001) )
})

test_that("parameters are retained even if killed in the derivative",{
  g <- D( A*x^2 + b~x)
  expect_true( "b" %in% names(formals(g)))
})

test_that("default values of parameters are retained in the derivative",{
  g <- D(A*x^2 + b~x, A=10, b=15)
  expect_that( formals(g)[["b"]], equals(15))
  expect_that( formals(g)[["A"]], equals(10))
  g <- numD(A*x^2 + b~x, A=20, b=25)
  expect_that( formals(g)[["b"]], equals(25))
  expect_that( formals(g)[["A"]], equals(20))
})

test_that("derivs with respect to irrelevant variables are zero",{
  g <- D(a*x^2 + b~x&y, a=10, b=15)
  expect_that( body(g), equals(0))
  expect_true( "y" %in% names(formals(g)))
})

test_that("default values of parameters retained in the anti-derivative",{
  g <- antiD(A*exp(x^2) + b~x, A=10, b=15) #numerical
  expect_that( formals(g)[["b"]], equals(15))
  expect_that( formals(g)[["A"]], equals(10))
  g <- antiD(A*x^2 + b~x, A=10, b=15) #symbolic
  expect_that( formals(g)[["b"]], equals(15))
  expect_that( formals(g)[["A"]], equals(10))
})

test_that("mixed partials work", {
  g <- D( a*x^2*y^2 ~ x&y, a=10) # numerical
  expect_that( g(x=1:10,y=5), equals(10*2*2*(1:10)*5,tol=.0001) )
  g <- D( a*x^2*y^2 ~ x&y, a=10) # symbolic
  expect_that( g(x=1:10,y=5), equals(10*2*2*(1:10)*5,tol=.0000001) )
})

test_that("unmixed 2nd-order partials work",{
  g <- numD( a*x^2*y^2 ~ x&x, a=10)
  expect_that( g(x=3, y=5), equals(500,tol=0.001)) 
  g <- numD( a*x^2*y^2 ~ y&y, a=10)
  expect_that( g(x=3, y=5), equals(180,tol=0.001)) 
  g <- numD( a*x^2*y^2 ~ a&a, a=10)
  expect_that( g(x=3, y=5), equals(0,tol=0.001)) 
})

test_that("basic integration works", {
  f <- antiD( a*x ~ x, a=10)
  expect_that( f(3), equals(45,tol=.001))
  expect_that( f(x=3), equals(45, tol=.001))
  expect_that( f(3,a=100), equals(450, tol=.001))
  expect_that( f(x=3,a=100), equals(450, tol=.001))
})

test_that("Default limits in integrals work",{
  f <- antiD( dnorm(x) ~ x, lower.bound=-5)
  expect_that( f(x=3), equals(pnorm(3)-pnorm(-5), tol=0.001))
  f <- antiD( dnorm(x) ~ x, lower.bound=-3)
  expect_that( f(x=3), equals(pnorm(3)-pnorm(-3), tol=0.001))
  f <- antiD( dnorm(x) ~ x, lower.bound=3)
  expect_that( f(x=-3), equals(pnorm(-3)-pnorm(3), tol=0.001))
})

test_that("Integrals work with Inf args",{
  f <- antiD(dnorm(x)~x)
  expect_that( f(x=0)-f(x=-Inf),equals(.5,tol=0.0001))
  expect_that( f(x=Inf), equals(.5, tol=0.0001))
  expect_that( f(x=-Inf), equals(-.5, tol=0.0001))
})

test_that("Initial condition (constant of integration) works", {
  f <- antiD( 1+ 0*exp(t^2)~t, force.numerical=TRUE) #numerical
  expect_that( f(t=0), equals(0, tol=0.00001))
  expect_that( f(t=5), equals(5, tol=0.00001))
  expect_that( f(t=0, C=2), equals(2, tol=0.00001))
  expect_that( f(t=5, C=2), equals(7, tol=0.00001))
})

test_that("Symbols for constant of integration work", {
  vel <- antiD( -9.8 + 0*exp(t^2) ~ t  ) # numerical
  pos <- antiD( vel( t=t, C=v0)~t )
  expect_that(pos(5, v0=10, C=100), equals(27.5,tol=0.00001) )
})

test_that("derivatives work in derivatives",{
  g <- numD( a*x^2 + x*y ~ x, a=1)  
  h <- numD( g(x=x,y=y,a=a) ~ y, a=1)
  expect_that( h(x=2,y=10),equals(1,tol=.001))
  g <- symbolicD( a*x^2 + x*y ~ x, a=1)  
  h <- numD( g(x=x,y=y,a=a) ~ y, a=1)
  expect_that( h(x=2,y=10),equals(1,tol=.001))
})


test_that("integrals work in other functions", {
  f <- antiD( a + 0*exp(x^2)~x, a=10 ) # numerical
  h <- makeFun(f(x)~x)
  expect_that( h(4), equals(f(4)))
  h <- makeFun(f(x=x,a=100)~x)
  expect_that( h(4), equals(f(4,a=100)))
  h <- makeFun(f(x=x,a=a)~x,a=20)
  expect_that( h(4),equals(f(4,a=20)))
})

test_that("integrals and derivatives interoperate", {
  F <- antiD(x~x)
  f <- D( F(x=x)~x )
  expect_that( f(3),equals(3,tol=0.00001))
})

test_that("integrals work on integrals", {
  one <- makeFun(1~x&y)
  by.x <- antiD( one(x=x, y=y) ~x )
  by.xy <- antiD(by.x(x=sqrt(1-y^2), y=y)~y)
  expect_that( by.xy(y=1)-by.xy(y=-1), equals(pi/2,tol=0.00001))
})

test_that("Basic numerical differentiation works", {
  g <- numD( a*x^2 + x*y ~ x, a=1)  
  expect_that( g(x=2,y=10), equals(14, tol=0.0001))
  gg <- numD( a*x^2 + x*y ~ x&x, a=1)
  expect_that( gg(x=2,y=10), equals(2, tol=0.0001))
  ggg <- numD( a*x^2 + x*y ~ x&y, a=1)
  expect_that( ggg(x=2,y=10,a=10), equals(1, tol=0.0001))
})

test_that("symbolic parameters are passed correctly",{
  f <- function(n) {
    numD( b*a*x^2*y^2 ~ x & y, a=.25, b=n) 
  }
  h <- function(x) {
    f(x)
  }
  g <- h(10)
  expect_that(g(1, 1), equals(10, tol=0.0001))
  expect_that(g(1, 1, b=20), equals(20, tol=0.0001)) 
  expect_that(g(1, 1, a=2, b=20), equals(160, tol=0.001)) 
})

test_that("Derivatives can be iterated.",{
  g <- numD( a*x^3 ~ x, a=10, .hstep=.001)
  gg <- numD( g(y)~y, .hstep=.001 )
  ggg <- numD( gg(x)~x, .hstep=.001 )
  expect_that(gg(3), equals(180, tol=.01)) 
  expect_that(ggg(3), equals(60, tol=.01)) 
})

test_that("Derivatives can be composed.",{
  ff <- makeFun( sin(x)~x )
  h <- numD( ff(x)~x )
  hh <- numD( sin(y)*ff(y)~y )
  expect_that(hh(y=3), equals(2*sin(3)*cos(3),tol=0.0001))
  hhh <- numD( sin(y)*h(y)~y ) 
  expect_that( hhh(y=3), equals(cos(3)*cos(3) - sin(3)*sin(3),tol=0.0001 ))
})

test_that("Vars. killed by differentiation remain arguments",{
  g <- numD( f^2*h~f&h)
  expect_that(g(f=2, h=1), equals(4,tol=0.0001))
})

test_that("add.h.control works",{
  f <- numD( sin(x)~x, add.h.control=TRUE)
  equals(f(3, .hstep=1), equals(-.83305,tol=0.0001)) 
})

test_that("symbolic derivative on simple function works",{
  f <- makeFun(x^2~x)
  g <- makeFun(sin(x)+x~x)
  fprime <- D(f(z)~z)
  gprime <- D(g(y)~y)
  expect_that(fprime(5), equals(2*5, tol=0.0000001))
  expect_that(gprime(2), equals(cos(2)+1, tol=0.0000001))
}) 

test_that("integration bug with -Inf as lower bound is worked around",{
  F <- antiD(dnorm(x)~x)
  expect_that(F(-Inf), equals(-.5, tol=0.00001))
  g <- makeFun( ifelse( x>0 & x<1, 1, 0) ~ x)
  G <- antiD(g(x)~x)
  expect_that(G(-Inf), equals(0, tol=0.00001))
})