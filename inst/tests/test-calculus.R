context('Testing calculus functions: D(), antiD(), etc.')


test_that("a function is created", {
  expect_true(is.function(D(sin(x) ~ x))) #symbolic Deriv
  expect_true(is.function(symD(sin(x) ~ x))) #symbolic Deriv
  expect_true(is.function(numD(pt(x,3)*sin(x) ~ x))) #numerical deriv
  expect_true(is.function(antiD(pt(x,3)*sin(x) ~ x))) #antiD
})

test_that("default parameters are used", {
  g <- D( a*x^2 ~ x, a=10 )
  expect_that(g(1:10), equals(10*2*(1:10),tol=.0001) )
  expect_that(g(1:10,a=5), equals(5*2*(1:10),tol=.0001) )
})

test_that("mixed partials work", {
  g <- D( a*x^2*y^2 ~ x&y, a=10) # numerical
  expect_that( g(x=1:10,y=5), equals(10*2*2*(1:10)*5,tol=.0001) )
  g <- D( a*x^2*y^2 ~ x&y, a=10) # symbolic
  expect_that( g(x=1:10,y=5), equals(10*2*2*(1:10)*5,tol=.0000001) )
})

do.tests = function(){
  too.different = function(x, y, tol=.001){abs(x-y) > tol}
  # Simple tests
  g = numD( a*x^2 ~ x, a=10 )
  if(too.different( g(3), 60 )) stop("Test 1")
  g = numD( a*x^2*y^2 ~ x&y, a=10)
  if(too.different( g(x=3, y=5), 600 )) stop("Test 2")
  g = numD( a*x^2*y^2 ~ x&x, a=10)
  if(too.different( g(x=3, y=5), 500)) stop("Test 3a")
  g = numD( a*x^2*y^2 ~ y&y, a=10)
  if(too.different( g(x=3, y=5), 180)) stop("Test 3b")
  g = numD( a*x^2*y^2 ~ a&a, a=10)
  if(too.different( g(x=3, y=5), 0)) stop("Test 3c")
  ## Construct the derivative in a function, then return it
  f = function(n) {
    numD( b*a*x^2*y^2 ~ x & y, a=.25, b=n) 
  }
  g = f(10)
  if(too.different(g(1, 1), 10)) stop("Test 4a")
  if(too.different(g(1, 1, b=20), 20)) stop("Test 4b")
  if(too.different(g(1, 1, a=2, b=20), 160)) stop("Test 4c")
  # Iterative derivatives
  g = numD( a*x^3 ~ x, a=10, .hstep=.001)
  gg = numD( g(y)~y, .hstep=.001 )
  ggg = numD( gg(x)~x, .hstep=.001 )
  if(too.different(gg(3), 180, tol=.01)) stop("Test 5a")
  if(too.different(ggg(3), 60, tol=.01)) stop("Test 5b")
  # Function composition
  ff = makeFunction( sin(x)~x )
  h = numD( ff(x)~x )
  hh = numD( sin(y)*ff(y)~y )
  if(too.different( hh(y=3), 2*sin(3)*cos(3))) stop("Test 6a")
  hhh = numD( sin(y)*h(y)~y ) 
  if(too.different( hhh(y=3), cos(3)*cos(3) - sin(3)*sin(3) )) stop("Test 6b")
  
  # Use internal variables to make sure there are no conflicts
  g = numD( f^2*h~f&h)
  if(too.different(g(f=2, h=1), 4)) stop("Test 7a")
  
  # Check a function defined outside this one
  testgg = numD( testg(y)~y)
  if(too.different(testgg(2),2)) stop("Test 8a")
  
  #Control of .hstep
  f = numD( sin(x)~x, add.h.control=TRUE)
  if(too.different(f(3, .hstep=1), -.83305)) stop("Test 9a")
}


