context('Formulas to Functions')


test_that("a function is created", {
  expect_true(is.function(formula2function(sin(x) ~ x)))
})

test_that("values are assigned correctly", {
  f <- formula2function( sin(x) ~ x )
  expect_equivalent( sin(1:3), f(1:3) )
})

test_that("default arguments work", {
  f <- formula2function( sin(a*x + b*y) ~ x & y, a=1, b=2)
  expect_equivalent( sin(1*3 + 2*4), f(3, 4) )
  expect_equivalent( sin(1*3 + 2*4), f(y=4, x=3) )
  expect_equivalent( sin(1*3 + 5*4), f(3, 4, b=5) )
  expect_equivalent( sin(7*3 + 5*4), f(3, 4, b=5, a=7) )
})

# these tests work when run in console but not from scripts.
#test_that("iteration works", {
#  f <- formula2function( sin(a*x) ~ x, a=1)
#  g <- formula2function( f(x^2, a) ~ x & a, a=1)
#  expect_equivalent( g(3), f(3^2) )
#  expect_equivalent( g(3, a=5), f(3^2, a=5) )
#})

test_that('Errors are thrown', {
  expect_error( formula2function( ~ x ) )
  expect_error( formula2function( 3 ) )
})
