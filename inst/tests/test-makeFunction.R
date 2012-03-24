context('Testing makeFun()')


test_that("a function is created", {
  expect_true(is.function(makeFun(sin(x) ~ x)))
})

test_that("values are assigned correctly", {
  f <- makeFun( sin(x) ~ x )
  expect_equivalent( sin(1:3), f(1:3) )
})

test_that("default arguments work", {
  f <- makeFun( sin(a*x + b*y) ~ x & y, a=1, b=2)
  expect_equivalent( sin(1*3 + 2*4), f(3, 4) )
  expect_equivalent( sin(1*3 + 2*4), f(y=4, x=3) )
  expect_equivalent( sin(1*3 + 5*4), f(3, 4, b=5) )
  expect_equivalent( sin(7*3 + 5*4), f(3, 4, b=5, a=7) )
})


test_that("pi is handled with care", {
  f <- makeFun( sin(a*pi*x + b*y) ~ x & y, a=1, b=2)
  expect_equivalent( sin(1*pi*3 + 2*4), f(3, 4) )
  expect_equivalent( sin(1*pi*3 + 2*4), f(y=4, x=3) )
  expect_equivalent( sin(7*pi*3 + 5*4), f(3, 4, b=5, a=7) )
  g <- makeFun( pi * (1-pi) ~ pi )
  expect_equivalent( g(.5), .5 * .5)
  expect_equivalent( g(0), 0 * 1)
  expect_equivalent( g(pi), pi * (1-pi))
})

# these tests work when run in console but not from scripts.
test_that("iteration works", {
  f <- makeFun( sin(a*x) ~ x, a=1)
  g <- makeFun( f(x^2, a) ~ x & a, a=1)
  expect_equivalent( g(3), f(3^2) )
  expect_equivalent( g(3, a=5), f(3^2, a=5) )
})

test_that('Errors are thrown', {
  expect_error( makeFun( ~ x ) )
  expect_error( makeFun( 3 ) )
  expect_error( makeFun( sin(x) ~ x & a, a=3, y=4 ) )
})

test_that('Argument list is correct',{
  f <- makeFun( a * sin(x) ~ x & a & y )
  expect_equivalent( names(formals(f)), c('x','a','y') )
  f <- makeFun( a * sin(x) ~ x & a & y, y=2, a=3 )
  expect_equivalent( names(formals(f)), c('x','a','y') )
})

test_that('Can make functions from models', {
  x <- 1:10
  ex <- exp(x)
  y <- c(1:5, 5:1) 
  model1 <- lm( y ~ x )
  model2 <- lm( y ~ log(ex) )
  f <- makeFun(model1)
  g <- makeFun(model2)
  expect_equivalent( f(7), 3 )
  expect_equivalent( g(7), 3 )
  expect_equivalent( names(formals(f)), c('x','...') )
  expect_equivalent( names(formals(g)), c('ex','...') )
})

test_that('Can make functions from models with no predictors', {
  model <- lm( age ~ 1, data=HELPrct )
  f <- makeFun(model)
  expect_equivalent( f(), mean(HELPrct$age) )
  expect_equivalent( f(7), mean(HELPrct$age) )
  expect_equivalent( f(x=7), mean(HELPrct$age) )
  expect_equivalent( f(sex='female'), mean(HELPrct$age) )
  expect_equivalent( f(1:3), rep(mean(HELPrct$age),3) )
  expect_equivalent( names(formals(f)), c('...') )
})

