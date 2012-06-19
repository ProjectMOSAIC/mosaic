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

test_that('Can make functions from lm models', {
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

test_that('Can make functions from glm models (gaussian)', {
  x <- 1:10
  ex <- exp(x)
  y <- c(1:5, 5:1) 
  model1 <- glm( y ~ x, family='gaussian' )
  model2 <- glm( y ~ log(ex), family='gaussian' )
  f <- makeFun(model1)
  g <- makeFun(model2)
  expect_equivalent( f(7), 3 )
  expect_equivalent( g(7), 3 )
  expect_equivalent( names(formals(f)), c('x','...') )
  expect_equivalent( names(formals(g)), c('ex','...') )
})

test_that('Can make functions from glm models (Gamma)', {
  # example from documentation for glm
  clotting <- data.frame(
  u = c(5,10,15,20,30,40,60,80,100),
  lot1 = c(118,58,42,35,27,25,21,19,18),
  lot2 = c(69,35,26,21,18,16,13,12,12))
  f <- makeFun(glm(lot1 ~ log(u), data=clotting, family=Gamma))
  g <- makeFun(glm(lot2 ~ log(u), data=clotting, family=Gamma))
  # expect_equivalent( abs( f(10) - 53.26389 ) < 0.001)
  # expect_true( abs( g(10) - 32.86152 ) < 0.001)
})


test_that('Can make functions from nls models', {
  # example from documentation for nlm
  x <- -(1:100)/10
  y <- 100 + 10 * exp(x / 2) + rnorm(x)/10
  nlmod <- nls(y ~  Const + A * exp(B * x), start=list(Const=1,A=1,B=1))
  f <- makeFun(nlmod)
  expect_equivalent( f(0), sum(coef(nlmod)[1:2]) )
  k <- coef(nlmod)[1]
  A <- coef(nlmod)[2]
  B <- coef(nlmod)[3]
  expect_equivalent( f(1), k + A * exp(B) )
})

test_that('coef works for makeFun functions', {
  x <- 1:10
  y <- c(1:5, 5:1) 
  lmmod <- lm( y ~ x )
  f <- makeFun(lmmod)
  expect_equivalent( coef(lmmod), coef(f) )
  #
  clotting <- data.frame(
  u = c(5,10,15,20,30,40,60,80,100),
  lot1 = c(118,58,42,35,27,25,21,19,18),
  lot2 = c(69,35,26,21,18,16,13,12,12))
  glmod <- glm(lot1 ~ log(u), data=clotting, family=Gamma)
  f <- makeFun(glmod)
  expect_equivalent( coef(glmod), coef(f) )
  #
  x <- -(1:100)/10
  y <- 100 + 10 * exp(x / 2) + rnorm(x)/10
  nlmod <- nls(y ~  Const + A * exp(B * x), start=list(Const=1,A=1,B=1))
  f <- makeFun(nlmod)
  expect_equivalent( coef(nlmod), coef(f) )
})
