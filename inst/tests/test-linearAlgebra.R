context('Linear Algebra')


test_that("dot works for vectors", {
  x <- c(1,2,3)
  u <- c(1,1,1)
  expect_equivalent( dot(x,u), 6 )
})

test_that("project works for vectors", {
  x <- c(1,2,3)
  u <- c(1,1,1)
  n <- 10
  y <- rnorm(n)
  one <- rep(1,n)
  expect_equivalent( project(x, c(1,0,0)), c(1,0,0) ) 
  expect_equivalent( project(x, c(0,1,0)), c(0,2,0) )
  expect_equivalent( project(x, c(0,0,1)), c(0,0,3) )
  expect_equivalent( project(x, u),  c(2,2,2) ) 
  expect_equivalent( project(y, one), rep(mean(y), n) )
})

test_that("1 vector works", {
  x <- c(1,2,3)
  u <- c(1,1,1)
  y <- rnorm(10)
  expect_equivalent( project(x,1), c(2,2,2) )
  expect_equivalent( project(y,1),  rep(mean(y),length(y)) ) 
})
