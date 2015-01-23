
context('dpqrdist')

test_that("dpqrdist works for normal dist", {
  expect_equivalent(
    dpqrdist("norm", "d", c(0,1,2)), 
    dnorm(c(0,1,2))
  )
  expect_equivalent(
    dpqrdist("norm", "d", c(0,1,2), mean=10, sd=2),
    dnorm(c(0,1,2), mean=10, sd=2) 
  )
  expect_equivalent(
    dpqrdist("norm", "p", c(0,1,2)), 
    pnorm(c(0,1,2))
  )
  expect_equivalent(
    dpqrdist("norm", "p", c(0,1,2), mean=10, sd=2),
    pnorm(c(0,1,2), mean=10, sd=2) 
  )
  expect_equivalent(
    dpqrdist("norm", "q", c(.1,.2,.3)),
    qnorm(c(.1,.2,.3))
  ) 
  expect_equivalent(
    dpqrdist("norm", "q", c(.1,.2,.3), mean=10, sd=2),
    qnorm(c(.1,.2,.3), mean=10, sd=2) 
  )    
})

test_that("dpqrdist works for t dist", {
  expect_equivalent(
    dpqrdist("t", "d", c(0,1,2), df=10), 
    dt(c(0,1,2), df=10)
  )
  expect_equivalent(
    dpqrdist("t", "p", c(0,1,2), df=10), 
    pt(c(0,1,2), df=10)
  )
  expect_equivalent(
    dpqrdist("t", "q", c(.1,.2,.3), df=10), 
    qt(c(.1,.2,.3), df=10)
  )
})