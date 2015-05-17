
context('dpqrdist()')

test_that("dpqrdist works for normal dist", {
  expect_equivalent(
    dpqrdist("norm", "d", x = c(0,1,2)), 
    dnorm(c(0,1,2))
  )
  expect_equivalent(
    dpqrdist("norm", "d", x = c(0,1,2), mean=10, sd=2),
    dnorm(c(0,1,2), mean=10, sd=2) 
  )
  expect_equivalent(
    dpqrdist("norm", "p", q = c(0,1,2)), 
    pnorm(c(0,1,2))
  )
  expect_equivalent(
    dpqrdist("norm", "p", q = c(0,1,2), mean=10, sd=2),
    pnorm(c(0,1,2), mean=10, sd=2) 
  )
  expect_equivalent(
    dpqrdist("norm", "q", p = c(.1,.2,.3)),
    qnorm(c(.1,.2,.3))
  ) 
  expect_equivalent(
    dpqrdist("norm", "q", p = c(.1,.2,.3), mean=10, sd=2),
    qnorm(c(.1,.2,.3), mean=10, sd=2) 
  )    
})

test_that("dpqrdist works for t dist", {
   expect_equivalent(
     dpqrdist("t", "d", x = c(0,1,2), df=10), 
     dt(c(0,1,2), df=10)
   )
  expect_equivalent(
    dpqrdist("t", "p", q = c(0,1,2), df=10), 
    pt(c(0,1,2), df=10)
  )
  expect_equivalent(
    dpqrdist("t", "q", p = c(.1,.2,.3), df=10), 
    qt(c(.1,.2,.3), df=10)
  )
})

test_that("dpqrdist works for binomial dist", {
  expect_equivalent(
    dpqrdist("binom", "d", x = c(0,1,2), size=10, prob=0.4), 
    dbinom(c(0,1,2), size=10, prob=0.4)
  )
  expect_equivalent(
    dpqrdist("binom", "p", q = c(0,1,2), size=10, prob=0.4), 
    pbinom(c(0,1,2), size=10, prob=0.4)
  )
  expect_equivalent(
    dpqrdist("binom", "q", p = c(.25, .5, .75), size=10, prob=0.4), 
    qbinom(c(.25, .5, .75), size=10, prob=0.4)
  )
})
