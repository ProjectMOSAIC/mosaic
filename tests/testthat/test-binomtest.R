
context("binom.test()")

  TestData <- data.frame( a = factor(rep(letters[1:3], length.out=100)),
                          b = rep(letters[1:3], length.out=100), 
                          c = rep(c(TRUE, FALSE, FALSE), length.out=100), 
                          stringsAsFactors = FALSE
  )
  
test_that("formulas work", {
 
  X <- stats::binom.test(34, 100)
  A <- binom.test(~ a, data=TestData)
  B <- binom.test(~ b, data=TestData)
  C <- binom.test(~ c, data=TestData)
  
  expect_equivalent(interval(A), interval(X))
  expect_match(A$data.name, "TestData\\$a")
  
  expect_equivalent(interval(B), interval(X))
  expect_match(B$data.name, "TestData\\$b")
  
  expect_equivalent(interval(C), interval(X))
  expect_match(C$data.name, "TestData\\$c")
  
})

test_that("formulas work with unnamed second arg", {
 
  X <- stats::binom.test(34, 100)
  A <- binom.test(~ a, TestData)
  B <- binom.test(~ b, TestData)
  C <- binom.test(~ c, TestData)
  
  expect_equivalent(interval(A), interval(X))
  expect_match(A$data.name, "TestData\\$a")
  
  expect_equivalent(interval(B), interval(X))
  expect_match(B$data.name, "TestData\\$b")
  
  expect_equivalent(interval(C), interval(X))
  expect_match(C$data.name, "TestData\\$c")
})
  
test_that("success = works", {
  
  X <- stats::binom.test(33, 100)
  Y <- stats::binom.test(66, 100)
  A <- binom.test(~ a, data=TestData, success = "b")
  B <- binom.test(~ b, data=TestData, success = "b")
  C <- binom.test(~ c, data=TestData, success = FALSE)
  
  expect_equivalent(interval(A), interval(X))
  expect_match(A$data.name, "TestData\\$a")
  expect_match(A$data.name, "success = b")
  
  expect_equivalent(interval(B), interval(X))
  expect_match(B$data.name, "TestData\\$b")
  expect_match(B$data.name, "success = b")
  
  expect_equivalent(interval(C), interval(Y))
  expect_match(C$data.name, "TestData\\$c")
  expect_match(C$data.name, "success = FALSE")
})


test_that("bare vars work", {
  X <- stats::binom.test(34, 100)
  A <- binom.test( a, data=TestData)
  B <- binom.test( b, data=TestData)
  C <- binom.test( c, data=TestData)
  
  expect_equivalent(interval(A), interval(X))
  expect_match(A$data.name, "a")
  expect_match(A$data.name, "success = a")
  
  expect_equivalent(interval(B), interval(X))
  expect_match(B$data.name, "b")
  expect_match(B$data.name, "success = a")
  
  expect_equivalent(interval(C), interval(X))
  expect_match(C$data.name, "c")
  expect_match(C$data.name, "success = TRUE")
})

test_that("numbers work", {
  expect_equivalent( 
    interval(stats::binom.test(33, 100)),
    interval(binom.test(33, 100))
  )
  
})
  
  test_that("x treated as raw data when n is missing", {
    X <- resample(1:3, 100)
    x <- sum(X == min(X))
    expect_equivalent(  
      interval(binom.test(X)), 
      interval(binom.test(x, 100)) )
  })  
  
test_that("CI methods correct", {

  # Clopper-Pearson, the default but with 3 names
  
  expect_equivalent(
    interval(stats::binom.test(26,200)),
    interval(binom.test(26,200)))
  
  expect_equivalent(
    interval(stats::binom.test(26,200)),
    interval(binom.test(26,200, ci.method="clopper-pearson")))
  
  expect_equivalent(
    interval(binom.test(26,200, ci.method="clopper-pearson")),
    interval(binom.test(26,200, ci.method="binom.test")))
 
  # Score/Wilson/prop.test  
  expect_equivalent(
    interval(stats::prop.test(26,200)),
    interval(binom.test(26,200, ci.method="wilson")))
  
  expect_equivalent(
    interval(binom.test(26,200, ci.method="score")),
    interval(binom.test(26,200, ci.method="wilson")))
  
  expect_equivalent(
    interval(binom.test(26,200, ci.method="score")),
    interval(binom.test(26,200, ci.method="prop.test")))
 
  # Clopper vs external reference 
  # NIST example from http://www.itl.nist.gov/div898/handbook/prc/section2/prc241.htm
  ci <- interval(binom.test(4, 20, ci.method="clopper", conf.level=.9))
  expect_equal(as.vector(ci[2:3]), c(0.071354, 0.401029), tolerance = 1e-5)

  # Wald vs external reference 
  # from http://www.stat.wmich.edu/s160/book/node47.html 
  ci <- interval(binom.test(15, 59, ci.method="Wald"))
  expect_equal(as.vector(ci[2:3]), c(.143, .365), tolerance = 1e-3)
 
  # Plus4 
  ci <- interval(binom.test(0, 100, ci.method="Plus4"))
  expect_equal(as.vector(ci[2:3]), c(0.0, .0456), tolerance = 1e-3)
  
})
  