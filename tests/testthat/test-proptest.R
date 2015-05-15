
context("prop.test()")

TestData <- data.frame( a = factor(rep(letters[1:3], length.out=100)),
                        b = rep(letters[1:3], length.out=100), 
                        c = rep(c(TRUE, FALSE, FALSE), length.out=100), 
                        stringsAsFactors = FALSE
)

test_that("formulas work", {
  
  X <- stats::prop.test(34, 100)
  A <- prop.test(~ a, data=TestData)
  B <- prop.test(~ b, data=TestData)
  C <- prop.test(~ c, data=TestData)
  
  expect_equivalent(interval(A), interval(X))
  expect_equivalent(A$data.name, "TestData$a")
  
  expect_equivalent(interval(B), interval(X))
  expect_equivalent(B$data.name, "TestData$b")
  
  expect_equivalent(interval(C), interval(X))
  expect_equivalent(C$data.name, "TestData$c")
  
})

test_that("formulas work with unnamed second arg", {
  
  X <- stats::prop.test(34, 100)
  A <- prop.test(~ a, TestData)
  B <- prop.test(~ b, TestData)
  C <- prop.test(~ c, TestData)
  
  expect_equivalent(interval(A), interval(X))
  expect_equivalent(A$data.name, "TestData$a")
  
  expect_equivalent(interval(B), interval(X))
  expect_equivalent(B$data.name, "TestData$b")
  
  expect_equivalent(interval(C), interval(X))
  expect_equivalent(C$data.name, "TestData$c")
})

test_that("success = works", {
  
  X <- stats::prop.test(33, 100)
  Y <- stats::prop.test(66, 100)
  A <- prop.test(~ a, data=TestData, success = "b")
  B <- prop.test(~ b, data=TestData, success = "b")
  C <- prop.test(~ c, data=TestData, success = FALSE)
  
  expect_equivalent(interval(A), interval(X))
  expect_equivalent(A$data.name, "TestData$a")
  
  expect_equivalent(interval(B), interval(X))
  expect_equivalent(B$data.name, "TestData$b")
  
  expect_equivalent(interval(C), interval(Y))
  expect_equivalent(C$data.name, "TestData$c")
})


test_that("bare vars work", {
  X <- stats::prop.test(34, 100)
  A <- prop.test( a, data=TestData)
  B <- prop.test( b, data=TestData)
  C <- prop.test( c, data=TestData)
  
  expect_equivalent(interval(A), interval(X))
  expect_equivalent(A$data.name, "a")
  
  expect_equivalent(interval(B), interval(X))
  expect_equivalent(B$data.name, "b")
  
  expect_equivalent(interval(C), interval(X))
  expect_equivalent(C$data.name, "c")
})

test_that("numbers work", {
  expect_equivalent( 
    interval(stats::prop.test(33, 100)),
    interval(prop.test(33, 100))
  )
  
})
