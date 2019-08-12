testthat::test_that("expandFun works", {
  
  testcase <- list(formula = (z)^2 ~ z, formals = as.pairlist(alist(x = )))
  
  expect_equivalent(testcase, expandFun(makeFun(x^2~x)(z)~z))
})

