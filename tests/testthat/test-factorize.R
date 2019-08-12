testthat::test_that("factorize works", {
  data(KidsFeet, package="mosaicData")
  factorize(KidsFeet$birthyear) %>% dput()
  
  testcase <- structure(c(2L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 
                          2L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 
                          2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), 
                         .Label = c("87", "88"), 
                         class = "factor")
  
  expect_equivalent(testcase, factorize(KidsFeet$birthyear))
  # alternative spelling
  expect_equivalent(testcase, factorise(KidsFeet$birthyear))
})