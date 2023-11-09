# context("Testing panicAttack")

test_that("nothing happens when nothing should",{
  conflicts <- panicAttack() 
  expect_that(length(conflicts),equals(0))
})

test_that("masked base functions are identified",{
  assign("c",function(x)x, envir=.GlobalEnv)
  conflicts <- panicAttack()
  expect_identical(conflicts,"c")
  panicAttack(fix=TRUE)
  conflicts <- panicAttack()
  expect_that(length(conflicts),equals(0))
})

