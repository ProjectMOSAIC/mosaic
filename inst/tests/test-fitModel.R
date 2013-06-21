
context('Testing fitModel()')


test_that("A function is created", {
  expect_true(is.function(fitModel( width ~ A * length + B, data=KidsFeet)))
  expect_equivalent(names(formals(fitModel( width ~ A * length + B, data=KidsFeet))), 
                    c('length','...','transform'))
})

test_that("Function gives correct results", {
  formula <- width ~ A * length + B
  f <- fitModel( formula, data=KidsFeet )
  model <- nls(formula, data=KidsFeet, start=list(A=1, B=1)) 
  expect_equivalent( f(KidsFeet$length), fitted(model) )
})

