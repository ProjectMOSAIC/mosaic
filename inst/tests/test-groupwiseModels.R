context("Groupwise Models")

test_that("basic calculations occur properly", {
  mod <- mm( width ~ 1, data=KidsFeet)
  expect_that(coef(mod), is_equivalent_to(mean(KidsFeet$width)))
  mod2 <- mm( width ~ sex, data=KidsFeet)
  mod3 <- lm( width ~ sex-1, data=KidsFeet)
  expect_that( coef(mod2)[1], is_equivalent_to(coef(mod3)[1]))
})

test_that("coefficient names are correct",{
  mod <- mm( width ~ 1, data=KidsFeet)
  expect_that( names(coef(mod)), equals("all"))
  mod <- mm( width ~ sex, data=KidsFeet)
  expect_true( all( names(coef(mod)) %in% c("sexB","sexG")))
  mod <- mm( width ~ sex&domhand, data=KidsFeet)
  expect_true( all( names(coef(mod)) %in% c("sexB:domhandR","sexB:domhandL","sexG:domhandR","sexG:domhandL")))
})

test_that("fitted and resids have the right relationship",{
  mod <- mm( width ~ sex&domhand, data=KidsFeet)
  expect_that( resid(mod)+fitted(mod), equals( KidsFeet$width, tol=0.000001))
})

test_that("mm works with do",{
  s <- do(10)*mm(width~sex&domhand, data=resample(KidsFeet))
  expect_that( nrow(s), equals(10))
  expect_that( ncol(s), equals(6))
})

