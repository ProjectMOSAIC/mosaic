context('statTally()')

test_that("Two-sided test", {
  set.seed(1)
  rdata <- data.frame("null" = rnorm(1000))
  expect_message(capture.output(tmp <- statTally(1, rdata, alternative="two.sided")),
                 "*168 \\( 16.78 % \\) had test stats <= -1*")
  expect_message(capture.output(tmp <- statTally(1, rdata, alternative="two.sided")),
                 "*159 \\( 15.88 % \\) had test stats >= 1*")
})

test_that("One-sided tests", {
  set.seed(1)
  rdata <- data.frame("null" = rnorm(1000))
  expect_message(capture.output(tmp <- statTally(1, rdata, alternative="greater")),
                 "*159 \\( 15.88 % \\) had test stats >= 1*")
  expect_message(capture.output(tmp <- statTally(1, rdata, alternative="less")),
                 "*843 \\( 84.22 % \\) had test stats <= 1*")
  expect_message(capture.output(tmp <- statTally(-1, rdata, alternative="greater")),
                 "*833 \\( 83.22 % \\) had test stats >= -1*")
  expect_message(capture.output(tmp <- statTally(-1, rdata, alternative="less")),
                 "*169 \\( 16.88 % \\) had test stats <= -1*")
})