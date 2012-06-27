context('Testing symbolic Integration.')

test_that('Simple polynomial expressions work',{
  ff = symbolicInt(2*x^6+9*x^3+2~x)
  gg = function(x)2*1/7*x^7+9*1/4*x^4+2*x
  expect_that(ff(seq(1,10, len=30)), equals(gg(seq(1,10, len=30)), tol=0.00001))
  
  ff = symbolicInt((2+3)*x^5+(2+9)*x^2+(8*6+2)*x~x)
  gg = function(x){(2+3)*1/(6)*x^6+(2+9)*1/(3)*x^3+(8*6+2)*1/(2)*x^2}
  expect_that(ff(seq(1,10, len=28)), equals(gg(seq(1,10, len=28)), tol=0.00001))
})