context('Testing symbolic Integration.')

test_that('Simple polynomial expressions work',{
  ff = symbolicInt(2*x^6+9*x^3+2~x)
  gg = function(x)2*1/7*x^7+9*1/4*x^4+2*x
  expect_that(ff(seq(1,10, len=30)), equals(gg(seq(1,10, len=30)), tol=0.00001))
  
  ff = symbolicInt((2+3)*x^5+(2+9)*x^2+(8*6+2)*x~x)
  gg = function(x){(2+3)*1/(6)*x^6+(2+9)*1/(3)*x^3+(8*6+2)*1/(2)*x^2}
  expect_that(ff(seq(1,10, len=28)), equals(gg(seq(1,10, len=28)), tol=0.00001))
})

test_that('Declared variables do not interfere with constants',{
  a=5
  ff = symbolicInt((a+3)*x^7-a*4*x+a~x)
  gg = function(x,a){(a+3)*1/(8)*x^8-a*4*1/(2)*x^2+a*x}
  expect_that(ff(seq(1,10, len=26), a=2), equals(gg(seq(1,10, len=26), a=2), tol=0.00001))
})

test_that('Negative exponents on polynomials work',{
  ff = symbolicInt(3*x^-4+x^-2-x^-3~x)
  gg = function(x){3*1/(-3)*x^-3+1/(-1)*x^-1-1/(-2)*x^-2}
  expect_that(ff(seq(1,10, len=28)), equals(gg(seq(1,10, len=28)), tol=0.00001))
  
  ff = symbolicInt(3/x~x)
  gg = function(x){3*log(x)}
  expect_that(ff(seq(1,10, len=28)), equals(gg(seq(1,10, len=28)), tol=0.00001))
})

test_that('Simple trigonometric functions are working',{
  ff = symbolicInt(27*sin(3*x)~x)
  gg = function(x){-9*cos(3*x)}
  expect_that(ff(seq(1,10, len=28)), equals(gg(seq(1,10, len=28)), tol=0.00001))
  
  ff = symbolicInt(-sin(x)~x)
  gg = function(x){cos(x)}
  expect_that(ff(seq(1,10, len=28)), equals(gg(seq(1,10, len=28)), tol=0.00001))
  
})

test_that('These are tests that do not run correctly yet',{
  #symbolicInt(3*(2*x)^2~x)
  #symbolicInt(1/(x+1)~x)
  #symbolicInt((x+1)^-1~x)
  
  #symbolicInt(3*sin(3*x+1)~x)
})