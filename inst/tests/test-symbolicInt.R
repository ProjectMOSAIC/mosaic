context('Testing symbolic Integration.')

test_that('Simple polynomial expressions work'){
  ff = symbolicInt(2*x^6+9*x^3+2~x)
}