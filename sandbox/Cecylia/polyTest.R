context('Testing simplification and use in calculus.R such as .polyExp()')


test_that('polynomial simplifier works', {
  coeffs <- .polyExp(lhs(3*(x+1)^6+x^6-x^2+3*8~x), "x", "")
  
  expect_that(coeffs$pow, equals(6))
  
  #Sees whether polynomial matches the given function
  checkPoly <- function(formula){
    set.seed(123)
    
    oldpoly <- makeFun(formula)
    .x. <- all.vars(rhs(formula))
    params <- setdiff(all.vars(formula), .x.)
    if(length(params)==0)
      params=""

    p <- suppressWarnings(.polyExp(lhs(formula), .x., params))
    co <- p$coeffs
    po <- p$pow

    newpoly <- oldpoly
    body(newpoly) <- parse(text = paste("(", co, ")", rep("*", po+1), rep(.x., po+1), rep("^", po+1), (po:0), sep="", collapse="+"))
    
    for(i in 1:20){
      vars = runif(length(all.vars(formula)), min=-10, max=10)
      expect_that(do.call(oldpoly, as.list(vars)), equals(do.call(newpoly, as.list(vars)), tol=.00001))
    }
  }
  
  checkPoly(3*(x+1)^6+x^6-x^2+3*8~x)
  checkPoly(a*x~x)
  checkPoly(a*x^2*(x+8)^3+a^4*x+10~x)
  checkPoly(3~x)
  checkPoly(a*(b+x)^7~x)
  checkPoly(a*b*c*(x+9)^2~x)
  checkPoly((a+b)^6*x^2+a*x-8~x)
  
})