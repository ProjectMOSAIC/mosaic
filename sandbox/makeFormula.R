followTilde = function(sexpr) {
  exprClass = tryCatch(class(eval(sexpr)), error=function(e){class(sexpr)})
  if (exprClass =="formula") return(eval(sexpr))
  else { # Handling non-formulas
    # Convert to a formula by putting a ~ on the left hand side
    return(as.formula(paste("~",deparse(sexpr))))
    }
}