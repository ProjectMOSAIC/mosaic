#First there are a list of patterns
#
#Each pattern is in the form of a list, specifying a desired parse tree
#elements of the parse tree can be:
#   Math expressions
#   Arithmetic expressions
#   Affine expressions
#   Square of an affine expression
#   Quadratic expression
#   
#
#Examples
#The generic formula for something will integrate into arctan is:
# A/(B+C*(D*x+E)^2) and this will integrate into
# A*(1/D)*(1/sqrt(C))*(1/sqrt(B))*arctan(D*sqrt(C)*x/sqrt(B))
# ^^ double check ^^

pattern1 <- list(One='cos', Two="affine")


for(ptrn in patternList){
  
}

#Notes to change this: be able to identify 3*x+x as an affine expression as well.
#This is really important:
# > .affine.exp(lhs(3*x^2+2~x), 'x^2')
# $a
# [1] 3
# 
# $b
# 0 + 2

#'Takes a call and returns its affine coefficients.
#'
#'@param tree A call that will be parse
#'@param .x. the variable name
#'
#'@return A list with values of a and b satisfying a*.x.+b  = tree.
#'If the expression is not affine, returns an empty list.
#'
.affine.exp <- function(tree, .x.){
  #if it is a simple expression
  if(tree==.x.){
    a=1
    b=0
    return(list(a=a,b=b))
  }
  
  #if there is no variable in the expression
  if(length(grep(toString(.x.), deparse(tree), fixed=TRUE))==0){
    a=0
    b=tree
    return(list(a=a,b=b))
  }
  
  #if the expression is more complex
  if(tree[[1]]=='('){
    return(Recall(tree[[2]], .x.))
  }
  
  if(tree[[1]]=='+'){
    lexp=Recall(tree[[2]], .x.)
    rexp=Recall(tree[[3]], .x.)
    
    if(lexp$a==0 && length(rexp)!=0){
      a = rexp$a
      b = parse(text=paste(deparse(lexp$b), "+", deparse(rexp$b),sep=""))[[1]]
      return(list(a=a,b=b))
    }
    
    if(rexp$a==0 && length(lexp)!=0){
      a = lexp$a
      b = parse(text=paste(deparse(lexp$b), "+", deparse(rexp$b),sep=""))[[1]]
      return(list(a=a,b=b))
    }
  }
  
  if(tree[[1]]=='-'){
    lexp=Recall(tree[[2]], .x.)
    rexp=Recall(tree[[3]], .x.)
    
    if(lexp$a==0 && length(rexp)!=0){
      a = parse(text=paste("-1*(",deparse(lexp$a), ")",sep=""))[[1]]
      b = parse(text=paste(deparse(lexp$b), "-(", deparse(rexp$b),")",sep=""))[[1]]
      return(list(a=a,b=b))
    }
    
    if(rexp$a==0 && length(lexp)!=0){
      a = lexp$a
      b = parse(text=paste(deparse(lexp$b), "-(", deparse(rexp$b),")",sep=""))[[1]]
      return(list(a=a,b=b))
    }
  }
  
  if(tree[[1]]=='*'){
    lexp=Recall(tree[[2]], .x.)
    rexp=Recall(tree[[3]], .x.)
    
    if(lexp$a==0 && length(rexp)!=0){
      a = parse(text=paste("(",deparse(lexp$b), ")*(", deparse(rexp$a),")",sep=""))[[1]]
      b = parse(text=paste("(",deparse(lexp$b), ")*(", deparse(rexp$b),")",sep=""))[[1]]
      if(lexp$b==0){
        a = 0
        b = 0
      }
      if(lexp$b==1){
        a = rexp$a
        b = rexp$b
      }
      if(rexp$a ==0){
        a = 0
      }
      if(rexp$b == 0){
        b = 0
      }
      if(rexp$a == 1){
        a = lexp$b
      }
      if(rexp$b == 1){
        b = lexp$b
      }
      return(list(a=a,b=b))
    }
    
    if(rexp$a==0 && length(lexp)!=0){
      a = parse(text=paste("(",deparse(lexp$a), ")*(", deparse(rexp$b),")",sep=""))[[1]]
      b = parse(text=paste("(",deparse(lexp$b), ")*(", deparse(rexp$b),")",sep=""))[[1]]
      
      if(rexp$b==0){
        a = 0
        b = 0
      }
      if(rexp$b==1){
        a = lexp$a
        b = lexp$b
      }
      if(lexp$a==0){
        a = 0
      }
      if(lexp$b==0){
        b = 0
      }
      if(lexp$a==1){
        a = rexp$b
      }
      if(lexp$b==1){
        b = rexp$b
      }
      return(list(a=a,b=b))
    }
  }
  
  if(tree[[1]]=='/'){
    lexp=Recall(tree[[2]], .x.)
    rexp=Recall(tree[[3]], .x.)
    
    if(rexp$a==0 && length(lexp)!=0){
      a = parse(text=paste(deparse(lexp$a), "/(", deparse(rexp$b),")",sep=""))[[1]]
      b = parse(text=paste(deparse(lexp$b), "/(", deparse(rexp$b),")",sep=""))[[1]]
      
      if(rexp$b==1){
        a = lexp$a
        b = lexp$b
      }
      
      return(list(a = a, b = b))
    }
  }
  return(list())
}

#'Takes a call and returns its quadratic coefficients
#'
#'@param tree A call that will be parse
#'@param .x. the variable name
#'
#'@return A list with values of a, b, c, ... satisfying a*.x.^2+b*.x.+c  = tree. The last value in the list, pow, indicates the highest power
#'of the expression.
#'If the expression is not a polynomial, returns an empty list.
#'
.polynomial.expression <- function(tree, .x.){
  highestPow = 0
  #if it is a simple expression
  if(tree==.x.){
    a=1
    b=0
    return(list(a=a,b=b, pow=1))
  }
  
  #if there is no variable in the expression
  if(length(grep(toString(.x.), deparse(tree), fixed=TRUE))==0){
    a=tree
    return(list(a=a,pow=0))
  }
  
  #if the expression is more complex
  if(tree[[1]]=='('){
    return(Recall(tree[[2]], .x.))
  }
  
  if(tree[[1]]=='+'){
    lexp=Recall(tree[[2]], .x.)
    rexp=Recall(tree[[3]], .x.)
    
    if(lexp$a==0 && length(rexp)!=0){
      a = rexp$a
      b = parse(text=paste(deparse(lexp$b), "+", deparse(rexp$b),sep=""))[[1]]
      return(list(a=a,b=b))
    }
    
    if(rexp$a==0 && length(lexp)!=0){
      a = lexp$a
      b = parse(text=paste(deparse(lexp$b), "+", deparse(rexp$b),sep=""))[[1]]
      return(list(a=a,b=b))
    }
  }
  
  if(tree[[1]]=='-'){
    lexp=Recall(tree[[2]], .x.)
    rexp=Recall(tree[[3]], .x.)
    
    if(lexp$a==0 && length(rexp)!=0){
      a = parse(text=paste("-1*(",deparse(lexp$a), ")",sep=""))[[1]]
      b = parse(text=paste(deparse(lexp$b), "-(", deparse(rexp$b),")",sep=""))[[1]]
      return(list(a=a,b=b))
    }
    
    if(rexp$a==0 && length(lexp)!=0){
      a = lexp$a
      b = parse(text=paste(deparse(lexp$b), "-(", deparse(rexp$b),")",sep=""))[[1]]
      return(list(a=a,b=b))
    }
  }
  
  if(tree[[1]]=='*'){
    lexp=Recall(tree[[2]], .x.)
    rexp=Recall(tree[[3]], .x.)
    
    if(lexp$a==0 && length(rexp)!=0){
      a = parse(text=paste("(",deparse(lexp$b), ")*(", deparse(rexp$a),")",sep=""))[[1]]
      b = parse(text=paste("(",deparse(lexp$b), ")*(", deparse(rexp$b),")",sep=""))[[1]]
      if(lexp$b==0){
        a = 0
        b = 0
      }
      if(lexp$b==1){
        a = rexp$a
        b = rexp$b
      }
      if(rexp$a ==0){
        a = 0
      }
      if(rexp$b == 0){
        b = 0
      }
      if(rexp$a == 1){
        a = lexp$b
      }
      if(rexp$b == 1){
        b = lexp$b
      }
      return(list(a=a,b=b))
    }
    
    if(rexp$a==0 && length(lexp)!=0){
      a = parse(text=paste("(",deparse(lexp$a), ")*(", deparse(rexp$b),")",sep=""))[[1]]
      b = parse(text=paste("(",deparse(lexp$b), ")*(", deparse(rexp$b),")",sep=""))[[1]]
      
      if(rexp$b==0){
        a = 0
        b = 0
      }
      if(rexp$b==1){
        a = lexp$a
        b = lexp$b
      }
      if(lexp$a==0){
        a = 0
      }
      if(lexp$b==0){
        b = 0
      }
      if(lexp$a==1){
        a = rexp$b
      }
      if(lexp$b==1){
        b = rexp$b
      }
      return(list(a=a,b=b))
    }
  }
  
  if(tree[[1]]=='/'){
    lexp=Recall(tree[[2]], .x.)
    rexp=Recall(tree[[3]], .x.)
    
    if(rexp$a==0 && length(lexp)!=0){
      a = parse(text=paste(deparse(lexp$a), "/(", deparse(rexp$b),")",sep=""))[[1]]
      b = parse(text=paste(deparse(lexp$b), "/(", deparse(rexp$b),")",sep=""))[[1]]
      
      if(rexp$b==1){
        a = lexp$a
        b = lexp$b
      }
      
      return(list(a = a, b = b))
    }
  }
  return(list())
}