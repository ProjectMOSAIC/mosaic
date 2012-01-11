
.createMathFun = function(sexpr=NULL, ...) { # A utility used by newD, newAntiD, and plotFun
  # sexpr = substitute(expr)
    # reconstruct the function finput from expr
    if(is.character(sexpr)) { # passing a function, e.g. sin
      fcall = paste(sexpr, "(x)~x",sep="")
      expr = parse(text = fcall)
      sexpr = substitute(expr)
    }
    if (is.numeric(sexpr)) {
      fcall = paste(sexpr,"+0*x ~ x",sep="")
      expr = parse(text=fcall)
      sexpr = substitute(expr)
    }
    # is.name(sexpr) is handled below
    
  #sexpr comes from a substitute(expr) in the top-level interface function.
  vals = list(...)
  # if( !is.null(s)) vals[["s"]] = s  
  # The above line, and the use of s in the argument list are
  # a kluge so that "s" can be used in the expression.
  specialNames=NULL
  specialVals=NULL
  rightSide=NULL
  if (is.name(sexpr)) {
        # Strategy, turn it into something of the standard form and call
        # this function recursively.
        fargs = formals(eval(sexpr))
        # kill off anything named ..args
        fargs["..args"] = NULL
        if( length(fargs) == 0 ) fcall = paste(sexpr, "(x)~x",sep="")
        else {
          if( length(fargs) == 1 ) 
           fcall = paste(sexpr,"(",names(fargs),") ~ ", names(fargs))
          else {
            nms = c()
            for (k in 1:length(fargs)) {
              if( nchar(as.character(fargs[[k]]))>0 )
                nms = c(nms, names(fargs)[k])
            }
            fcall = paste( sexpr,"(", paste(nms,collapse=","),")~",
              paste(nms,collapse="+"))
          }
        }
        expr <- parse(text = fcall)
        sexpr = substitute(expr)
        return( .createMathFun(sexpr, ...) )
  }
  exprClass = tryCatch(class(eval(sexpr)), error=function(e){class(sexpr)})
  if (exprClass == "formula") {
    # Get the special names from the right side of the formula
    expr = eval(sexpr)
    sexpr = expr[[2]] # left side of formula
    specialNames = all.vars(expr[[3]]) # right side of formula
    rightSide = all.names(expr[[3]]) # for detecting possibly repeated names
    specialVals = NULL
  }
  else if (exprClass == "call") {
    nmvals = names(vals)
  # which ones are the limits?
    sz = mapply( length, vals )
    inds = which(sz==2)
    specialNames = nmvals[inds]
    specialVals = vals[inds]
    vals[specialNames] = NULL
  }
  
    # create the formal arguments of the function
    goo = c(list())
    # put the arguments in an "alist" so that they are unbound 
    one = list()
    two = list()
    if( length(specialNames) > 0) {
     tmp = paste( "alist( ", 
         paste(specialNames, "=",collapse=",",sep=""),")")
     one = eval(parse(text=tmp))    
    }
    tmp2 = all.vars(sexpr)
    tmp2 = setdiff(tmp2,specialNames)
    if( length(tmp2) > 0) {
     tmp = paste( "alist( ", 
         paste(tmp2, "=",collapse=",",sep=""),")")
     two = eval(parse(text=tmp))    
    }
    goo =c(one,two,goo)
    #goo[specialNames] = NA # put it first in the list
    #goo[all.vars(sexpr)] = NA
    goo[names(vals)] = vals 
    # EXCEPTIONS for 
    # global variables which are not accessible
    # if they are contained in the formals list
    goo["pi"] = NULL # let pi stand for pi
    
    # kill anything with a ..args argument
    goo["..args"] = NULL
    ff = function() {
      eval(sexpr, enclos=parent.frame())
    }
    formals(ff) = goo
    return(list(fun=ff,names=specialNames,
      vals=specialVals,others=names(vals),
      sexpr=sexpr,RHS=rightSide))
}
  
#==========

plotFun1 = function(expr, ..., add=FALSE,
  xlim=NULL,ylim=NULL,npts=NULL,
  ylab=NULL, xlab=NULL, zlab=NULL, main=NULL, 
  lwd=1,col="black",filled=TRUE,nlevels=10,
  surface=FALSE,
  colorscheme=topo.colors,type="l",transparency=NULL ) { 
    vals = list(...)
    ..currentAxisNames = mosaic.par.get("currentAxisNames")
    ..currentAxisLimitX= mosaic.par.get("currentAxisLimitX")
    ..currentAxisLimitY= mosaic.par.get("currentAxisLimitX")
    if ( is.null(..currentAxisNames) )  ..currentAxisNames= c("", "")
    if ( is.null(..currentAxisLimitX) ) ..currentAxisLimitX =  c(0,1)
    if ( is.null(..currentAxisLimitY) ) ..currentAxisLimitY =  c(0,1)
    
    ..f.. = .createMathFun( sexpr=substitute(expr), ...)
    vars = formals(..f..$fun)
    if (length(..f..$names) == 0 ) {
      if( ..currentAxisNames[1] == "" )
        stop("No plotting variable defined")
      else ..f..$names = ..currentAxisNames[ ..currentAxisNames != ""]
    }
    ndims = length(..f..$names)
    if( ndims == 1 ){
      npts = ifelse( is.null(npts), 200, npts)
      # create a function of that one variable
      #   the "bogus" arg is to make things work with .adapt_seq
      pfun = function(.x){
        vals[[..f..$names]] = .x
        # PUT AN assign HERE
        # assign(..f..$names, .x)
        eval( ..f..$sexpr, envir=vals, enclos=parent.frame())
      }
      # since plot will handle expressions nicely, let it do so.
      # but note that zlab in 3-d plots doesn't handle expressions
      # so it's necessary to deparse things there.
      if( length(ylab) == 0 ) ylab = ..f..$sexpr #deparse(..f..$sexpr)
      if( length(xlab) == 0 ) xlab = ..f..$names
      # figure out the limits.  
      # Is a limit specified, either through xlim or the variable name
      xlim2 = xlim
      if( ..f..$names %in% names(vals) ) {
          xlim2 = vals[[..f..$names]]
      }
      if( length(xlim2)<2 ) { # no limits were specified
        if( ..f..$names != ..currentAxisNames[1] )
          stop(paste("Dependent variable in add-on plot, ",
           ..f..$names, ", does not match existing plot variable ", 
           ..currentAxisNames[1], sep=""))
        else xlim2 = ..currentAxisLimitX
           
      } 
      if( (length( xlim2) < 2) & (length( xlim ) <2 ) ) {
          stop(paste("Must provide x-axis limit via ", 
            ..f..$names, "= or xlim=", sep=""))
      }
      # Evaluate the function.
      if( require(mosaic) )
        .xset = mosaic:::.adapt_seq(min(xlim2), max(xlim2), 
             f=function(xxqq,bogus){pfun(xxqq)}, length=npts)
      else
        .xset = seq(min(xlim2),max(xlim2),length=npts)
      
      .yset = pfun(.xset)
      if( length(.yset) != length(.xset) ){
        .yset == rep(0, length(.xset)) 
        for (k in 1:length(.xset) ) {
          .yset[k] = pfun(.xset[k]) # NULL for compatibility with .adapt_seq
        }
      }
      if (add) {
        # Check to make sure the new plot will show up
        if( all(.yset > max(..currentAxisLimitY)) |
            all(.yset < min(..currentAxisLimitY)) )
            warning("New values are outside of the y-axis range.")
         
        graphics::lines(.xset, .yset, lwd=lwd, col=col)      }
      else { # draw a new plot
        ..currentAxisLimitX = xlim2
        ..currentAxisNames = c(..f..$names, "")
        graphics::plot( .xset, .yset, type=type, 
         lwd=lwd, col=col, xlim=xlim, ylim=ylim,
         xlab=xlab,ylab=ylab,main=main)
        goo = par("usr") # get the limits of the plot
        ..currentAxisLimitY = goo[c(3,4)]
        mosaic.par.set(currentAxisLimitX = ..currentAxisLimitX )
        mosaic.par.set(currentAxisLimitY = ..currentAxisLimitY )
      }  
    }
    if (ndims == 2 ) {
      npts = ifelse( is.null(npts), 40, npts)
      # create a function of those two variables
      pfun = function(.x,.y){
        vals[[..f..$names[1]]] = .x
        vals[[..f..$names[2]]] = .y
        eval( ..f..$sexpr, envir=vals, enclos=parent.frame())
      }
      if( length(ylab) == 0 ) ylab = ..f..$names[2]
      if( length(xlab) == 0 ) xlab = ..f..$names[1]
      if( length(zlab) == 0 ) zlab = deparse(..f..$sexpr)
      xlim2 = xlim
      ylim2 = ylim
      if( ..f..$names[1] %in% names(vals) ) {
          xlim2 = vals[[..f..$names[1]]]
      }
      if( ..f..$names[2] %in% names(vals) ) {
        ylim2 = vals[[..f..$names[2]]]
      }
      if (add  | length(xlim2)==0 | length(ylim2) == 0) {
        xlim2 = ..currentAxisLimitX
        ylim2 = ..currentAxisLimitY
        add = TRUE
        if( !all(..f..$names == ..currentAxisNames) ){
          stop(paste("Dependent variables in add-on plot, ",
           ..f..$names[1], " and ", 
           ..f..$names[2], ", do not match existing plotting variable ", 
           ..currentAxisNames[1], " and ", ..currentAxisNames[2], sep=""))
        }
      }
     
        
      if( (length( xlim2) < 2) & (length( xlim ) <2 ) ) {
          stop(paste("Must provide x-axis limit via ", 
            ..f..$names, "= or xlim=", sep=""))
      }
        
      .xset = seq(min(xlim2),max(xlim2),length=npts)
      .yset = seq(min(ylim2),max(ylim2),length=npts)
  #   zvals = matrix( rep(0,length(.xset)*length(.yset)),nrow=length(.xset))
  #   for (k in 1:length(.xset)) {
  #    for (j in 1:length(.yset)) {
  #     zvals[k,j] = pfun( .xset[k], .yset[j] )
  #    }
  #   }
     
     if( !add ){
       ..currentAxisLimitX=c(min(xlim2),max(xlim2))
       ..currentAxisLimitY=c(min(ylim2),max(ylim2))
     }
     
     zvals = outer(.xset, .yset, function(x,y){pfun(x,y)} )
     if( surface ) {
       grid = expand.grid( .xset, .yset )
       grid$height = c(zvals)
       if( require(manipulate) ) {
         manipulate(print(wireframe(height ~ Var1 + Var2, xlab=xlab,ylab=ylab,zlab=zlab,data=grid,drape=filled,
           shade=TRUE,screen=c(x=-90,y=rot,z=0),col=rgb(1,1,1,0))), 
          rot = slider(-180,180,step=5,initial=45,label="Rotation"))
           
       } 
       else {
         print(wireframe(height ~ Var1 + Var2, xlab=xlab,ylab=ylab,zlab=zlab,data=grid,drape=filled,shade=TRUE,
           col=rgb(1,1,1,0)) )
       }
       ..currentAxisNames = ..f..$names
       
       # No ADD method yet for surface plots
     }
     else {
      if( add & is.null(transparency) ) transparency=.4
      if( is.null(transparency) ) transparency=1
      fillcolors = colorscheme(20,alpha=transparency)
      if( is.logical(zvals[1,1]) ){ # it's a constraint function
        if( add ) fillcolors= c(rgb(0,0,0,transparency),rgb(0,0,0,0))
        else fillcolors = colorscheme(2)
        nlevels=2
      }
      if( filled) {
       graphics::image( .xset, .yset, zvals, col=fillcolors,add=add,
         xlab=xlab,ylab=ylab,main=main )
       graphics::contour(.xset, .yset, zvals, col=col,lwd=lwd,add=TRUE, nlevels=nlevels)
      }
      else {
       graphics::contour(.xset,.yset,zvals,nlevels=nlevels,add=add,lwd=lwd,col=col,
         xlab=xlab,ylab=ylab,main=main)
      } 
     }
     ..currentAxisNames = ..f..$names
    }
    else if( ndims > 2 ) 
      stop("More than 2 plotting variables.")

    mosaic.par.set(currentAxisNames = ..currentAxisNames)
    mosaic.par.set(currentAxisLimitX = ..currentAxisLimitX)
    mosaic.par.set(currentAxisLimitY = ..currentAxisLimitY)

    invisible(..f..$fun)
  }
# =============================

plotFun <- function(expr, ..., add=FALSE,
                   xlim=NULL,ylim=NULL,npts=NULL,
                   ylab=NULL, xlab=NULL, zlab=NULL, main=NULL, 
                   lwd=1,col="black",filled=TRUE,nlevels=10,
                   surface=FALSE,
                   colorscheme=topo.colors,type="l",transparency=NULL ) { 
  dots <- list(...)
  
  ..f.. <- .createMathFun( sexpr=substitute(expr), ...)

  vars <- formals(..f..$fun)
#  if (length(..f..$names) == 0 ) {
#    if( ..currentAxisNames[1] == "" )
#      stop("No plotting variable defined")
#    else ..f..$names <- ..currentAxisNames[ ..currentAxisNames != ""]
#  }

  ndims <- length(..f..$names)
  if( ndims == 1 ){
    npts <- ifelse( is.null(npts), 200, npts)
    # create a function of that one variable
    pfun <- function(.x){
	  mydots <- dots
      mydots[[..f..$names]] <- .x
      eval( ..f..$sexpr, envir=mydots, enclos=parent.frame())
    }

    # since plot will handle expressions nicely, let it do so.
    # but note that zlab in 3-d plots doesn't handle expressions
    # so it's necessary to deparse things there.
    if( is.null(ylab) ) ylab <- expr(..f..$sexpr) #deparse(..f..$sexpr)
    if( is.null(xlab) ) xlab <- ..f..$names

    # figure out the limits.  
    # Is a limit specified, either through xlim or the variable name
    xlim2 <- xlim
    if( ..f..$names %in% names(dots) ) {
      xlim2 <- dots[[..f..$names]]
    }

    if( length(xlim2) < 2 ) { # no limits were specified
      if( FALSE && ..f..$names != ..currentAxisNames[1] )
        stop(paste("Dependent variable in add-on plot, ",
                   ..f..$names, ", does not match existing plot variable ", 
                   ..currentAxisNames[1], sep=""))
      else xlim2 <- c(0,1)   # ..currentAxisLimitX
    } 

    if( (length( xlim2) < 2) & (length( xlim ) <2 ) ) {
      stop(paste("Must provide x-axis limit via ", 
                 ..f..$names, "= or xlim=", sep=""))
    }
    # Evaluate the function.
	#if( require(mosaic) ) {
		.xset <- mosaic:::.adapt_seq(min(xlim2), max(xlim2), 
									 f=function(xxqq){ pfun(xxqq) }, length=npts)
        #dots[[..f..$names]] <- .x
    #} else {
	#	.xset <- seq(min(xlim2),max(xlim2),length=npts)
	#}

	.yset <- pfun(.xset)
	if( length(.yset) != length(.xset) ){
		.yset == rep(0, length(.xset)) 
		for (k in 1:length(.xset) ) {
			.yset[k] <- pfun(.xset[k]) 
			dots[[..f..$names]] <- .xset[k]
		}
	}

    if (add) { # add to existing plot using ladd()

      # Check to make sure the new plot will show up
      #if( all(.yset > max(..currentAxisLimitY)) |
      #  all(.yset < min(..currentAxisLimitY)) )
      #  warning("New values are outside of the y-axis range.")
      
      ladd(panel.lines(.xset, .yset, lwd=lwd, col=col, ...))       
	} else { # draw a new plot
      # ..currentAxisLimitX <- xlim2
      # ..currentAxisNames <- c(..f..$names, "")
      thePlot <- lattice::xyplot(.yset ~ .xset, type=type,
                      lwd=lwd, col=col, 
					  xlim=xlim, ylim=ylim,
                      xlab=xlab,ylab=ylab,
					  main=main)
      # goo <- par("usr") # get the limits of the plot
      # ..currentAxisLimitY <- goo[c(3,4)]
      # mosaic.par.set(currentAxisLimitX = ..currentAxisLimitX )
      # mosaic.par.set(currentAxisLimitY = ..currentAxisLimitY )
      return(thePlot)
    }  
  }
  if (ndims == 2 ) {
    npts <- ifelse( is.null(npts), 40, npts)
    # create a function of those two variables
    pfun <- function(.x,.y){
      dots[[..f..$names[1]]] <- .x
      dots[[..f..$names[2]]] <- .y
      eval( ..f..$sexpr, envir=dots, enclos=parent.frame())
    }
    if( length(ylab) == 0 ) ylab <- ..f..$names[2]
    if( length(xlab) == 0 ) xlab <- ..f..$names[1]
    if( length(zlab) == 0 ) zlab <- deparse(..f..$sexpr)
    xlim2 <- xlim
    ylim2 <- ylim
    if( ..f..$names[1] %in% names(dots) ) {
      xlim2 <- dots[[..f..$names[1]]]
    }
    if( ..f..$names[2] %in% names(dots) ) {
      ylim2 <- dots[[..f..$names[2]]]
    }
    if (add  | length(xlim2)==0 | length(ylim2) == 0) {
      xlim2 <- ..currentAxisLimitX
      ylim2 <- ..currentAxisLimitY
      add <- TRUE
      if( !all(..f..$names == ..currentAxisNames) ){
        stop(paste("Dependent variables in add-on plot, ",
                   ..f..$names[1], " and ", 
                   ..f..$names[2], ", do not match existing plotting variable ", 
                   ..currentAxisNames[1], " and ", ..currentAxisNames[2], sep=""))
      }
    }
    
    
    if( (length( xlim2) < 2) & (length( xlim ) <2 ) ) {
      stop(paste("Must provide x-axis limit via ", 
                 ..f..$names, "= or xlim=", sep=""))
    }
    
    .xset <- seq(min(xlim2),max(xlim2),length=npts)
    .yset <- seq(min(ylim2),max(ylim2),length=npts)
    #   zvals <- matrix( rep(0,length(.xset)*length(.yset)),nrow=length(.xset))
    #   for (k in 1:length(.xset)) {
    #    for (j in 1:length(.yset)) {
    #     zvals[k,j] <- pfun( .xset[k], .yset[j] )
    #    }
    #   }
    
    if( !add ){
      ..currentAxisLimitX<-c(min(xlim2),max(xlim2))
      ..currentAxisLimitY<-c(min(ylim2),max(ylim2))
    }
    
    zvals <- outer(.xset, .yset, function(x,y){pfun(x,y)} )
    if( surface ) {
      grid <- expand.grid( .xset, .yset )
      grid$height <- c(zvals)
      if( require(manipulate) ) {
        manipulate(print(wireframe(height ~ Var1 + Var2, xlab=xlab,ylab=ylab,zlab=zlab,data=grid,drape=filled,
                                   shade=TRUE,screen=c(x=-90,y=rot,z=0),col=rgb(1,1,1,0))), 
                   rot <- slider(-180,180,step=5,initial=45,label="Rotation"))
        
      } 
      else {
        print(wireframe(height ~ Var1 + Var2, xlab=xlab,ylab=ylab,zlab=zlab,data=grid,drape=filled,shade=TRUE,
                        col=rgb(1,1,1,0)) )
      }
      ..currentAxisNames <- ..f..$names
      
      # No ADD method yet for surface plots
    }
    else {
      if( add & is.null(transparency) ) transparency<-.4
      if( is.null(transparency) ) transparency<-1
      fillcolors <- colorscheme(20,alpha=transparency)
      if( is.logical(zvals[1,1]) ){ # it's a constraint function
        if( add ) fillcolors<- c(rgb(0,0,0,transparency),rgb(0,0,0,0))
        else fillcolors <- colorscheme(2)
        nlevels<-2
      }
      if( filled) {
        graphics::image( .xset, .yset, zvals, col=fillcolors,add=add,
                         xlab=xlab,ylab=ylab,main=main )
        graphics::contour(.xset, .yset, zvals, col=col,lwd=lwd,add=TRUE, nlevels=nlevels)
      }
      else {
        graphics::contour(.xset,.yset,zvals,nlevels=nlevels,add=add,lwd=lwd,col=col,
                          xlab=xlab,ylab=ylab,main=main)
      } 
    }
    ..currentAxisNames <- ..f..$names
  }
  else if( ndims > 2 ) 
    stop("More than 2 plotting variables.")
  
  mosaic.par.set(currentAxisNames <- ..currentAxisNames)
  mosaic.par.set(currentAxisLimitX <- ..currentAxisLimitX)
  mosaic.par.set(currentAxisLimitY <- ..currentAxisLimitY)
  
  invisible(..f..$fun)
}
# =============================
