
## @knitr setup
#setCacheDir("cache")
require(grDevices) 
require(datasets) 
require(stats) 
require(lattice)
require(grid) 
require(mosaic) 
require(fastR)
trellis.par.set(theme=col.mosaic(bw=FALSE))
trellis.par.set(fontsize=list(text=9))
options(keep.blank.line=FALSE) 
options(width=70)
require(vcd)
require(knitr)
knit_hooks$set(chunk=function(x,options){
	return(paste("%% Using custom chunk hook function", 
		  "\\begin{knitrout}",
	      "\\definecolor{shadecolor}{rgb}{0.9, 0.9, 0.9}\\color{fgcolor}",
		  "\\begin{kframe}",
		  x,
         "\\end{kframe}",
		 "\\end{knitrout}",
		 sep="\n"
		 ))
})

knit_hooks$set(output=function(x,options) {
			   vshift <- options$vshift
               if (is.null(vshift)) vshift <- "0ex"
			   return(paste("%% Using custom output hook function", 
					 paste("\\vspace*{",vshift,"}", sep=""),
					 "\\begin{verbatim}",
                      x, 
					 "\\end{verbatim}",
					  sep="\n" ))
})
