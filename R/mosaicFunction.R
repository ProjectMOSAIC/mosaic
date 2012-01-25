
setClass('mosaicFunction', 
	representation(
		fun = "function",
		names = "character",
		vals = "character",
		others = "character",
		sexpr = "call",
		rhs = "character"
		)
)

setMethod('show', 'mosaicFunction',
	function(object) {
		cat("A mosaic function... \n")
		print(object@fun)
		cat("\n  ** names:"); print( object@names)
		cat("\n  ** vals:"); print( object@vals)
		cat("\n  ** others:"); print( object@others)
		cat("\n  ** sexpr:"); print( object@sexpr)
		cat("\n  ** rhs:"); print( object@rhs)
	}
)
