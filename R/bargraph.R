
bargraph <- function(x, data, ...) {
	if (inherits(x, "formula")) {
		barchart(with(data, xtabs(x)), data, ...)
	}else {
		barchart(x, data, ...)
	}
	
}
