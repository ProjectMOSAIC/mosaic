
## @knitr setup
#setCacheDir("cache")
require(grDevices); require(datasets); require(stats); require(lattice)
require(grid); require(mosaic); require(fastR)
trellis.par.set(theme=col.mosaic(bw=FALSE))
trellis.par.set(fontsize=list(text=9))
options(keep.blank.line=FALSE); options(width=100)
densityplot <- function(...) { print(lattice::densityplot(...)) }
xyplot <- function(...) { print(lattice::xyplot(...)) }
bwplot <- function(...) { print(lattice::bwplot(...)) }
histogram <- function(...) { print(lattice::histogram(...)) }
barchart <- function(...) { print(lattice::barchart(...)) }
require(vcd)
mosaic <- function(...) { print(vcd::mosaic(...)) }


