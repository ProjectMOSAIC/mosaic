#' mosaic
#'
#' Data sets and utilities from Project MOSAIC (mosaic-web.org) used to teach mathematics, 
#' statistics, computation and modeling.  Funded by the NSF, Project MOSAIC is a 
#' community of educators working to tie together aspects of quantitative work that students 
#' in science, technology, engineering and mathematics will need in their professional lives, 
#' but which are usually taught in isolation, if at all.
#'
#'
#' @name mosaic-package
#' @aliases mosaic mosaic-package
#' @docType package
#' @title mosaic: the Project MOSAIC package
#' @author Randall Pruim (\email{rpruim@@calvin.edu}), Daniel Kaplan (\email{kaplan@@macalester.edu}), Nicholas Horton (\email{nhorton@@smith.edu})
#' @references
#' \url{http://mosaic-web.org}
#'
#' @keywords package
#' @importFrom car deltaMethod
#' @importFrom utils head modifyList
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @importFrom ggdendro segment label dendro_data
#' @importFrom reshape2 melt acast dcast
#' @import methods
#' @importFrom MASS fitdistr fractions
#' @import dplyr
#' @importFrom grid grid.polyline grid.text grid.lines grid.points grid.rect grid.layout gpar
#' @import lattice
#' @import ggplot2
#' @import splines 
#' @import mosaicData
#' @importFrom grDevices col2rgb colorRampPalette gray heat.colors rgb topo.colors
#' @importFrom graphics hist plot stem
#' @importFrom stats TukeyHSD nls
#' @importFrom stats addmargins aggregate anova aov approxfun as.formula chisq.test coef
#' @importFrom stats complete.cases confint cutree dbeta dcauchy dchisq dexp dgamma dist dlnorm dnorm
#' @importFrom stats dpois dt dweibull formula lm loess loess.control lsfit model.frame model.matrix
#' @importFrom stats  model.response na.omit pnorm ppoints predict printCoefmat qnorm qt rbinom resid
#' @importFrom stats rmultinom rnorm runif setNames splinefun terms uniroot update vcov wilcox.test xtabs
#' @importFrom utils browseURL capture.output menu read.csv read.table str tail head
NA
