#' Lattice Theme
#' 
#' A theme for use with lattice graphics.
#'
#' @param bw whether color scheme should be "black and white"
#' @param lty vector of line type codes
#' @param ... additional named arguments passed to 
#'   \code{\link{trellis.par.set}}
#' 
#' @return Returns a list that can be supplied as the \code{theme} to 
#' \code{\link{trellis.par.set}()}.
#' @note
#'   These two functions are identical.  \code{col.mosaic} is named 
#' similarly to \code{\link[lattice]{col.whitebg}}, but since more 
#' than just colors are set, \code{theme.mosaic} is a preferable name.
#'
#'
#' @seealso \code{\link{trellis.par.set}}, \code{\link{show.settings}} 
#' 
#' @rdname themes
#'
#' @examples
#' trellis.par.set(theme=theme.mosaic())
#' show.settings()
#' trellis.par.set(theme=theme.mosaic(bw=TRUE))
#' show.settings()
#' 
#' @keywords graphics 
#' @export

theme.mosaic <-
  function (bw = FALSE, lty = if (bw) 1:7 else 1, ...) 
  {
    aBlue <- colorRampPalette(c("white", "navy"))(10)[8]
    paleBlue <- colorRampPalette(c("white", "navy"))(10)[6]
    lightBlue <- colorRampPalette(c("white", "steelblue"))(10)[5]
    veryLightBlue <- colorRampPalette(c("white", "steelblue"))(12)[3]
    darkBlue <- colorRampPalette(c("white", "navy"))(10)[9]
    paleGreen <- colorRampPalette(c("white", "darkGreen"))(10)[8]
    if (bw) {
      res <- list(background = list(col = "transparent"), axis.line = list(col = "gray30"), 
                  axis.text = list(col = "gray30"), plot.polygon = list(col = "gray80"), 
                  box.rectangle = 
                    list(col = "gray10"), box.umbrella = list(col = "gray10", 
                                                              lty = 1), box.dot = list(col = "gray10"), dot.line = list(col = "gray50"), 
                  dot.symbol = 
                    list(col = "gray30", pch = 16), plot.line = list(col = "black", 
                                                                     lwd = 2), plot.symbol = list(col = "black", fill = "gray80", 
                                                                                                  pch = 16), regions = list(col = gray((1:100)/100)), 
                  reference.line = 
                    list(col = "gray50"), add.line = list(lty = 1,  col = "gray80", lwd = 2), 
                  superpose.polygon = 
                    list(col = c("gray30", "gray70", "black", "gray50", "gray20", "gray80", 
                                 "gray60", "gray40"), fill = c("gray80")), 
                  superpose.line = 
                    list(lty = lty, lwd = 2, 
                         col = c("gray30", "gray70", "black", 
                                 "gray50", "gray20", "gray80", "gray60", "gray40")), 
                  superpose.symbol = 
                    list(pch = c(16, 15, 18, 1, 3, 6, 0, 5), 
                         cex = rep(0.7, 7), 
                         col = c("gray30","gray70", "black", "gray50", "gray20", "gray80", 
                                 "gray60", "gray40")),
                  par.strip.text = list(cex = 0.5, col = c("gray60", "gray30")) 
      )
    } else {
      res <- list(background = list(col = "transparent"), 
                  plot.polygon = list(col = paleBlue), 
                  superpose.polygon = 
                    list(
                    col = c(aBlue, 
                            "lightskyblue3", "darkgreen", "tan", "orange", 
                            "purple", "lightgreen")), 
                  box.rectangle = 
                    list(col = darkBlue), box.umbrella = list(col = darkBlue), 
                  dot.line = list(col = "#e8e8e8"), 
                  dot.symbol = 
                    list(col = darkBlue, pch = 16), 
                  plot.line = 
                    list(lwd = 2, col = darkBlue), 
                  plot.symbol = 
                    list(col = darkBlue, pch = 16), 
                  regions = list(col = heat.colors(100)), 
                  reference.line =  list(col = "#e8e8e8"), 
                  add.line = list(lty = 1, col = "gray20", lwd = 2), 
                  superpose.line = 
                    list(lty = lty, 
                         col = c(darkBlue, "lightskyblue3", "darkgreen",  
                                 "tan", "orange", "purple", "pink", "lightgreen")), 
                  superpose.symbol = 
                    list(pch = c(16, 15, 18, 1, 3,  6, 0, 5), 
                         cex = rep(0.7, 7), 
                         col = c(darkBlue, "lightskyblue3", "darkgreen", "tan", "orange", 
                                                  "purple", "pink", "lightgreen")), 
                  strip.background = 
                    list(alpha = 1, 
                         col = c("#ffe5cc", veryLightBlue, "#ccffff", 
                                 "#cce6ff", "#ffccff", "#ffcccc", "#ffffcc")), 
                  strip.shingle = 
                    list(alpha = 1, 
                         col = c("#ff7f00", 
                                 darkBlue, "#00ffff", "#0080ff", "#ff00ff", "#ff0000", 
                                 "#ffff00")), 
                  par.strip.text = list(cex = 0.5)
      )
    }
    res <- c(res, list(...))
    res
  }

#' @rdname themes
#' @export

col.mosaic <- theme.mosaic

