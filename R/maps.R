#' Map Utilities
#'
#'Some utilities for working with map data
#'
#' @param map a map object of class \code{SpatialPolygonsDataFrame}
#' @param ... other arguments, currently ignored
#' @export
#' @examples
#'
#' \dontrun{ 
#' if(require(maptools)) {
#'   data(wrld_simpl)
#'   worldmap <- sp2df(wrld_simpl)
#' }
#' 
#' if ( require(ggplot2) && require(maptools) && require(plyr) ) { 
#'   data(wrld_simpl)
#'   World <- sp2df(wrld_simpl)
#'   World2 <- merge(World, Countries, by.x="NAME", by.y="maptools", all.y=FALSE)
#'   Mdata <- merge(Alcohol, World2, by.x="country", by.y="gapminder", all.y=FALSE) 
#'   Mdata <- Mdata[order(Mdata$order),]
#'   qplot( x=long, y=lat, fill=ntiles(alcohol,5), 
#'          data=subset(Mdata, year==2008), group = group, 
#'                      geom="polygon")
#' }
#' }
sp2df <- function (map, ...) 
{
  .try_require("ggplot2")
  map@data$id <- rownames(map@data)
  coords_matrix <- coordinates(map)
  map@data$clon = coords_matrix[, 1]
  map@data$clat = coords_matrix[, 2]
  map_points <- fortify(map, region = "id")
  result <- merge(map_points, map@data, by = "id")
  return(result)
}
