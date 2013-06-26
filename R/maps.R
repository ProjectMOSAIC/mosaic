#' Map Utilities
#'
#'Some utilities for working with map data
#'
#' @param map a map object of class code{SpatialPolygonsDataFrame}
#' @param ... other arguments, currently ignored
#' @export
#' @examples
#' 
#' if(require(maptools)) {
#'   data(wrld_simpl)
#'   worldmap <- sp2df(wrld_simpl)
#' }
#' 
sp2df <- function(map,...) {
  .try_require("ggplot2")   # needed for fortify()
  
  map@data$id <- rownames(map@data)
  
  coords_matrix <- coordinates(map)
  map@data$clon = coords_matrix[,1] 
  map@data$clat = coords_matrix[,2]
  
  map_points <- fortify( map, region="id" )
  result <- merge(map_points, map@data, by="id")
  return(result)
}

# worldmap <- readOGR(dsn="/Users/rpruim/Downloads/TM_WORLD_BORDERS_SIMPL-0.2", 
#                    layer="TM_WORLD_BORDERS_SIMPL-0.2")
#
# world.df <- sp2df(worldmap)
# world.df2 <- sp2df(wrld_simpl)
#

