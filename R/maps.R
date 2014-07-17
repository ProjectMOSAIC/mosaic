
tryCatch(utils::globalVariables(c('coordinates')),
         error=function(e) message('Looks like you should update R.'))


#' Map Utilities
#'
#'Some utilities for working with map data
#'
#' @param map A map object of class \code{SpatialPolygonsDataFrame}
#' @param ... Other arguments, currently ignored
#' @return A dataframe, in which the first 7 columns hold geographical
#' information (ex: \code{long} and \code{lat})
#' @examples
#'
#' \dontrun{ 
#' if(require(maptools)) {
#'   data(wrld_simpl)
#'   worldmap <- sp2df(wrld_simpl)
#' }
#' 
#' if ( require(ggplot2) && require(maptools) ) { 
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
#' @export

sp2df <- function (map, ...) 
{
  .try_require(c("ggplot2", "maptools")) 
  map@data$id <- rownames(map@data)
  coords_matrix <- coordinates(map)  # in sp, which maptools depends on
  map@data$clon = coords_matrix[, 1]
  map@data$clat = coords_matrix[, 2]
  map_points <- do.call(fortify, list(map, region="id"), 
                        envir=environment(ggplot2::fortify))
  result <- merge(map_points, map@data, by = "id")
  return(result)
}
