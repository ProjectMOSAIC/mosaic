tryCatch(utils::globalVariables(c('coordinates',"Name","Code","long","lat")),
         error=function(e) message('Looks like you should update R.'))

## Files needed for the country and state functions:
#### US_States : a SpatialPolygonsDataFrame object for US states
#### US_States_df : fortify(US_States)
#### stateAlternatives : a vector for standardizing state names
#### World_Countries : a SpatialPolygonsDataFrame object for world countries
#### World_Countries_df : fortify(World_Countries)
#### countryAlternatives : a vector for standardizing country names
#### CIA: a dataframe specifying the possible datasets available for
####      download through the CIAdata() function

#' Standardization of Geographic Names
#' 
#' Often different sources of geographical data will use different names for
#' the same region.  These utilities make it easier to merge data from different
#' sources by converting names to standardized forms.
#' 
#' @details
#' \describe{
#' 
#' \item{standardName}{This is the most general standardizing function.
#' In addition to \code{x}, this function requires another argument:
#' \code{standard} - a named vector in which each name is a particular
#' spelling of the region name in question and the corresponding value
#' is the standardized version of that region name}
#'  
#' \item{standardCountry}{This function will standardize the country
#' names in \code{x} to the standard ISO_a3 country code format. If 
#' \code{returnAlternatives} is set to \code{TRUE}, this function will also
#' return the the named vector used to standardize the country names}
#' 
#' \item{standardState}{This function will standardize the US state
#' names in \code{x} to the standard two-letter abbreviations. If 
#' \code{returnAlternatives} is set to \code{TRUE}, this function will also
#' return the the named vector used to standardize the state names}
#' }
#' In all three cases, any names not found in \code{standard}
#' will be left unaltered.  Unless supressed, a warning message will
#' indicate the number of such cases, if there are any.
#' 
#' @param x A vector with the region names to standardize
#' @param standard a named vector providing the map from 
#' non-standard names (names of vector) to standard names (values of vector)
#' @param quiet a logical indicating whether warnings should be surpressed
#' @export
standardName <- function(x, standard, ignore.case=TRUE, returnAlternatives=FALSE, quiet=FALSE) {
  nn <- names(x)  
  if (ignore.case) {
    x <- toupper(x)
    names(standard) <- toupper(names(standard))
  }
  res <- standard[x]
  numNAs <- sum(is.na(res))
  res[ is.na(res) ] <- x [ is.na(res) ]
  names(res) <- nn
  if (! quiet && (numNAs > 0)) {
    warning(paste(numNAs, "items were not transalted"))
  }
  if (!returnAlternatives) return(res)
  list(standardized = stand, alternatives = countryAlternatives)
}

#' @export
#' @rdname standardName
#' @param returnAlternatives a logical indicating whether all alternatives should
#' be returned in addition to the standard name.
standardCountry <- function(x, ignore.case=TRUE, returnAlternatives = FALSE, 
                            quiet=FALSE) {
  standardName(x, countryAlternatives, 
               ignore.case = ignore.case, 
               returnAlternatives=returnAlternatives,
               quiet=quiet)
}

#' @export
#' @rdname standardName
standardState <- function(x, ignore.case=TRUE, 
                          returnAlternatives = FALSE, 
                          quiet=FALSE) {
  standardName(x, stateAlternatives, 
               ignore.case = ignore.case, 
               returnAlternatives=returnAlternatives, 
               quiet=quiet)
}

#' @export
fortify.SpatialPolygonsDataFrame <- function(model, data, region=NULL, ...) {
  if (!is.null(region)) stop("`region' not supported")  
  model@data$id <- rownames(model@data) 
  region <- "id"
  fmodel <- do.call(fortify, list(model, region="id"), 
                    envir=environment(ggplot2::fortify))
  merge(fmodel, model@data, by="id")
}

#' Make a map with \code{ggplot2}
#'
#' \code{makeMap} takes in two sources of data that refer to geographical
#' regions and merges them together. Depending on the arguments passed,
#' it returns this merged data or a ggplot object constructed with the data.
#'
#' @param data A dataframe with regions as cases
#' @param map An object that can be fortified to a dataframe (ex: a dataframe itself,
#' or a SpatialPolygonsDataFrame) 
#' @param key.data The column name in the \code{data} that holds the
#' unique names of each region
#' @param key.map The column name in the \code{map} that holds the
#' unique names of each region
#' @param key The combination of \code{key.data} and \code{key.map} 
#' @param tr.data A function of the transformation to be performed to
#' the \code{key.data} column
#' @param tr.map A function of the transformation to be performed to
#' the \code{key.map} column
#' @param plot The plot desired for the output. \code{plot} = "none"
#' returns the merged data that is the result of merging the \code{data}
#' and \code{map} together; \code{plot}="frame" returns an empty
#' (unplottable) ggplot object; \code{plot} = "border" (the default)
#' returns a ggplot object with one geom_polygon layer that shows the
#' borders of the regions.
#' @export 
makeMap <- function (data, map=NULL, key=c(key.data, key.map), 
                  key.data, key.map, tr.data = identity, tr.map = identity,
                  plot=c("borders", "frame", "none")) {
  message("Mapping API still under development and may change in future releases.")
  plot <- match.arg(plot)
  if (!is.null(map)) {
    map <- fortify(map)
    if (!("long" %in% names(map) && "lat" %in% names(map))) {
      stop("`map' does not appear to be a map")
    }
    key <- rep(key, out.length=2)
    if (!(key[1] %in% names(data))) {
      stop(paste(key[1], "not in names of `data'"))
    }
    if (!(key[2] %in% names(map))) {
      stop(paste(key[2], "not in names of `map'"))
    }
    data[[key[1]]] <- tr.data(data[[key[1]]])
    map[[key[2]]] <- tr.map(map[[key[2]]])
    data <- merge(data, map, by.x=key[1], by.y=key[2])
  }
  switch(plot, 
         borders = ggplot(data, aes(x=long, y=lat, group=group, order=order)) +
           geom_polygon(color="darkgray", fill=NA) + theme_map() +
           labs(x="", y=""),
         frame = ggplot(data, aes(x=long, y=lat, group=group, order=order)),
         none = data)
}


#' Make a world map with \code{ggplot2}
#'
#' \code{mWorldMap} takes in one dataframe that includes information
#' about different countries. It merges this dataframe with a dataframe
#' that includes geographical coordinate information. Depending on the
#' arguments passed, it returns this data or a ggplot object constructed
#' with the data.
#' 
#' @param data A dataframe with countries as cases
#' @param key The column name in the \code{data} that holds
#' the unique names of each country
#' @param fill A variable in the \code{data} used to specify the fill
#' color of countries in the map (note: if \code{fill} is not null, then
#' \code{plot} cannot be set to "none")
#' @param plot The plot desired for the output. \code{plot} = "none"
#' returns the merged data that is the result of merging the \code{data}
#' and the dataframe with the geographical coordinate information;
#' \code{plot} = "frame" returns an empty (unplottable) ggplot object;
#' \code{plot} = "border" (the default) returns a ggplot object with
#' one geom_polygon layer that shows the borders of the countries
#' 
#' @examples
#'\dontrun{
#' gdpData <- CIAdata("GDP")      # load some world data
#' 
#' mWorldMap(gdpData, key="country", fill="GDP")
#'
#' gdpData <- gdpData %>% mutate(GDP5 = ntiles(-GDP, 5, format="rank")) 
#' mWorldMap(gdpData, key="country", fill="GDP5")
#'
#' mWorldMap(gdpData, key="country", plot="frame") +
#' geom_point()
#' 
#' mergedData <- mWorldMap(gdpData, key="country", plot="none")
#' 
#' ggplot(mergedData, aes(x=long, y=lat, group=group, order=order)) +
#' geom_polygon(aes(fill=GDP5), color="gray70", size=.5) + guides(fill=FALSE)  
#' }
#' @export 
mWorldMap <- function(data, key, fill=NULL, plot=c("borders", "frame", "none")) {
  plot <- match.arg(plot)
  map <- makeMap(data=data, map=World_Countries_df, key=c(key, "iso_a3"), 
              tr.data=standardCountry, tr.map=toupper, plot=plot)
  if (plot != "none") { map <- map + coord_map() }
  if ( (!is.null(fill) && plot != "none") ) {
    map <- map + geom_polygon(aes_string(fill=fill), color="darkgray")
  }
  map
}


#' Make a US map with \code{ggplot2}
#'
#' \code{mUSMap} takes in one dataframe that includes information
#' about different US states. It merges this dataframe with a dataframe
#' that includes geographical coordinate information. Depending on the
#' arguments passed, it returns this data or a ggplot object constructed
#' with the data.
#'
#' @param data A dataframe with US states as cases
#' @param key The column name in the \code{data} that holds the unique
#' names of each state
#' @param fill A variable in the \code{data} used to specify the fill
#' color of states in the map (note: if \code{fill} is not null, then
#' \code{plot} cannot be set to "none")
#' @param plot The plot desired for the output. \code{plot} = "none"
#' returns the merged data that is the result of merging the \code{data}
#' and the dataframe with the geographical coordinate information;
#' \code{plot} = "frame" returns an empty (unplottable) ggplot object;
#' \code{plot} = "border" (the default) returns a ggplot object with
#' one geom_polygon layer that shows the borders of the states
#' @param style The style in which to display the map. \code{compact} gives 
#' a polyconic projection with Alaska and Hawaii on the lower left corner;
#' \code{real} gives the real size and position of all states without any
#' projection.
#' 
#' @examples
#' 
#' sAnscombe <- Anscombe %>% 
#'   group_by(state = rownames(Anscombe)) %>% 
#'   summarise(income = sum(income)) %>%
#'   mutate(state = standardName(state, c(IO = "IA", KA = "KS"), quiet=TRUE))
#' 
#' mUSMap(sAnscombe, key="state", fill="income")
#'
#' mUSMap(sAnscombe, key="state", plot="frame") +
#' geom_point()
#' 
#' mergedData <- mUSMap(sAnscombe, key="state", plot="none")
#' 
#' ggplot(mergedData, aes(x=long, y=lat, group=group, order=order)) +
#' geom_polygon(aes(fill=state), color="darkgray") + guides(fill=FALSE) 
#' @export 
mUSMap <- function(data, key, fill=NULL, 
                   plot=c("borders", "frame", "none"),
                   style=c("compact","real")) {
  plot <- match.arg(plot)
  style <- match.arg(style)
  if (style == "compact") {US_States_df <- US_States_comp_df}
  map <- makeMap(data=data, map=US_States_df, key=c(key, "STATE_ABBR"), 
              tr.data=standardState, tr.map=toupper, plot=plot)
  if ( (!is.null(fill) && plot != "none") ) {
    map <- map + geom_polygon(aes_string(fill=fill), color="darkgray")
  }
  map
}


#' Return a dataset based on the CIA World Factbook
#' 
#' This function can be used in two different ways. Without an argument, it returns a reference
#' table that includes information about all the CIA World Factbook tables that are available
#' through this function. Note the  \code{Name} column that indicates a unique name for each
#' available dataset. If this name is passed as an argument to the function, the function 
#' will return the corresponding dataset.
#' 
#' @param name An optional parameter specifying the name of the desired dataset
#' 
#' @examples
#' head(CIAdata())
#' gdpData <- CIAdata("pop")
#' nrow(gdpData)
#' 
#' mergedData <- merge(CIAdata("pop"), CIAdata("fert"), by="country")
#' head(mergedData)
#' @export
CIAdata <- function (name = NULL) {
  if (is.null(name)) return(CIA)  
  
  if (name %in% CIA$Name) {
    sub <- subset(CIA, Name == name)
  } else {
    if (name %in% CIA$Code) {
      sub <- subset(CIA, Code == name)
    } else {
      message("Unable to locate desired table.  See list of available tables.")
      return(CIA)
    }
  }
  code <- sub[["Code"]]
  url <- (paste0("https://www.cia.gov/library/publications/the-world-factbook/rankorder/rawdata_",
                 code, ".txt"))
  
  .try_require("RCurl")
  
  table <- read.delim(textConnection(getURL(url, ssl.verifypeer = FALSE)),
                      header = FALSE, stringsAsFactors = FALSE)
  table[, 1] <- NULL
  names(table) <- c("country", name)
  table[[2]] = as.numeric(gsub("[^.+[:digit:] ]", "",
                               table[[2]]))
  return(table)
}


#' Transforms a shapefile into a dataframe
#'
#' This function takes in a shapefile (formal class of
#' \code{SpatialPolygonsDataFrame}) and transforms it into a dataframe
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

#' @export
theme_map <- function (base_size=12) {
  theme_grey(base_size) %+replace%
    theme(
      axis.title = element_blank (),
      axis.text = element_blank (),
      panel.background = element_blank (),
      panel.grid = element_blank (),
      axis.ticks.length = unit (0,"cm"),
      axis.ticks.margin = unit (0.01,"cm"),
      panel.margin = unit (0,"lines"),
      plot.margin = unit(c(0,0,0,0),"lines"),
      complete = TRUE
    )    
}
