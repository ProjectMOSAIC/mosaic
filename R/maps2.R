## Files needed for the country and state functions:
#### US_States : a SpatialPolygonsDataFrame object for US states
#### stateAlternatives : a vector for standardizing state names
#### World_Countries : a SpatialPolygonsDataFrame object for world countries
#### countryAlternatives : a vector for standardizing country names
#### CIA: a dataframe specifying the possible datasets available for
####      download through the CIAdata() function

#' Standardization of Geographic Names
#' 
#' Often different sources of geographical data will use different names for
#' the same region.  These utilities make it easier to merge data from different
#' sources by converting names to standardized forms.
#' 
#' @param x A vector with the names to standardize
#' @param standard A named vector in which each name is a particular spelling of
#' the name in question and the value is the standardized version of that name 
#' @export
standardName <- function(x, standard) {
  standard[toupper(x)]
}

#' @param x A vector with the country names to standardize
#' @return A vector with standardized country names (in the ISO-a3 format)
#' @export
#' @rdname standardName
standardCountry <- function(x) {
  standardName(x, countryAlternatives)
}

#' @param x A vector with the US state names to standardize
#' @return A vector with standardized state names (as two-letter abbreviations)
#' @export
#' @rdname standardName
standardState <- function(x) {
  standardName(x, stateAlternatives)
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
           geom_polygon(color="darkgray", fill=NA) + theme_minimal() + coord_map() +
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
#' \dontrun{
#' gdpData <- CIAdata("GDP")      # load some world data
#' 
#' mWorldMap(gdpData, key="country", fill="GDP")
#'
#' mWorldMap(gdpData, key="country", plot="frame") +
#' geom_point()
#' 
#' mergedData <- mWorldMap(gdpData, key="country", plot="none")
#' 
#' ggplot(mergedData, aes(x=long, y=lat, group=group, order=order)) +
#' geom_polygon(aes(fill=country), color="darkgray") + guides(fill=FALSE)  
#' }
#' @export 
mWorldMap <- function(data, key, fill=NULL, plot=c("borders", "frame", "none")) {
  plot <- match.arg(plot)
  map <- mMap(data=data, map=World_Countries, key=c(key, "iso_a3"), 
              tr.data=standardCountry, tr.map=toupper, plot=plot)
  if (!(is.null(fill) && plot== "none")) {
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
#' 
#' @examples
#' \dontrun{
#' require(car)
#' require(dplyr)
#' 
#' sAnscombe <- Anscombe %>% group_by(state = rownames(Anscombe)) %>% 
#' summarise(income = sum(income))       # get some data grouped by state
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
#' }
#' @export 
mUSMap <- function(data, key, fill=NULL, plot=c("borders", "frame", "none")) {
  plot <- match.arg(plot)
  map <- mMap(data=data, map=US_States, key=c(key, "STATE_ABBR"), 
              tr.data=standardState, tr.map=toupper, plot=plot)
  if (!(is.null(fill) && plot== "none")) {
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
    if (name %in% CIA$code) {
      sub <- subset(CIA, Code == name)
    } else {
      message("Unable to locate desired table.  See list of available tables.")
      return(CIA)
    }
  }
  code <- sub[["Code"]]
  url <- (paste0("https://www.cia.gov/library/publications/the-world-factbook/rankorder/rawdata_",
                 code, ".txt"))
  table <- read.delim(textConnection(getURL(url, ssl.verifypeer = FALSE)),
                      header = FALSE, stringsAsFactors = FALSE)
  table[, 1] <- NULL
  names(table) <- c("country", name)
  table[[2]] = as.numeric(gsub("[^.+[:digit:] ]", "",
                               table[[2]]))
  return(table)
}
