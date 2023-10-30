utils::globalVariables(c('coordinates',"Name","Code","long","lat"))

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
#' In addition to `x`, this function requires another argument:
#' `standard` - a named vector in which each name is a particular
#' spelling of the region name in question and the corresponding value
#' is the standardized version of that region name}
#'  
#' \item{standardCountry}{This function will standardize the country
#' names in `x` to the standard ISO_a3 country code format. If 
#' `returnAlternatives` is set to `TRUE`, this function will also
#' return the the named vector used to standardize the country names}
#' 
#' \item{standardState}{This function will standardize the US state
#' names in `x` to the standard two-letter abbreviations. If 
#' `returnAlternatives` is set to `TRUE`, this function will also
#' return the the named vector used to standardize the state names}
#' }
#' In all three cases, any names not found in `standard`
#' will be left unaltered.  Unless supressed, a warning message will
#' indicate the number of such cases, if there are any.
#' 
#' @param x A vector with the region names to standardize
#' @param standard a named vector providing the map from 
#' non-standard names (names of vector) to standard names (values of vector)
#' @param ignore.case a logical indicating whether case should be ignored 
#' when matching.
#' @param quiet a logical indicating whether warnings should be suppressed
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
    warning(paste(numNAs, "items were not translated"))
  }
  if (!returnAlternatives) return(res)
  list(standardized = res, alternatives = countryAlternatives)
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

#' Make a map with `ggplot2`
#'
#' `makeMap` takes in two sources of data that refer to geographical
#' regions and merges them together. Depending on the arguments passed,
#' it returns this merged data or a ggplot object constructed with the data.
#'
#' @param data A dataframe with regions as cases
#' @param map An object that can be fortified to a dataframe (ex: a dataframe itself,
#' or a SpatialPolygonsDataFrame) 
#' @param key.data The column name in the `data` that holds the
#' unique names of each region
#' @param key.map The column name in the `map` that holds the
#' unique names of each region
#' @param key The combination of `key.data` and `key.map` 
#' @param tr.data A function of the transformation to be performed to
#' the `key.data` column
#' @param tr.map A function of the transformation to be performed to
#' the `key.map` column
#' @param plot The plot desired for the output. `plot` = "none"
#' returns the merged data that is the result of merging the `data`
#' and `map` together; `plot`="frame" returns an empty
#' (unplottable) ggplot object; `plot` = "border" (the default)
#' returns a ggplot object with one geom_polygon layer that shows the
#' borders of the regions.
#' @export 
makeMap <- function (data = NULL, map=NULL, key=c(key.data, key.map), 
                  key.data, key.map, tr.data = identity, tr.map = identity,
                  plot=c("borders", "frame", "none")) {
  message("Mapping API still under development and may change in future releases.")
  plot <- match.arg(plot)
  if (is.null(map) && is.null(data)) {
    stop("At least one of `data' and `map' must be specified.")
  }
  
  # If map is given, make sure it looks like a map
  if (!is.null(map)) {
    map <- fortify(map)
    if (! "long" %in% names(map)) {
      if ("lon" %in% names(map)) map$long <- map$lon
    } 
    
    for (n in c("lat", "long", "order", "group")) {
      if (!(n %in% names(map))) {
        stop(paste0("`map' does not appear to be a map (missing ", n, ")"))
      }
    }
  }
 
  # If we have both data and map, merge them 
  if (!is.null(data) && !is.null(map)) {
    key <- rep(key, out.length=2)
    if (!(key[1] %in% names(data))) {
      stop(paste(key[1], "not in names of `data'"))
    }
    if (!(key[2] %in% names(map))) {
      stop(paste(key[2], "not in names of `map'"))
    }
    map[[key[2]]] <- tr.map(map[[key[2]]])
    data[[key[1]]] <- tr.data(data[[key[1]]])
    data <- merge(data, map, by.x=key[1], by.y=key[2])
  }
  
  # make sure data is ready to go
  if (is.null(data)) data <- map
  
  for (n in c("lat", "long", "group", "order")) {
    if (! n %in% names(data)) {
      stop(paste0("`data' does not appear to be properly formatted (missing ", n, ")"))
    }
  }
  
  switch(plot, 
         borders = 
           ggplot(data |> group_by(group) |> arrange(order) |> ungroup(), 
                  aes(x=long, y=lat, group=group)) +
           geom_polygon(color="darkgray", fill=NA) + theme_map() +
           labs(x="", y=""),
         frame = 
           ggplot(data |> group_by(group) |> arrange(order) |> ungroup(), 
                  aes(x=long, y=lat, group=group)),
         none = 
           data)
}


#' Make a world map with `ggplot2`
#'
#' `mWorldMap` takes in one dataframe that includes information
#' about different countries. It merges this dataframe with a dataframe
#' that includes geographical coordinate information. Depending on the
#' arguments passed, it returns this data or a ggplot object constructed
#' with the data.
#' 
#' @param data A dataframe with countries as cases
#' @param key The column name in the `data` that holds
#' the unique names of each country
#' @param fill A variable in the `data` used to specify the fill
#' color of countries in the map (note: if `fill` is not null, then
#' `plot` cannot be set to "none")
#' @param plot The plot desired for the output. `plot` = "none"
#' returns the merged data that is the result of merging the `data`
#' and the dataframe with the geographical coordinate information;
#' `plot` = "frame" returns an empty (unplottable) ggplot object;
#' `plot` = "border" (the default) returns a ggplot object with
#' one geom_polygon layer that shows the borders of the countries
#' 
#' @examples
#'\dontrun{
#' gdpData <- CIAdata("GDP")      # load some world data
#' 
#' mWorldMap(gdpData, key="country", fill="GDP")
#'
#' gdpData <- gdpData |> mutate(GDP5 = ntiles(-GDP, 5, format="rank")) 
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
mWorldMap <- function(data = NULL, key = NA, fill = NULL, plot = c("borders", "frame", "none")) {
  plot <- match.arg(plot)
  map <- makeMap(data = data, map = World_Countries_df, key = c(key, "iso_a3"), 
              tr.data = standardCountry, tr.map = toupper, plot = plot)
  if (plot != "none") { 
    # deleting coord_map() for now for sake of CRAN checks (not understood) 2014-08-20
    map <- map # + coord_map()   
  }
  if ( (!is.null(fill) && plot != "none") ) {
    map <- map + geom_polygon(aes(fill = {{ fill }}), color = "darkgray")
  }
  map
}


#' Make a US map with `ggplot2`
#'
#' `mUSMap` takes in one dataframe that includes information
#' about different US states. It merges this dataframe with a dataframe
#' that includes geographical coordinate information. Depending on the
#' arguments passed, it returns this data or a ggplot object constructed
#' with the data.
#'
#' @param data A dataframe with US states as cases
#' @param key The column name in the `data` that holds the unique
#' names of each state
#' @param fill A variable in the `data` used to specify the fill
#' color of states in the map (note: if `fill` is not null, then
#' `plot` cannot be set to "none")
#' @param plot The plot desired for the output. `plot` = "none"
#' returns the merged data that is the result of merging the `data`
#' and the dataframe with the geographical coordinate information;
#' `plot` = "frame" returns an empty (unplottable) ggplot object;
#' `plot` = "border" (the default) returns a ggplot object with
#' one geom_polygon layer that shows the borders of the states
#' @param style The style in which to display the map. `compact` gives 
#' a polyconic projection with Alaska and Hawaii on the lower left corner;
#' `real` gives the real size and position of all states without any
#' projection.
#'  
#' @examples
#' USArrests2 <- USArrests |> tibble::rownames_to_column("state")
#' mUSMap(USArrests2, key="state", fill = "UrbanPop") 
#' @export 
mUSMap <- function(data = NULL, key, fill = NULL, 
                   plot = c("borders", "frame", "none"),
                   style = c("compact","real")) {
  plot <- match.arg(plot)
  style <- match.arg(style)
  if (style == "compact") {US_States_df <- US_States_comp_df}
  map <- makeMap(data = data, map = US_States_df, key = c(key, "STATE_ABBR"), 
              tr.data = standardState, tr.map = toupper, plot = plot)
  if ( (!is.null(fill) && plot != "none") ) {
    map <- map + geom_polygon(aes(fill = .data[[{{fill}}]]), color = "darkgray")
  }
  map
}


#' Return a dataset based on the CIA World Factbook
#' 
#' This function can be used in two different ways. Without an argument, it returns a reference
#' table that includes information about all the CIA World Factbook tables that are available
#' through this function. Note the  `Name` column that indicates a unique name for each
#' available dataset. If this name is passed as an argument to the function, the function 
#' will return the corresponding dataset.
#' 
#' @param name An optional parameter specifying the name of the desired dataset.
#' If multiple names are given, a merge will be attempted on the individual data
#' sets.
#' 
#' @examples
#' \dontrun{
#' head(CIAdata())
#' Population <- CIAdata("pop")
#' nrow(Population)
#' head(Population)
#' 
#' PopArea <- 
#'   CIAdata(c("pop","area")) |> 
#'   mutate(density = pop / area)
#' nrow(PopArea)
#' head(PopArea)
#' PopArea |> 
#'   filter(!is.na(density)) |>
#'   arrange(density) |> 
#'   tail()
#' }
# #' @importFrom readr parse_number
#' @export
CIAdata <- function (name = NULL) {
 
  rlang::check_installed('readr') 
  if (is.null(name)) return(CIA)  
  
  if (length(name) > 1) {
    return(Reduce(function(A,B) merge(A,B, by="country", all=TRUE), 
                  Map(CIAdata, name=name)))
  }
  
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
  message(paste("Retrieving data from", url))
  
  if (! requireNamespace("RCurl")) stop("Package `RCurl' must be installed.")

# it appears that the file format has moved from tab delimited to fixed width
# parsing on two or more spaces seems to work, at least for some files.
  
#  table <- read.delim(textConnection(RCurl::getURL(url, ssl.verifypeer = FALSE)),
#                      header = FALSE, stringsAsFactors = FALSE)
  lines <- readLines(textConnection(RCurl::getURL(url, ssl.verifypeer = FALSE)))
  table <- as.data.frame(do.call(rbind, strsplit( lines, "  +")), stringsAsFactors=FALSE)
  table[, 1] <- NULL
  names(table) <- c("country", name)
  table[[2]] = readr::parse_number(table[[2]])
    # as.numeric(gsub("[^.+[:digit:] ]", "", table[[2]]))
  return(table)
}


#' Transforms a shapefile into a dataframe
#'
#' This function takes in a shapefile (formal class of
#' `SpatialPolygonsDataFrame`) and transforms it into a dataframe
#'
#' @param map A map object of class `SpatialPolygonsDataFrame`
#' @param ... Other arguments, currently ignored
#' @return A dataframe, in which the first 7 columns hold geographical
#' information (ex: `long` and `lat`)
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
  if (! requireNamespace("sp")) stop( "Package `sp' must be installed.")

  map@data$id <- rownames(map@data)
  coords_matrix <- sp::coordinates(map)  # in sp, which maptools depends on
  map@data$clon = coords_matrix[, 1]
  map@data$clat = coords_matrix[, 2]
  map_points <- do.call(fortify, list(map, region="id"), 
                        envir=environment(ggplot2::fortify))
  result <- merge(map_points, map@data, by = "id")
  return(result)
}

#' ggplot2 theme for maps
#' 
#' A very plain \pkg{ggplot2} theme that is good for maps.
#' 
#' @details
#' This theme is largely based on an example posted by 
#' Winston Chang at the \pkg{ggplot2} Google group forum.
#' 
#' @param base_size the base font size for the theme.
#' @export
theme_map <- function (base_size=12) {
  theme_grey(base_size) %+replace%
    theme(
      axis.title = element_blank (),
      axis.text = element_blank (),
      # axis.text = element_text(margin = grid::unit(0, "cm")),
      panel.background = element_blank (),
      panel.grid = element_blank (),
      axis.ticks.length = grid::unit (0,"cm"),
      # axis.ticks.margin = grid::unit (0.01,"cm"),
      panel.spacing = grid::unit (0,"lines"),
      plot.margin = grid::unit(c(0,0,0,0),"lines"),
      complete = TRUE
    )    
}
