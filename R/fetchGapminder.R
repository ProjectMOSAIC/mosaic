#' @rdname fetchGapminder
#' Fetch Gapminder data
#'
#' Fetch data originally obtained from Gapminder.
#' 
#' @param name a chracter vector of length 1
#' @param \dots character strings naming desired variables
#' @return A data frame
#' @export

fetchGapminderSimple <- function(name,value.name=NULL){
  if (! require(reshape2) ) { stop("plyr required for this function") }
  if( is.null(value.name) ) # Just the base name of the file --- no extension.
    value.name <- sub("\\.[^.]+$","",basename(name))
  dat <- fetchData(name)
  countryLabel <- names(dat)[1]
  yearCols <- names(dat)[-1]
  res <- melt(dat, id.vars=countryLabel, value.name="Var", 
              measure.vars=yearCols, variable.name="Year")
  res <- subset(res, !is.na(Var))
  res$Year <- as.numeric(sub("X","",as.character(res$Year)))
  names(res) <- c("Country","Year", value.name)
  return(res)
}

#' @rdname fetchGapminder
#' @param all.cases a logical indicating whether all cases should be included.  
#' If FALSE, only cases with complete data are included.
#' @param all.vars a logical indicating whether all available data should be 
#' downloaded.
#' @export
#' 
fetchGapminder <- function(..., all.cases=TRUE, all.vars=FALSE) {
  datasets <- list()
  availableNames <- sort(c(
    'AidReceived', 'FemaleBMI', 
    'LowWeightForAgePercent', 'TCfemale', 
    'Alcohol', 'Fertility', 
    'MaleBMI', 'TCmale', 
    'Armsimports', 'HIVprevalence', 
    'MarriageAgeFemale', 'TobaccoUseFemale', 
    'CellPhoneTotal', 'ImprovedSanitation', 
    'PavedRoads', 'TobaccoUseMale', 
    'CO2emissions', 'IncomePerCapitaPPP', 
    'PrimaryEdCompletionFemale', 'TotalPopulation', 
    'ContraceptiveUse', 'LandArea', 
    'PrimaryEdCompletionMale', 'TrafficDeathRate', 
    'DemocracyScore', 'LiverCancerIncidenceFemale', 
    'PrimaryEdCompletionTotal', 'Under5mortality', 
    'EconomicGrowth', 'LiverCancerIncidenceMale', 
    'PrimaryEdSpending', 'UrbanPopulationPercent')
  )
  requestedNames <- unlist(list(...))
  if (all.vars) requestedNames <- availableNames
  vars <- match.arg( requestedNames, availableNames, several.ok=TRUE )
  if (length(requestedNames) != length( vars )) 
    warning("Omitting some variables because they are not uniquely determined by your request.")
  for ( v in vars ) {
    datasets[[1 + length(datasets)]] <- fetchGapminderSimple(paste("Gapminder/", v,".csv", sep="")) 
  }
  if (length(datasets) < 1) return (NULL)
  result <- datasets[[1]]
  for ( data in tail(datasets, -1) ) {
    result <- merge( result, data, by=c("Country", "Year"), all=all.cases)
  }  
  return(result)
}