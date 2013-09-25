#' @rdname fetchGapminder
#' Fetch Gapminder data
#'
#' Fetch data originally obtained from Gapminder.
#' 
#' @param name a character vector of length 1
#' @param \dots character strings naming desired variables
#' @return A data frame
#' @export

fetchGapminder1 <- function(name,value.name=NULL){
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
#' downloaded, in which case \dots is ignored.
#' @details There is little reason for the end user to use \code{fetchGapminder1};
#' \code{fetchGapminder} provides a simpler user interface in which 
#' the user can specify the desired variables to be included in the 
#' data frame returned.
#' 
#' The following variables are currently available
#'    \code{'AidReceived'}, 
#'    \code{'Alcohol'},
#'    \code{'Armsimports'}, 
#'    \code{'CellPhoneTotal'}, 
#'    \code{'CO2emissions'}, 
#'    \code{'ContraceptiveUse'}, 
#'    \code{'DemocracyScore'}, 
#'    \code{'EconomicGrowth'}, 
#'    \code{'FemaleBMI'}, 
#'    \code{'Fertility'}, 
#'    \code{'HIVprevalence'}, 
#'    \code{'ImprovedSanitation'}, 
#'    \code{'IncomePerCapitaPPP'}, 
#'    \code{'LandArea'}, 
#'    \code{'LiverCancerIncidenceFemale'}, 
#'    \code{'LiverCancerIncidenceMale'}, 
#'    \code{'LowWeightForAgePercent'},
#'    \code{'MaleBMI'},
#'    \code{'MarriageAgeFemale'}, 
#'    \code{'PavedRoads'}, 
#'    \code{'PrimaryEdCompletionFemale'}, 
#'    \code{'PrimaryEdCompletionMale'}, 
#'    \code{'PrimaryEdCompletionTotal'}, 
#'    \code{'PrimaryEdSpending'},  
#'    \code{'TCfemale'}, 
#'    \code{'TCmale'}, 
#'    \code{'TobaccoUseFemale'}, 
#'    \code{'TobaccoUseMale'}, 
#'    \code{'TotalPopulation'}, 
#'    \code{'TrafficDeathRate'}, 
#'    \code{'Under5mortality'}, 
#'    and 
#'    \code{'UrbanPopulationPercent'}
#'    
#' @source The data sets are generated from snapshots of the data available
#' at \url{http://www.gapminder.org/}.
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
    datasets[[1 + length(datasets)]] <- fetchGapminder1(paste("Gapminder/", v,".csv", sep="")) 
  }
  if (length(datasets) < 1) return (NULL)
  result <- datasets[[1]]
  for ( data in tail(datasets, -1) ) {
    result <- merge( result, data, by=c("Country", "Year"), all=all.cases)
  }  
  return(result)
}