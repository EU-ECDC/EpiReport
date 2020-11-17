#' Dataset describing the parameters for the epidemiological report production
#'
#' A dataset describing the parameters to be used for each output of each disease report
#' for all 53 health topics included in TESSy
#'
#' @format A data frame with 53 rows (corresponding to the 53 health topics) and 24 variables:
#' \describe{
#'   \item{HealthTopic}{Disease code that should match with the health topic code
#'      from the disease-specific dataset  e.g. \code{ANTH}, \code{SALM}, etc.}
#'   \item{DG}{(optional) Disease group e.g. FWD}
#'   \item{DP}{(optional) Disease programme e.g. FWD}
#'   \item{Label}{Disease label to be used in the report e.g. salmonellosis, anthrax}
#'   \item{FrequencyCategory}{(optional) Frequency of the disease e.g. \code{VERY RARE}, \code{NON-RARE}, etc.}
#'   \item{MeasurePopulation}{Type of population presented for this disease i.e. \code{ALL} or \code{CONFIRMED} cases}
#'   \item{DatePublicAtlas}{Date of latest availability in the public access of the Atlas}
#'   \item{TableUse}{Type of table to present in the report i.e. \code{NO} table,
#'      \code{ASR} table presenting age-standardised rates, \code{RATE} table presenting rates
#'      or \code{COUNT} table presenting the number of cases only.}
#'   \item{TableRatesLabel}{Label to use in the table for rates e.g. \code{RATE PER 100000 POPULATION}}
#'   \item{TableRatesNoDecimals}{Number of decimals to use when presenting rates}
#'   \item{TableASRNoDecimals}{Number of descimals to use when presenting ASR}
#'   \item{AgeGenderUse}{Type of age and gender bar graph to present i.e. \code{NO} graph,
#'      \code{AG-COUNT} Bar graph presenting the number of cases by age and gender,
#'      \code{AG-RATE} Bar graph presenting the rates of cases by age and gender,
#'      \code{AG-PROP} Bar graph presenting the proportion of cases by age and gender,
#'      \code{A-RATE} Bar graph presenting the rates of cases by age.}
#'   \item{AgeGenderBarGraphLabel}{Label to use in the age and gender bar graph}
#'   \item{AgeGenderGraphNoDecimals}{Number of decimals to use when presenting rates
#'      in the age and gender bar graph}
#'   \item{TSTrendGraphUse}{Logical Y/N specifying whether to include a line graph
#'      describing the trend of the disease over the time}
#'   \item{TSSeasonalityGraphUse}{Logical Y/N specifying whether to include
#'      a line graph describing the seasonality of the disease}
#'   \item{TSSpecific}{Logical Y/N for specific line graph inclusion}
#'   \item{MapNumbersUse}{Logical Y/N specifying whether to include the map
#'      presenting the number of cases by Member State}
#'   \item{MapRatesUse}{Logical Y/N specifying whether to include the map
#'      presenting the rates of cases by Member State}
#'   \item{MapRatesNoDecimals}{(optional) Number of decimals to use for presenting maps}
#'   \item{MapASRUse}{Logical Y/N specifying whether to include the map
#'      presenting the age-standardised rates of cases by Member State}
#'   \item{MapASRNoDecimals}{(optional) Number of decimals to use for presenting maps}
#'   \item{Transmission}{Not implemented yet}
#'   \item{TransmissionNoDecimals}{Not implemented yet}
#' }
#' @docType data
#' @keywords datasets
#' @name AERparams
#' @usage AERparams
"AERparams"


#' Dataset including Salmonellosis data for 2012-2016
#'
#' A dataset containing the data and indicators required to build the epidemiological report
#' for Salmonellosis 2016 TESSy data (default dataset used throughout \code{EpiReport})
#'
#' @format A data frame with 60,775 rows and 18 variables:
#' \describe{
#'   \item{HealthTopicCode}{Disease code e.g. \code{ANTH}, \code{SALM}, etc.}
#'   \item{MeasureLabel}{optional) Label of the measure indicator}
#'   \item{MeasurePopulation}{Population targeted by the measure indicator}
#'   \item{MeasureCode}{Code of the measure indicator}
#'   \item{MeasureId}{(optional) Measure indicator ID}
#'   \item{MeasureType}{(optional) Type of measure indicator}
#'   \item{TimeUnit}{Unit of the time variable i.e. \code{Y} for yearly data or \code{M} for monthly data}
#'   \item{GeoLevel}{(optional) Geographical level e.g. 1, 2, etc}
#'   \item{TimeCode}{Time variable including dates in any formats available
#'   (according to the unit defined in \code{TimeUnit}) yearly data (e.g. 2001) or monthly data (e.g. 2001-01)}
#'   \item{GeoCode}{Geographical level in coded format including country names
#'   (e.g. \code{AT} for Austria, \code{BE} for Belgium, \code{BG} for Bulgaria,
#'   see also the \code{EpiReport::MSCode} table, correspondence table for Member State labels and codes)}
#'   \item{XValue}{(optional) XValue}
#'   \item{XLabel}{The label associated with the x-axis in the epidemiological report
#'   (see \code{getAgeGender()} and \code{plotAgeGender()} bar graph for the age variable)}
#'   \item{YValue}{The value associated with the y-axis in the epidemiological report
#'   (see \code{plotAge()} bar graph for the variable age, or \code{getTableByMS()}
#'   for the number of cases, rate or age-standardised rate in the table by Member States by year)}
#'   \item{YLabel}{The label associated with the y-axis in the epidemiological report
#'   (see \code{getAgeGender()} and \code{plotAgeGender()} bar graph for the grouping variable gender)}
#'   \item{ZValue}{The value associated with the stratification of XLabel and YLabel
#'   in the age and gender bar graph (see \code{getAgeGender()} and \code{plotAgeGender()})}
#'   \item{N}{Number of cases (see \code{getTrend()} and \code{getSeason()} line graph)}
#'   \item{NMissing}{(optional)}
#'   \item{NLowerResolution}{(optional)}
#' }
#' @docType data
#' @keywords datasets salmonellosis
#' @name SALM2016
#' @usage SALM2016
#' @seealso The correspondence table for Member State labels and codes \code{\link{MSCode}} \cr
#' and the functions mentioned above: \code{\link{getAgeGender}},
#' \code{\link{plotAgeGender}},  \code{\link{plotAge}},  \code{\link{getTableByMS}},
#' \code{\link{getTrend}} and \code{\link{getSeason}}.
#'
"SALM2016"


#' Dataset correspondence table between country names and country code
#'
#' Dataframe providing the correspondence table of the geographical code \code{GeoCode}
#' used in the disease dataset, and the geographical label \code{Country} to use
#' throughout the report. Additional information on the EU/EEA affiliation
#' is also available in column \code{EUEEA}.
#'
#' @format A data frame with 32 rows and 3 variables:
#' \describe{
#'   \item{Country}{Full name of the country / Member State e.g. Austria, Belgium, etc.}
#'   \item{TheCountry}{Full name of the country / Member State
#'   including 'the' article for NL and UK  e.g. Austria, Belgium, the Netherlands, the United Kingdom etc.}
#'   \item{GeoCode}{Associated code (see \code{GeoCode} variable
#'      on the \code{SALM2016} internal dataset) e.g. AT, BE, BG, etc.}
#'   \item{EUEEA}{For each Member State, variable specifying in the country
#'      is part of the EU or EEA.}
#' }
#' @docType data
#' @keywords datasets
#' @name MSCode
#' @usage MSCode
#' @seealso \code{\link{SALM2016}}
"MSCode"


