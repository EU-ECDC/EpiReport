#' Dataset including parameters for AER production
#'
#' A dataset containing the parameters required to build the AER
#' for all 53 diseases included in TESSy
#'
#' @format A data frame with 53 rows (corresponding to the 53 diseases) and 28 variables:
#' \describe{
#'   \item{HealthTopic}{Disease code e.g. ANTH, SALM, etc.}
#'   \item{DG}{(optional) Disease group e.g. FWD}
#'   \item{DP}{(optional) Disease programme e.g. FWD}
#'   \item{Label}{Disease label e.g. salmonellosis, anthrax}
#'   \item{FrequencyCategory}{(optional) Frequency of the disease e.g. VERY RARE, NON-RARE, etc.}
#'   \item{MeasurePopulation}{Type of population presented for this disease e.g. all or confirmed cases}
#'   \item{DatePublicAtlas}{Date of latest availability in the public access of the Atlas}
#'   \item{TableUse}{Type of table to present in the report e.g. NO table, ASR, RATE table, etc.}
#'   \item{TableRatesLabel}{Label to use in the table for presenting rates}
#'   \item{TableRatesNoDecimals}{Number of decimals to use for presenting rates}
#'   \item{TableASRNoDecimals}{Number of descimals to use for presenting ASR}
#'   \item{AgeGenderUse}{Type of age and gender bar graph to present e.g. NO graph, AG-RATE, AG-COUNT, etc.}
#'   \item{AgeGenderBarGraphLabel}{Label to use in the age and gender bar graph}
#'   \item{AgeGenderGraphNoDecimals}{Number of decimals to use for presenting rates in the age an dgender bar graph}
#'   \item{TSTrendGraphUse}{Logical Y/N for trend line graph inclusion}
#'   \item{TSSeasonalityGraphUse}{Logical Y/N for seasonal line graph inclusion}
#'   \item{TSSpecific}{Logical Y/N for specific line graph inclusion}
#'   \item{MapNumbersUse}{Logical Y/N for map inclusion}
#'   \item{MapRatesUse}{Logical Y/N for map inclusion}
#'   \item{MapRatesNoDecimals}{(optional) Number of decimals to use for presenting maps}
#'   \item{MapASRUse}{Logical Y/N for map inclusion}
#'   \item{MapASRNoDecimals}{(optional) Number of decimals to use for presenting maps}
#'   \item{Transmission}{Not implemented yet}
#'   \item{TransmissionNoDecimals}{Not implemented yet}
#' }
#' @docType data
#' @keywords datasets
#' @name AERparams
#' @usage AERparams
"AERparams"


#' Dataset including Salmonellosis data for 2016
#'
#' A dataset containing the parameters required to build the AER
#' for Salmonellosis TESSy data
#'
#' @format A data frame with 60,775 rows and 18 variables:
#' \describe{
#'   \item{HealthTopicCode}{Disease code e.g. ANTH, SALM, etc.}
#'   \item{MeasureLabel}{Label of the measure indicator}
#'   \item{MeasurePopulation}{Population targeted by the measure indicator}
#'   \item{MeasureCode}{Measure indicator code}
#'   \item{MeasureId}{(optional) Measure indicator ID}
#'   \item{MeasureType}{(optional) Type of measure indicator}
#'   \item{TimeUnit}{Unit of the time variable e.g. Y or M}
#'   \item{GeoLevel}{Geographical level e.g. 1, 2, etc}
#'   \item{TimeCode}{Time variable according to the unit defined in TimeUnit}
#'   \item{GeoCode}{Geographical level including country names e.g. AU, BE, BG, etc.}
#'   \item{XValue}{XValue}
#'   \item{XLabel}{XLabel}
#'   \item{YValue}{YValue}
#'   \item{YLabel}{YLabel}
#'   \item{ZValue}{ZValue}
#'   \item{N}{Number of cases}
#'   \item{NMissing}{(optional)}
#'   \item{NLowerResolution}{(optional)}
#' }
#' @docType data
#' @keywords datasets
#' @name SALM2016
#' @usage SALM2016
"SALM2016"


#' Dataset correspondence table for country names and code
#'
#' Dataset correspondence table for country names, code and EU or EEA.
#' EUEEA code and name is included in a additional row in the dataframe for
#' the EUEEA summary of table 1 in the AER report
#'
#' @format A data frame with 32 rows and 2 variables:
#' \describe{
#'   \item{Country}{Country name}
#'   \item{GeoCode}{Country code}
#'   \item{EUEEA}{For each country, EU or EEA depending on the country}
#' }
#' @docType data
#' @keywords datasets
#' @name MSCode
#' @usage MSCode
"MSCode"


