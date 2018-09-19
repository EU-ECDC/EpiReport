#' Dataset including parameters for AER production
#'
#' A dataset containing the parameters required to build the AER
#' for all 53 diseases included in TESSy
#'
#' @format A data frame with 53 rows (corresponding to the 53 diseases) and 28 variables:
#' \describe{
#'   \item{HealthTopic}{Disease code e.g. ANTH, SALM, etc.}
#'   \item{DG}{}
#'   \item{DP}{}
#'   \item{Label}{}
#'   \item{FrequencyCategory}{}
#'   \item{MeasurePopulation}{}
#'   \item{DatePublicAtlas}{}
#'   \item{TableUse}{}
#'   \item{TableRatesLabel}{}
#'   \item{TableRatesNoDecimals}{}
#'   \item{TableASRNoDecimals}{}
#'   \item{AgeGenderBarGraphUse}{}
#'   \item{AgeGenderRatesUse}{}
#'   \item{AgeBarGraphUse}{}
#'   \item{AgeGenderBarGraphLabel}{}
#'   \item{AgeGenderGraphNoDecimals}{}
#'   \item{TSTrendGraphUse}{}
#'   \item{TSSeasonalityGraphUse}{}
#'   \item{TSSpecific}{}
#'   \item{MapNumbersUse}{}
#'   \item{MapRatesUse}{}
#'   \item{MapRatesNoDecimals}{}
#'   \item{MapASRUse}{}
#'   \item{MapASRNoDecimals}{}
#'   \item{Transmission}{}
#'   \item{TransmissionNoDecimals}{}
#' }
#' @docType data
#' @keywords datasets
#' @name AERparams
#' @usage data(AERparams)
"AERparams"


#' Dataset including Salmonellosis data for 2016
#'
#' A dataset containing the parameters required to build the AER
#' for Salmonellosis TESSy data
#'
#' @format A data frame with 60,775 rows and 18 variables:
#' \describe{
#'   \item{HealthTopicCode}{Disease code e.g. ANTH, SALM, etc.}
#'   \item{MeasureLabel}{}
#'   \item{MeasurePopulation}{}
#'   \item{MeasureCode}{}
#'   \item{MeasureId}{}
#'   \item{MeasureType}{}
#'   \item{TimeUnit}{}
#'   \item{GeoLevel}{}
#'   \item{TimeCode}{}
#'   \item{GeoCode}{}
#'   \item{XValue}{}
#'   \item{XLabel}{}
#'   \item{YValue}{}
#'   \item{YLabel}{}
#'   \item{ZValue}{}
#'   \item{N}{}
#'   \item{NMissing}{}
#'   \item{NLowerResolution}{}
#' }
#' @docType data
#' @keywords datasets
#' @name SALM2016
#' @usage data(SALM2016)
"SALM2016"


#' Dataset correspondence table for country names and code
#'
#' Dataset correspondence table for country names and code
#'
#' @format A data frame with 32 rows and 2 variables:
#' \describe{
#'   \item{Country}{Country name}
#'   \item{GeoCode}{Country code}
#' }
#' @docType data
#' @keywords datasets
#' @name MSCode
#' @usage data(MSCode)
"MSCode"


