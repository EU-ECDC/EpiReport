#' Dataset for AER production
#'
#' A dummy dataset (ECDC Atlas export) containing the information required to build the AER
#' for a set of 6 diseases: Botulism, Cholera, Crimean-Congo haemorrhagic fever,
#' Pertussis, Salmonellosis, Tuberculosis
#'
#' @format A data frame with 90,649 rows and 9 variables:
#' \describe{
#'   \item{HealthTopic}{Disease name e.g. Botulism, Cholera, etc}
#'   \item{Population}{Population characteristics e.g. All cases, Confirmed cases, etc}
#'   \item{Indicator}{Indicator e.g. Age-standardised rate, Hospitalised cases, Reported cases, Number of deaths, etc.}
#'   \item{Unit}{Unit of the indicator e.g. N \% or 1/100000}
#'   \item{Time}{Time variable including both yearly data from 1995 to 2016, and monthly data from 1998-01 to 2016-12}
#'   \item{RegionCode}{Geographical level including country codes e.g. AT, BE, BG, etc.}
#'   \item{RegionName}{Geographical level including country names e.g. Austria, Belgium, Bulgaria, etc.}
#'   \item{NumValue}{Value of the indicator}
#'   \item{TxtValue}{...}
#' }
#' @docType data
#' @keywords datasets
#' @name AERdata
#' @usage data(AERdata)
#' @source \url{http://atlas.ecdc.europa.eu/public/index.aspx}
"AERdata"


#' Dataset including parameters for AER production
#'
#' A dataset containing the parameters required to build the AER
#' for all 53 diseases included in TESSy
#'
#' @format A data frame with 53 rows (corresponding to the 53 diseases) and 28 variables:
#' \describe{
#'   \item{Health.topic}{Disease code e.g. ANTH, SALM, etc.}
#'   \item{DG}{}
#'   \item{DP}{}
#'   \item{Label}{}
#'   \item{Frequency.category}{}
#'   \item{MeasurePopulation}{}
#'   \item{DatePublicAtlas}{}
#'   \item{Table1.Use}{}
#'   \item{Table1.Rates.use}{}
#'   \item{Table1.Rates.label}{}
#'   \item{Table1.Rates.No.Decimals}{}
#'   \item{Table1.ASR.use}{}
#'   \item{Table1.ASR.No.Decimals}{}
#'   \item{AgeGender.bar.graph.used}{}
#'   \item{AgeGender.Rates.use}{}
#'   \item{Age.bar.graph.used}{}
#'   \item{AgeGender.bar.graph.label}{}
#'   \item{AgeGender.graph.No.Decimals}{}
#'   \item{TS.trend.graph.use}{}
#'   \item{TS.seasonality.graph.use}{}
#'   \item{TS.specific}{}
#'   \item{Map.numbers.use}{}
#'   \item{Map.rates.use}{}
#'   \item{No.Decimals}{}
#'   \item{Map.ASR.use}{}
#'   \item{No.Decimals.1}{}
#'   \item{Transmission}{}
#'   \item{Transmission.No.Decimals}{}
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
