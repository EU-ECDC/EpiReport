#' Get epidemiological report (empty) template
#'
#' Function to export the generic 'Microsoft Word' empty template (included in
#' the \code{'EpiReport'} package) used to produce the
#' epidemiological report similar to the ECDC Annual Epidemiological Report (AER).
#' The modified version of the template can then be used to produce the final
#' epidemiological report using \code{getAER(template = 'NewTemplate.docx', ...)} \cr
#' (see the package vignette "The Epidemiological Report Package" with
#' \code{browseVignettes("EpiReport")})  \cr
#' (see ECDC annual epidemilogical reports \url{https://ecdc.europa.eu/en/annual-epidemiological-reports})
#'
#' @param output_path character string, the full path where to create the 'Word' output.
#' Defaut location will be the current working directory (default \code{getwd()})
#'
#' @return A 'Word' document
#'
#' @examples
#'
#' \donttest{
#' # --- Export the template in the default folder: working directory
#' getTemplate()
#'
#' # --- Or specify the full path
#' getTemplate(output_path = getwd())
#' }
#'
#'
#' @seealso \code{\link{getAER}}
#'
#' @export
#'
getTemplate <- function(output_path){

  ## ----
  ## Setting default arguments if missing
  ## ----

  if(missing(output_path)){
    output_path <- getwd()
  }

  ## ----
  ## Initialising the 'Word' object
  ## ----

  doc <- officer::read_docx(path = file.path(system.file(package = "EpiReport"),
                                             "template/AER_template.docx" ))

  ## ----
  ## Generating the 'Word' output
  ## ----

  print(doc, target = paste(output_path, "/Empty_AER_template.docx", sep=""))

}





#' Get full disease-specific epidemiological report
#'
#' Function to generate the 'Microsoft Word' epidemiological report
#' (similar to the ECDC Annual Epidemiological Report (AER))
#' including all disease-specific outputs at each output-specific bookmarks exact location. \cr
#' (for further information on the outputs and the corresponding bookmarks,
#' please see the package vignette "The Epidemiological Report Package" with \code{browseVignettes("EpiReport")})\cr
#' (see ECDC AER \url{https://ecdc.europa.eu/en/annual-epidemiological-reports})
#'
#' @param template doc (see \code{'officer'} package), the empty 'Word' document template in which
#' to include the table and plots disease-specific outputs.
#' Default value is the empty template included in the package. See \code{getTemplate()}.
#' @param outputPath character string, the full path where to generate the epidemiological
#' report 'Word' output.
#' Default value is the current working directory \code{getwd()}.
#' @param x dataframe, raw disease-specific dataset (see specification of the dataset in the
#' package vignette with \code{browseVignettes("EpiReport")})
#' (default \code{SALM2016})
#' @param disease character string, disease code (default \code{"SALM"}).
#' Please make sure the disease code is included in the disease-specific dataset x
#' in the \code{HealthTopicCode} variable.
#' @param year numeric, year to produce the report for (default \code{2016}).
#' Please make sure the year is included in the disease-specific dataset x in the \code{TimeCode} variable.
#' @param reportParameters dataframe, dataset including the required parameters for the report
#' production (default \code{AERparams}) (see specification of the dataset in the
#' package vignette with \code{browseVignettes(package = "EpiReport")})
#' @param MSCode dataframe, correspondence table of GeoCode names and codes
#' (default \code{MSCode}) (see specification of the dataset in the
#' package vignette with \code{browseVignettes(package = "EpiReport")})
#' @param pathPNG character string, the full path to the folder containing
#' the maps (in PNG) to include in the final report
#'
#' @seealso Default template: \code{\link{getTemplate}} \cr
#' Default datasets: \code{\link{MSCode}}
#' \code{\link{AERparams}} \code{\link{SALM2016}} \cr
#' Disease-specific outputs: \code{\link{getTableByMS}}
#' \code{\link{getSeason}} \code{\link{getTrend}} \code{\link{getMap}} \code{\link{getAgeGender}}
#'
#' @examples
#'
#' \donttest{
#' # --- Generating the AER report using the default Salmonellosis dataset
#' getAER()
#' }
#'
#' \dontrun{
#' # --- Or using external data (example below)
#' ZIKV2016 <- read.table("data/ZIKV2016.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
#' output <- "C:/EpiReport/doc/"
#' pathMap <- "C:/EpiReport/maps/"
#' getAER(disease = "ZIKV", year = 2016, x = ZIKV2016, outputPath = output, pathPNG = pathMap)
#' }
#'
#'
#' @return A 'Word' document
#'
#' @export
#'
getAER <- function(template =  file.path(system.file(package = "EpiReport"), "template/AER_template.docx" ),
                   outputPath = getwd(),
                   x = EpiReport::SALM2016,
                   disease = "SALM",
                   year = 2016,
                   reportParameters = EpiReport::AERparams,
                   MSCode = EpiReport::MSCode,
                   pathPNG = system.file("maps", package = "EpiReport")){


  ## ----
  ## Setting default arguments if missing
  ## ----
  if(missing(template)){
    template <- file.path(system.file(package = "EpiReport"),
                          "template/AER_template.docx" )
  }
  if(missing(outputPath)){
    outputPath <- getwd()
  }
  if(missing(x)) { x <- EpiReport::SALM2016 }
  if(missing(disease)) { disease <- "SALM" }
  if(missing(year)) { year <- 2016 }
  if(missing(reportParameters)) { reportParameters <- EpiReport::AERparams }
  if(missing(MSCode)) { MSCode <- EpiReport::MSCode }
  if(missing(pathPNG)) { pathPNG <- system.file("maps", package = "EpiReport") }



  ## ----
  ## Filtering
  ## ----
  reportParameters <- filterDisease(disease, reportParameters)



  ## ----
  ## Initialising the 'Word' object
  ## ----
  doc <- officer::read_docx(path = template)



  ## ----
  ## Preparing the data
  ## ----
  x$MeasureCode <- cleanMeasureCode(x$MeasureCode)



  ## ----
  ## Disease and year title
  ## ----
  if ("DISEASE" %in% officer::docx_bookmarks(doc)) {
    doc <- officer::body_replace_text_at_bkm(doc,
                                             bookmark = "DISEASE",
                                             value = toCapTitle(reportParameters$Label))
  }
  doc <- officer::body_replace_text_at_bkm(doc,
                                           bookmark = "YEAR",
                                           value = as.character(year))



  ## ----
  ## Extraction date on which the Atlas is based on
  ## ----
  dateAtlas <- paste("This report is based on data for ", year,
                     " retrieved from The European Surveillance System (TESSy) on ",
                     reportParameters$DatePublicAtlas,
                     ". TESSy is a system for the collection, analysis and dissemination",
                     " of data on communicable diseases.",
                     sep = "")
  doc <- officer::body_replace_text_at_bkm(doc,
                                           bookmark = "DATEPUBLICATLAS",
                                           value = dateAtlas)



  ## ----
  ## Initialising figures and tables numbering
  ## ----
  indexTab <- 1
  indexFig <- 1



  ## ----
  ## Adding the table
  ## ----
  if (reportParameters$TableUse != "NO" &
      "TABLE1" %in% officer::docx_bookmarks(doc)){
    doc <- EpiReport::getTableByMS(x = x,
                                   disease = disease,
                                   year = year,
                                   reportParameters = reportParameters,
                                   MSCode = MSCode,
                                   index = indexTab,
                                   doc = doc)
    indexTab <- indexTab + 1
  }



  ## ----
  ## Map
  ## ----
  if ((reportParameters$MapNumbersUse == "Y" & "MAP_NB" %in% officer::docx_bookmarks(doc)) |
      (reportParameters$MapRatesUse == "Y" & "MAP_RATE" %in% officer::docx_bookmarks(doc)) |
      (reportParameters$MapASRUse == "Y" & "MAP_ASR" %in% officer::docx_bookmarks(doc)) ){
    doc <- EpiReport::getMap(disease = disease,
                             year = year,
                             reportParameters = reportParameters,
                             index = indexFig,
                             pathPNG = pathPNG,
                             doc = doc)
    indexFig <- indexFig + 1
  }



  ## ----
  ## Trend plot
  ## ----
  if (reportParameters$TSTrendGraphUse != "N" &
      "TS_TREND" %in% officer::docx_bookmarks(doc)){
    doc <- EpiReport::getTrend(x = x,
                               disease = disease,
                               year = year,
                               reportParameters = reportParameters,
                               MSCode = MSCode,
                               index = indexFig,
                               doc = doc)
    indexFig <- indexFig + 1
  }



  ## ----
  ## Seasonal plot
  ## ----
  if (reportParameters$TSSeasonalityGraphUse != "N" &
      "TS_SEASON" %in% officer::docx_bookmarks(doc)){
    doc <- EpiReport::getSeason(x = x,
                                disease = disease,
                                year = year,
                                reportParameters = reportParameters,
                                MSCode = MSCode,
                                index = indexFig,
                                doc = doc)
    indexFig <- indexFig + 1
  }



  ## ----
  ## Bar graph
  ## ----

  if (reportParameters$AgeGenderUse != "NO" &
      "BARGPH_AGEGENDER" %in% officer::docx_bookmarks(doc)){
    doc <- EpiReport::getAgeGender(x = x,
                                   disease = disease,
                                   year = year,
                                   reportParameters= reportParameters,
                                   geoCode = "EU_EEA31",
                                   index = indexFig,
                                   doc = doc)}



  ## ----
  ## Generating the 'Word' output
  ## ----

  print(doc,
        target = paste(outputPath, "/AnnualEpidemiologicalReport_",
                       disease, year, ".docx", sep=""))

}

