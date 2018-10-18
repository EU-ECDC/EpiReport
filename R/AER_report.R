#' Get AER Word template
#'
#' Function to export the standard Word template for ECDC Annual Epidemiological Report (AER)
#' (see reports already available on the ECDC dedicated web page
#' https://ecdc.europa.eu/en/annual-epidemiological-reports)
#'
#' @param output_path character string, the full path where to create the Word output.
#' Defaut location will be the current working directory (default getwd())
#' @usage getTemplate(output_path)
#' @return A word document
#' @examples
#' getTemplate(output_path = "C:/R/AER")
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
  ## Initialising the Word object
  ## ----

  doc <- officer::read_docx(path = file.path(system.file(package = "EpiReport"),
                                             "template/AER_template.docx" ))

  ## ----
  ## Generating the word output
  ## ----

  print(doc, target = paste(output_path, "/Empty_AER_template.docx", sep=""))

}





#' Produce the AER Word report
#'
#' Function to generate the Word ECDC Annual Epidemiological Report (AER)
#' including all disease-specific outputs in the Word bookmark location
#' (see reports already available on the ECDC dedicated web page
#' https://ecdc.europa.eu/en/annual-epidemiological-reports)
#'
#' @param template doc (see \code{officer} package), the word document in which to add the AER outputs.
#' Default value is the empty template included in the package getTemplate().
#' @param outputPath character string, the full path where to generate the AER Word output.
#' Default value is the current working directory getwd().
#' @param x dataframe, raw disease-specific dataset (see more information in the vignette)
#' (default reportParameters <- EpiReport::SALM2016)
#' @param disease character string, disease name (default "SALM")
#' @param year numeric, year to produce the report for (default 2016)
#' @param reportParameters dataset of parameters for the report (default reportParameters <- EpiReport::AERparams)
#' @param MSCode dataset of corresponding table of GeoCode names and codes
#' (default reportParameters <- EpiReport::MSCode)
#' @param pathPNG character string, the full path to the folder containing
#' the maps in PNG to include in the final report
#' @usage getAER(template, outputPath, x, disease, year, reportParameters, MSCode, pathPNG)
#' @seealso \code{\link{getTemplate}} \code{\link{MSCode}}
#' \code{\link{AERparams}}
#' @examples
#' # --- Generating the AER report using the default Salmonellosis dataset
#' getAER()
#'
#' # --- Or using external data (example below)
#' # ZIKV2016 <- read.table("data/ZIKV2016.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
#' # output <- "C:/EpiReport/doc/"
#' # pathMap <- "C:/EpiReport/maps/"
#' # getAER(disease = "ZIKV", year = 2016, x = ZIKV2016, outputPath = output, pathPNG = pathMap)
#'
#' @return A word document
#' @export
getAER <- function(template,
                   outputPath = getwd(),
                   x, disease = "SALM", year = 2016,
                   reportParameters,
                   MSCode,
                   pathPNG){

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
  if(missing(disease)) { disease <- "SALM" }    # disease <- "SALM"
  if(missing(year)) { year <- 2016 }
  if(missing(reportParameters)) { reportParameters <- EpiReport::AERparams }
  if(missing(MSCode)) { MSCode <- EpiReport::MSCode }
  if(missing(pathPNG)) { pathPNG <- system.file("maps", package = "EpiReport") }


  ## ----
  ## Filtering
  ## ----
  reportParameters <- filterDisease(disease, reportParameters)



  ## ----
  ## Initialising the Word object
  ## ----
  doc <- officer::read_docx(path = template)


  ## ----
  ## Preparing the data
  ## ----
  x$MeasureCode <- cleanMeasureCode(x$MeasureCode)


  ## ----
  ## Disease and year title
  ## ----
  doc <- officer::body_replace_text_at_bkm(doc,
                                           bookmark = "DISEASE",
                                           value = toCapTitle(reportParameters$Label))
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
  ## Adding the table
  ## ----
  index <- 1
  doc <- EpiReport::getTableByMS(x = x,
                                 disease = disease,
                                 year = year,
                                 reportParameters = reportParameters,
                                 MSCode = MSCode,
                                 index = index,
                                 doc = doc)
  index <- index + 1




  ## ----
  ## Seasonal plot
  ## ----
  doc <- EpiReport::getSeason(x = x,
                              disease = disease,
                              year = year,
                              reportParameters = reportParameters,
                              MSCode = MSCode,
                              index = index,
                              doc = doc)
  index <- index + 1





  ## ----
  ## Trend plot
  ## ----
  doc <- EpiReport::getTrend(x = x,
                             disease = disease,
                             year = year,
                             reportParameters = reportParameters,
                             MSCode = MSCode,
                             index = index,
                             doc = doc)
  index <- index + 1






  ## ----
  ## Map
  ## ----
  doc <- EpiReport::getMap(disease = disease,
                           year = year,
                           reportParameters = reportParameters,
                           index = index,
                           pathPNG = pathPNG,
                           doc = doc)
  index <- index + 1




  ## ----
  ## Bar graph
  ## ----
  # index <- index + 1

  doc <- EpiReport::getAgeGender(x = x,
                                 disease = disease,
                                 year = year,
                                 reportParameters= reportParameters,
                                 geoCode = "EU_EEA31",
                                 index = index,
                                 doc = doc)





  ## ----
  ## Generating the word output
  ## ----

  print(doc,
        target = paste(outputPath, "/AnnualEpidemiologicalReport_",
                       disease, year, ".docx", sep=""))

}

