#' Get AER Word template
#'
#' Function to get the standard Word template for ECDC Annual Epidemiological Report
#'
#' @param output_path The path where to create the Word output.
#' Defaut location will be the current working directory (getwd())
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

  doc <- officer::read_docx(path = file.path(system.file(package = "EpiReport"), "template/AER_template.docx" ))

  ## ----
  ## Generating the word output
  ## ----

  print(doc, target = paste(output_path, "/Empty_AER_template.docx", sep=""))

}



#' Produce the AER report
#'
#' Function to generate the AER Word report
#'
#' @param template The word document in which to add the table.
#' Default value is the empty template included in the package getTemplate().
#' @param outputPath The path for the word output.
#' Default value is the current working directory getwd().
#' @param x dataset including required data for AER
#' @param disease character string, disease name
#' @param year numeric, year to produce the report for
#' @param reportParameters dataset of parameters for the report
#' @param MSCode dataset of corresponding table of GeoCode names and codes
#' @param pathPNG character string, path to the folder containing the maps in PNG
#'
#' @usage getAER(template, outputPath, x, disease, year, reportParameters, MSCode, pathPNG)
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

