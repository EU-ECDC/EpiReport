#' Get the disease-specific Map
#'
#' Function returning the disease-specific PNG map
#' previously created and stored in a specific folder (see pathPNG)
#' that will be included in the Annual Epidemiological Report (AER)
#' (see reports already available on the ECDC dedicated web page
#' https://ecdc.europa.eu/en/annual-epidemiological-reports)
#'
#' @param disease character string, disease name (default "SALM")
#' @param year numeric, year to produce the report for (default 2016)
#' @param reportParameters dataset of parameters for the report (default reportParameters <- EpiReport::AERparams)
#' @param index integer, figure number
#' @param pathPNG character string, full path to the folder containing the maps in PNG
#' @param doc Word document (see \code{officer} package)
#' @return Word doc an image preview
#' @seealso \code{\link{getAER}}
#' \code{\link{includeMap}} \code{\link{previewMap}}
#' \code{\link{AERparams}}
#' @examples
#' # --- Preview of the PNG map using the default Salmonellosis dataset
#' getMap()
#' @export
getMap <- function(disease = "SALM", year = 2016,
                   reportParameters, index = 1, pathPNG, doc){

  ## ----
  ## Setting default arguments if missing
  ## ----

  if(missing(disease)) { disease <- "SALM" }
  if(missing(year)) { year <- 2016 }
  if(missing(reportParameters)) { reportParameters <- EpiReport::AERparams }
  if(missing(index)) { index <- 1 }
  if(missing(pathPNG)) { pathPNG <- system.file("maps", package = "EpiReport") }
  if(missing(doc)) { preview <- "" }


  ## ----
  ## Filtering
  ## ----

  reportParameters <- filterDisease(disease, reportParameters)


  ## ----
  ## Caption definition
  ## ----

  pop <- ifelse(reportParameters$MeasurePopulation == "ALL", "", "-")
  pop <- ifelse(reportParameters$MeasurePopulation == "CONFIRMED", "confirmed ", pop)



  ## ----
  ## Opt1: Number of cases map
  ## ----

  if(reportParameters$MapNumbersUse == "Y") {
    if(missing(doc)) {
      preview <- c(preview, previewMap(disease, year, reportParameters,
                                       pathPNG, namePNGsuffix = "COUNT"))
    }else{
      doc <- includeMap(disease, year, reportParameters,
                        index, pathPNG, doc, pop,
                        namePNGsuffix = "COUNT",
                        unit = "",
                        mapBookmark = "MAP_NB_BOOKMARK",
                        captionBookmark = "MAP_NB_CAPTION")
      index <- index + 1
    }
  }



  ## ----
  ## Opt2: Rates map
  ## ----

  if(reportParameters$MapRatesUse == "Y") {
    if(missing(doc)) {
      preview <- c(preview, previewMap(disease, year, reportParameters,
                                       pathPNG, namePNGsuffix = "RATE"))
    }else{
      doc <- includeMap(disease, year, reportParameters,
                        index, pathPNG, doc, pop,
                        namePNGsuffix = "RATE",
                        unit = "per 100 000 population",
                        mapBookmark = "MAP_RATE_BOOKMARK",
                        captionBookmark = "MAP_RATE_CAPTION")
      index <- index + 1
    }
  }



  ## ----
  ## Opt3: Age Specific Rate map
  ## ----

  if(reportParameters$MapASRUse == "Y") {
    if(missing(doc)) {
      preview <- c(preview, previewMap(disease, year, reportParameters,
                                       pathPNG, namePNGsuffix = "AGESTANDARDISED"))
    }else{
      doc <- includeMap(disease, year, reportParameters,
                        index, pathPNG, doc, pop,
                        namePNGsuffix = "AGESTANDARDISED",
                        unit = "per 100 000 population",
                        mapBookmark = "MAP_ASR_BOOKMARK",
                        captionBookmark = "MAP_ASR_CAPTION")
      index <- index + 1
    }
  }



  ## ----
  ## No Maps for this disease
  ## ----

  if(reportParameters$MapNumbersUse != "Y" &
     reportParameters$MapRatesUse != "Y" &
     reportParameters$MapASRUse != "Y") {
    message(paste('According to the parameter table \'AERparams\', this disease "',
                  disease, '" does not include any map in the AER report.', sep = ""))
  }


  ## ----
  ## Final output
  ## ----

  if(missing(doc)) {
    return(preview[-1])
  }else{
    return(doc)
  }

}






#' Include the PNG map (Word)
#'
#' Function including the disease-specific PNG map in the Word document
#' at the specific bookmark location
#'
#' @param disease character string, disease name (default "SALM")
#' @param year numeric, year to produce the report for (default 2016)
#' @param reportParameters dataset of parameters for the report
#' (default reportParameters <- EpiReport::AERparams)
#' @param index integer, figure number
#' @param pathPNG character string, full path to the folder containing the maps in PNG
#' @param doc Word document (see \code{officer} package)
#' @param pop character string, label of the type of population used in the caption
#' @param namePNGsuffix character string, suffix of the PNG file name of the map
#' @param unit character string, label of the unit used in the caption
#' @param mapBookmark character string, label of the bookmark in the Word document
#' @param captionBookmark character string, label of the bookmark for the caption in the Word document
#' @return Word doc
#' @seealso \code{\link{getMap}} \code{\link{previewMap}}
#' \code{\link{getAER}}
#' @export
includeMap <- function(disease, year, reportParameters,
                       index, pathPNG, doc,
                       pop, namePNGsuffix, unit,
                       mapBookmark, captionBookmark){

  ## ----- Map file name
  namePNG <- paste(pathPNG, "/", disease, "_", year, ".",
                   reportParameters$MeasurePopulation, ".",
                   namePNGsuffix, ".png", sep = "")

  # --- If word document provided, add the maps in the doc
  officer::cursor_bookmark(doc, id = mapBookmark)
  if( file.exists(namePNG) ){
    doc <- officer::body_add_img(doc, namePNG, width = 7.018, height = 4.956)
  } else {
    warning(paste('The file "', namePNG,
                  '"does not exist and could not be included in the report.',
                  sep = ""),
            call. = FALSE)
  }

  ## ------ Caption definition
  caption <- paste("Figure ", index, ". Distribution of ", pop, reportParameters$Label,
                   " cases ", unit, " by country, ",
                   "EU/EEA, ", year, sep = "")
  officer::cursor_bookmark(doc, id = captionBookmark)
  doc <- officer::body_add_par(doc, value = caption)

  return(doc)

}




#' Preview the PNG map
#'
#' Function preview the disease-specific PNG map
#'
#' @param disease character string, disease name (default "SALM")
#' @param year numeric, year to produce the report for (default 2016)
#' @param reportParameters dataset of parameters for the report (default reportParameters <- EpiReport::AERparams)
#' @param pathPNG character string, full path to the folder containing the maps in PNG
#' @param namePNGsuffix character string, suffix of the PNG file name of the map
#' @return Preview
#' @seealso \code{\link{getMap}} \code{\link{includeMap}}
#' \code{\link{getAER}}
#' @export
previewMap <- function(disease, year, reportParameters,
                       pathPNG, namePNGsuffix){

  ## ----- Map file name
  namePNG <- paste(pathPNG, "/", disease, "_", year, ".",
                   reportParameters$MeasurePopulation, ".",
                   namePNGsuffix, ".png", sep = "")

  # --- If no Word document, then just preview the map
  if( file.exists(namePNG) ){
    img <- png::readPNG(namePNG)
    grid::grid.raster(img)
  } else {
    stop(paste('The file "', namePNG,
               '" does not exist and could not be displayed.',
               sep = ""),
         call. = FALSE)
  }

  return(namePNG)

}
