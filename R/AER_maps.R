#' Get the disease map
#'
#' Function returning the map
#'
#' @param disease character string, disease name
#' @param year numeric, year to produce the report for
#' @param reportParameters dataset of parameters for the report
#' @param index figure number
#' @param pathPNG character string, path to the folder containing the maps in PNG
#' @param doc Word document
#' @return Word doc a preview
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






#' Function including the PNG map
#'
#' Function including the PNG map
#'
#' @param disease character string, disease name
#' @param year numeric, year to produce the report for
#' @param reportParameters dataset of parameters for the report
#' @param index figure number
#' @param pathPNG character string, path to the folder containing the maps in PNG
#' @param doc Word document
#' @param pop Label of the type of population used in the caption
#' @param namePNGsuffix Suffix of the PNG file name of the map
#' @param unit Label of the unit used in the caption
#' @param mapBookmark Bookmark for the map in the Word document
#' @param captionBookmark Bookmark for the caption in the Word document
#' @return Word doc
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


#' Function including the PNG map
#'
#' Function including the PNG map
#'
#' @param disease character string, disease name
#' @param year numeric, year to produce the report for
#' @param reportParameters dataset of parameters for the report
#' @param pathPNG character string, path to the folder containing the maps in PNG
#' @param namePNGsuffix Suffix of the PNG file name of the map
#' @return Preview
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
