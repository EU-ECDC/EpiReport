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

  # if(missing(x)) { x <- EpiReport::SALM2016 }
  if(missing(disease)) { disease <- "SALM" }    # disease <- "SALM"
  if(missing(year)) { year <- 2016 }
  if(missing(reportParameters)) { reportParameters <- EpiReport::AERparams }
  if(missing(index)) { index <- 1 }
  if(missing(pathPNG)) { pathPNG <- system.file("maps", package = "EpiReport") }



  ## ----
  ## Filtering
  ## ----

  # x <- dplyr::filter(x, x$HealthTopic == disease)
  # if( nrow(x) == 0 ) {
  #   stop(paste('The dataset does not include the selected disease "', disease, '".'))
  #   }

  reportParameters <- dplyr::filter(reportParameters, reportParameters$Health.topic == disease)
  if( nrow(reportParameters) ==0 ) {
    stop(paste('The disease "', disease, '" is not described in the parameter table.
               The report cannot be produced.'))
  }



  ## ----
  ## Caption definition
  ## ----

  pop <- ifelse(reportParameters$MeasurePopulation == "ALL", "", "-")
  pop <- ifelse(reportParameters$MeasurePopulation == "CONFIRMED", "confirmed ", "-")



  ## ----
  ## Opt1: Number of cases map
  ## ----

  if(reportParameters$Map.numbers.use == "Y") {
    includemap(disease, year, reportParameters,
               index, pathPNG, doc, pop,
               namePNGsuffix = "COUNT",
               unit = "",
               mapBookmark = "MAP_NB_BOOKMARK",
               captionBookmark = "MAP_NB_CAPTION")
  }



  ## ----
  ## Opt2: Rates map
  ## ----

  if(reportParameters$Map.rates.use == "Y") {
    includemap(disease, year, reportParameters,
               index, pathPNG, doc, pop,
               namePNGsuffix = "RATE",
               unit = "per 100 000 population",
               mapBookmark = "MAP_RATE_BOOKMARK",
               captionBookmark = "MAP_RATE_CAPTION")
  }



  ## ----
  ## Opt3: Age Specific Rate map
  ## ----

  if(reportParameters$Map.ASR.use == "Y") {
    includemap(disease, year, reportParameters,
               index, pathPNG, doc, pop,
               namePNGsuffix = "AGESTANDARDISED",
               unit = "per 100 000 population",
               mapBookmark = "MAP_ASR_BOOKMARK",
               captionBookmark = "MAP_ASR_CAPTION")
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
#' @return Word doc a preview
includemap <- function(disease, year, reportParameters,
                       index, pathPNG, doc,
                       pop, namePNGsuffix, unit,
                       mapBookmark, captionBookmark){

  ## ----- Map file name
  namePNG <- paste(pathPNG, "/", disease, "_", year, ".",
                   reportParameters$MeasurePopulation, ".",
                   namePNGsuffix, ".png", sep = "")

  if(missing(doc)) {

    # --- If no Word document, then just preview the map
    img <- png::readPNG(namePNG)
    grid::grid.raster(img)

  } else {

    # --- If word document provided, add the maps in the doc
    officer::cursor_bookmark(doc, id = mapBookmark)
    doc <- officer::body_add_img(doc, namePNG, width = 7.018, height = 4.956)

    ## ------ Caption definition
    caption <- paste("Figure ", index, ". Distribution of ", pop, reportParameters$Label,
                     " cases", unit, " by country, ",
                     "EU/EEA, ", year, sep = "")
    officer::cursor_bookmark(doc, id = captionBookmark)
    doc <- officer::body_add_break(doc)
    doc <- officer::body_add_par(doc, value = caption)

    #-----> Adding the link
    # link <- pot('Link to the graph',
    #             hyperlink = parameters$Map.rates.URL,
    #             format=textProperties(color = MAINCOLOR , underline = TRUE ))
    # doc <- addParagraph(doc, link, bookmark = "MAP_RATE_BOOKMARK_LINK")

    return(doc)
  }
}
