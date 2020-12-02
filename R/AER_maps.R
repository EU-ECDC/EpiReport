#' Get disease-specific map: distribution of cases by Member State
#'
#' Function returning the disease-specific PNG map previously created
#' and stored in a specific folder (see \code{pathPNG} argument) and
#' that will be included in the epidemiological report at the bookmark location
#' of the template report, depending of the type of map.
#' Three type of maps can be included in the report:
#' \itemize{
#'    \item{Bookmark \code{'MAP_NB'}: }{Distribution of cases by country.
#'    An additional caption will be included at the location of the bookmark \code{'MAP_NB_CAPTION'}.}
#'    \item{Bookmark \code{'MAP_RATE'}: }{Distribution of cases
#'    per 100 000 population by country. An additional caption will be included
#'    at the location of the bookmark \code{'MAP_RATE_CAPTION'}.}
#'    \item{Bookmark \code{'MAP_ASR'}: }{Distribution of cases using
#'    age-strandardised rates per 100 000 population by country.
#'    An additional caption will be included at the location of the bookmark \code{'MAP_ASR_CAPTION'}.}
#' }
#' (see ECDC reports
#' \url{https://ecdc.europa.eu/en/annual-epidemiological-reports})
#'
#' @param disease character string, disease code (default \code{"DENGUE"}).
#' @param year numeric, year to produce the map for (default \code{2019}).
#' @param reportParameters dataframe, dataset including the required parameters
#' for the map and report production (default \code{AERparams}) (see specification
#' of the dataset in the package vignette with \code{browseVignettes(package = "EpiReport")})
#' @param index integer, figure number
#' @param pathPNG character string, full path to the folder containing the maps in PNG
#' (default 'maps' folder included in the package \code{system.file("maps", package = "EpiReport")})
#' @param doc 'Word' document (see \code{'officer'} package) in which to add the maps
#' at the bookmark location.
#' If doc is missing, \code{getMap} returns a preview of the PNG image.
#'
#' @return 'Word' doc an image preview
#'
#' @seealso Global function for the full epidemilogical report: \code{\link{getAER}}  \cr
#' Required Packages: \code{\link{officer}} \cr
#' Internal functions: \code{\link{includeMap}} \code{\link{previewMap}} \cr
#' Default datasets: \code{\link{AERparams}}
#'
#' @examples
#' # --- Preview of the PNG map using the default Dengue dataset
#' getMap()
#'
#' # --- Plot using external PNG image
#' # --- Please see examples in the vignette
#' browseVignettes(package = "EpiReport")
#'
#' @export
#'
getMap <- function(disease = "DENGUE",
                   year = 2019,
                   reportParameters = EpiReport::AERparams,
                   index = 1,
                   pathPNG = system.file("maps", package = "EpiReport"),
                   doc){

  ## ----
  ## Setting default arguments if missing
  ## ----

  if(missing(disease)) { disease <- "DENGUE" }
  if(missing(year)) { year <- 2019 }
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
                        mapBookmark = "MAP_NB",
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
                        mapBookmark = "MAP_RATE",
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
                        mapBookmark = "MAP_ASR",
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






#' Including PNG map in the 'Microsoft Word' template
#'
#' Function including the disease-specific PNG map in the 'Word' document
#' at the specific bookmark location.
#'
#' @param disease character string, disease code (default \code{"DENGUE"}).
#' @param year numeric, year to produce the graph for (default \code{2019}).
#' @param reportParameters dataframe, dataset including the required parameters
#' for the graph and report production (default \code{AERparams}) (see specification
#' of the dataset in the package vignette with \code{browseVignettes(package = "EpiReport")})
#' @param index integer, figure number
#' @param pathPNG character string, full path to the folder containing the maps in PNG
#' (default 'maps' folder included in the package \code{system.file("maps", package = "EpiReport")})
#' @param doc 'Word' document (see \code{'officer'} package) in which to add
#' the maps at the bookmark location
#' @param pop character string, label of the type of population to use in the caption
#' (e.g. \code{confirmed})
#' @param namePNGsuffix character string, suffix of the PNG file name of the map
#' (i.e. \code{"COUNT"}, \code{"RATE"} or \code{"AGESTANDARDISED"}.)
#' @param unit character string, label of the unit used in the caption
#' (e.g. \code{"per 100 000 population"})
#' @param mapBookmark character string, label of the bookmark where to add
#' the map in the 'Word' document
#' @param captionBookmark character string, label of the bookmark where to add
#' the caption in the 'Word' document
#'
#' @return 'Word' doc
#'
#' @seealso Global function: \code{\link{getMap}}
#'
#' @export
#'
includeMap <- function(disease, year, reportParameters,
                       index, pathPNG, doc,
                       pop, namePNGsuffix, unit,
                       mapBookmark, captionBookmark){

  ## ----- Map file name
  namePNG <- paste(pathPNG, "/", disease, "_", year, ".",
                   reportParameters$MeasurePopulation, ".",
                   namePNGsuffix, ".png", sep = "")

  # --- If 'Word' document provided, add the maps in the doc
  officer::cursor_bookmark(doc, id = mapBookmark)
  if( file.exists(namePNG) ){
    doc <- officer::body_replace_img_at_bkm(x = doc,
                                            bookmark = mapBookmark,
                                            value = officer::external_img(src = namePNG,
                                                                          width = 7.018,
                                                                          height = 4.956))
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
  doc <- officer::body_replace_text_at_bkm(x = doc,
                                           bookmark = captionBookmark,
                                           value = caption)

  return(doc)

}




#' Previewing the PNG map
#'
#' Function previewing the disease-specific PNG map
#'
#' @param disease character string, disease code (default \code{"DENGUE"}).
#' @param year numeric, year to produce the graph for (default \code{2019}).
#' @param reportParameters dataframe, dataset including the required parameters
#' for the graph and report production (default \code{AERparams}) (see specification
#' of the dataset in the package vignette with \code{browseVignettes(package = "EpiReport")})
#' @param pathPNG character string, full path to the folder containing the maps in PNG
#' (default 'maps' folder included in the package \code{system.file("maps", package = "EpiReport")})
#' @param namePNGsuffix character string, suffix of the PNG file name of the map
#' (i.e. \code{"COUNT"}, \code{"RATE"} or \code{"AGESTANDARDISED"}.)
#'
#' @return Preview
#'
#' @seealso Global function: \code{\link{getMap}}
#'
#' @export
#'
previewMap <- function(disease, year, reportParameters,
                       pathPNG, namePNGsuffix){

  ## ----- Map file name
  namePNG <- paste(pathPNG, "/", disease, "_", year, ".",
                   reportParameters$MeasurePopulation, ".",
                   namePNGsuffix, ".png", sep = "")

  # --- If no 'Word' document, then just preview the map
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
