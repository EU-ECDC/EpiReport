#' Get disease-specific table: distribution of cases by Member State (GeoCode)
#'
#' Function returning the table (\code{'flextable'}) that will be included
#' in the epidemiological report at the bookmark location \code{'TABLE1_BOOKMARK'}
#' of the template report. An additional caption will be included at the location
#' of the bookmark \code{'TABLE1_CAPTION'}. \cr
#' (see Table 1 of the ECDC annual reports
#' \url{https://ecdc.europa.eu/en/annual-epidemiological-reports})
#'
#' The current version of the \code{'EpiReport'} package includes three types of table
#' (see detailed specification of the tables in the
#' package vignette with \code{browseVignettes(package = "EpiReport")}):
#' \itemize{
#'    \item{\code{COUNT} - }{Table presenting the number of cases by Member State (GeoCode) over a 5-year period};
#'    \item{\code{RATE} - }{Table presenting the number of cases and rates by Member State (GeoCode) over a 5-year period};
#'    \item{\code{ASR} - }{Table presenting the number of cases and rates by Member State (GeoCode) over a 5-year period,
#'    including age-standardised rates for the most recent year.}
#' }
#'
#'
#' @param x dataframe, raw disease-specific dataset (see specification of the dataset in the
#' package vignette with \code{browseVignettes(package = "EpiReport")})
#' (default \code{SALM2016})
#' @param disease character string, disease code (default \code{"SALM"}).
#' Please make sure the disease code is included in the disease-specific dataset x
#' in the \code{HealthTopicCode} variable.
#' @param year numeric, year to produce the table for (default \code{2016}).
#' Please make sure the year is included in the disease-specific dataset x in the \code{TimeCode} variable.
#' @param reportParameters dataframe, dataset including the required parameters for the report
#' production (default \code{AERparams}) (see specification of the dataset in the
#' package vignette with \code{browseVignettes(package = "EpiReport")})
#' @param MSCode dataframe, correspondence table of GeoCode names and codes
#' (default \code{MSCode}) (see specification of the dataset in the
#' package vignette with \code{browseVignettes(package = "EpiReport")})
#' @param index integer, figure number
#' @param doc 'Word' document (see \code{officer} package) in which to add the table
#' at the bookmark location.
#' If doc is missing, \code{getTable} returns the \code{flextable} table object.
#'
#' @return 'Word' doc or \code{flextable} object (see \code{'flextable'} package)
#'
#' @seealso Global function for the full epidemilogical report: \code{\link{getAER}}  \cr
#' Required Packages: \code{\link{flextable}} \code{\link{officer}} \cr
#' Internal functions: \code{\link{shapeECDCFlexTable}} \code{\link{cleanECDCTable}} \cr
#' Default datasets: \code{\link{AERparams}} \code{\link{MSCode}}
#'
#' @examples
#' # --- Draft the table using the default Salmonellosis dataset
#' getTableByMS()
#'
#' @export
#'
getTableByMS <- function(x = EpiReport::SALM2016 ,
                         disease = "SALM",
                         year = 2016,
                         reportParameters = EpiReport::AERparams,
                         MSCode = EpiReport::MSCode,
                         index = 1,
                         doc){

  ## ----
  ## Setting default arguments if missing
  ## ----

  if(missing(x)) { x <- EpiReport::SALM2016 }
  if(missing(disease)) { disease <- "SALM" }
  if(missing(year)) { year <- 2016 }
  if(missing(reportParameters)) { reportParameters <- EpiReport::AERparams }
  if(missing(MSCode)) { MSCode <- EpiReport::MSCode }
  if(missing(index)) { index <- 1 }


  ## ----
  ## Preparing the data
  ## ----

  x$MeasureCode <- cleanMeasureCode(x$MeasureCode)


  ## ----
  ## Filtering parameter table
  ## ----

  reportParameters <- filterDisease(disease, reportParameters)


  ## ----
  ## Filtering data
  ## ----

  # --- Filtering on the required variables
  x <- dplyr::select(x, c("HealthTopicCode", "MeasureCode", "TimeUnit",
                          "TimeCode", "GeoCode", "YValue"))
  if(nrow(x) == 0) {
    stop(paste('The dataset does not include the necessary variables.'))
  }

  # --- Filtering on the disease of interest
  x <- dplyr::filter(x, x$HealthTopic == disease)
  if(nrow(x) == 0) {
    stop(paste('The dataset does not include the selected disease "', disease, '".'))
  }

  # --- Filtering on Yearly data only
  x <- dplyr::filter(x, x$TimeUnit == "Y")
  if(nrow(x) == 0) {
    stop(paste('The dataset does not include the required time unit \'Y\' for the selected disease "',
               disease, '".'))
  }

  # --- Filtering on 5-year period
  x <- dplyr::filter(x, x$TimeCode %in% (year-4):year)
  if(nrow(x) == 0) {
    stop(paste('The dataset does not include the required 5-year study period for the selected disease "',
               disease, '".'))
  }

  # --- Filtering for analysis at country and EU-EEA level only, no EU level
  x <- dplyr::filter(x, x$GeoCode %in% MSCode$GeoCode)
  if(nrow(x) == 0) {
    stop(paste('The dataset does not include any \'GeoCode\' from the MSCode dataset for the selected disease "',
               disease, '".'))
  }



  ## ------------------
  ## Building the Table
  ## ------------------


  # ----
  # Opt 1: ASR table
  # ----

  if(reportParameters$TableUse == "ASR") {

    # --- Filtering
    x <- dplyr::filter(x, x$MeasureCode %in% paste(reportParameters$MeasurePopulation,
                                                   c("COUNT", "RATE", "AGESTANDARDISED.RATE") , sep = "."))
    # --- Filtering ASR only for the year of interest
    x <- dplyr::filter(x, !(x$TimeCode != year &
                              x$MeasureCode %in% paste(reportParameters$MeasurePopulation,
                                                       "AGESTANDARDISED.RATE" , sep = ".")))
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the required \'MeasureCode\' indicator for the selected disease "',
                 disease, '" to present the AER table with ASR.'))
    }

    # --- Rounding rates
    x$YValue <- round(x$YValue, reportParameters$TableRatesNoDecimals)

    # --- Building the table
    x <- dplyr::select(x, c("GeoCode", "TimeCode", "MeasureCode", "YValue"))
    x <- tidyr::unite(x, col = "Key", "TimeCode", "MeasureCode")
    x <- tidyr::spread(x, "Key", "YValue")

    # --- Reordering and rounding columns
    lastColumn <- paste(year, "_", reportParameters$MeasurePopulation,
                        ".AGESTANDARDISED.RATE", sep = "")
    asrColumn <- dplyr::select(x, lastColumn)
    asrColumn <- round(asrColumn, reportParameters$TableASRNoDecimals)
    x <- dplyr::bind_cols(dplyr::select(x, -lastColumn),
                          asrColumn)

    # --- Cleaning table
    x <- cleanECDCTable(x, MSCode$Country, MSCode$GeoCode)

    # --- Preparing headers
    names(x) <- make.names(names(x))    #FlexTable supports only syntactic names
    headers <- data.frame(
      col_keys = names(x),
      years = c("Country", rep((year-4):year, each = 2), year),
      indicator = c("Country", rep(c("Number", "Rate"), 5), "ASR"),
      stringsAsFactors = FALSE
    )

  }


  # ----
  # Opt 2: Rates table only
  # ---

  if(reportParameters$TableUse == "RATE") {

    # --- Filtering
    x <- dplyr::filter(x, x$MeasureCode %in% paste(reportParameters$MeasurePopulation,
                                                   c("COUNT", "RATE") , sep = "."))
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the required \'MeasureCode\' indicator for the selected disease "',
                 disease, '" to present the AER table with RATES.'))
    }

    # --- Rounding rates
    x$YValue <- round(x$YValue, reportParameters$TableRatesNoDecimals)

    # --- Building the table
    x <- dplyr::select(x, c("GeoCode", "TimeCode", "MeasureCode", "YValue"))
    x <- tidyr::unite(x, col = "Key", "TimeCode", "MeasureCode")
    x <- tidyr::spread(x, "Key", "YValue")

    # --- Cleaning table
    x <- cleanECDCTable(x, MSCode$Country, MSCode$GeoCode)

    # --- Preparing headers
    names(x) <- make.names(names(x))    #FlexTable supports only syntactic names
    headers <- data.frame(
      col_keys = names(x),
      years = c("Country", rep((year-4):year, each = 2)),
      indicator = c("Country", rep(c("Number", "Rate"), 5)),
      stringsAsFactors = FALSE
    )

  }


  # ----
  # Opt 3: Reported cases table only
  # ----

  if(reportParameters$TableUse == "COUNT") {
    # --- Filtering
    x <- dplyr::filter(x, x$MeasureCode %in% paste(reportParameters$MeasurePopulation,
                                                   "COUNT" , sep="."))
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the required \'MeasureCode\' indicator for the selected disease "',
                 disease, '" to present the AER table with COUNTS.'))
    }

    # --- Building the table
    x <- dplyr::select(x, c("GeoCode", "TimeCode", "MeasureCode", "YValue"))
    x <- tidyr::unite(x, col = "Key", "TimeCode", "MeasureCode")
    x <- tidyr::spread(x, "Key", "YValue")

    # --- Cleaning table
    x <- cleanECDCTable(x, MSCode$Country, MSCode$GeoCode)

    # --- Preparing headers
    names(x) <- make.names(names(x))    #FlexTable supports only syntactic names
    headers <- data.frame(
      col_keys = names(x),
      years = c("Country", (year-4):year),
      indicator = c("Country", rep("Number", 5)),
      stringsAsFactors = FALSE
    )

  }


  # ----
  # No Table
  # ----

  if(reportParameters$TableUse == "NO") {
    return(doc)
  }


  # ----
  # Specific Tables (to be continued)
  # ----

  if(reportParameters$TableUse == "SPECIFIC") {

    # --- Filtering
    x <- dplyr::filter(x, x$MeasureCode %in% paste(c("CONFIRMED", "ACUTE", "CHRONIC", "UNKNOWN"),
                                                   rep(c("COUNT", "RATE"), each = 4) , sep = "."))
    # --- Filtering ASR only for the year of interest
    x <- dplyr::filter(x, !(x$TimeCode != year &
                              x$MeasureCode %in% paste(c("ACUTE", "CHRONIC", "UNKNOWN"),
                                                       rep(c("COUNT", "RATE"), each = 3) , sep = ".")))
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the required \'MeasureCode\' indicator for the selected disease "',
                 disease, '" to present the AER table by disease stage'))
    }

    # --- Rounding rates
    x$YValue <- round(x$YValue, reportParameters$TableRatesNoDecimals)

    # --- Building the table
    x <- dplyr::select(x, c("GeoCode", "TimeCode", "MeasureCode", "YValue"))
    x <- tidyr::unite(x, col = "Key", "TimeCode", "MeasureCode")
    x <- tidyr::spread(x, "Key", "YValue")

    # --- Reordering and rounding columns
    lastColumn <- paste(year, "_", paste(rep(c("CONFIRMED","ACUTE", "CHRONIC", "UNKNOWN"), each = 2),
                                         c("COUNT", "RATE"), sep = "."), sep = "")
    stageColumn <- dplyr::select(x, lastColumn)
    x <- dplyr::bind_cols(dplyr::select(x, -lastColumn),
                          stageColumn)

    # --- Cleaning table
    x <- cleanECDCTable(x, MSCode$Country, MSCode$GeoCode)

    # --- Preparing headers
    names(x) <- make.names(names(x))    #FlexTable supports only syntactic names
    headers <- data.frame(
      col_keys = names(x),
      years = c("Country", rep((year-4):year, each = 2), rep(year, 6)),
      stage = c("Country", rep("Confirmed", 10), rep( c("Acute", "Chronic", "Unknown") , each = 2)),
      indicator = c("Country", rep(c("Cases", "Rate"), 8)),
      stringsAsFactors = FALSE
    )
  }

  # ----
  # Table Layout
  # ----

  ft <- flextable::flextable(x)
  ft <- shapeECDCFlexTable(ft = ft, headers = headers)


  # ----
  # Final Output
  # ----

  if(missing(doc)) {
    # --- If no 'Word' document, then return the flextable
    return(ft)
  } else {
    # --- If there is a 'Word' document, then replace the corresponding bookmark
    officer::cursor_bookmark(doc, id = "TABLE1_BOOKMARK")
    doc <- flextable::body_add_flextable(doc, value = ft)


    # ----
    # Adding the caption
    # ----

    ## ------ Caption definition
    pop <- ifelse(reportParameters$MeasurePopulation == "ALL", "", "-")
    pop <- ifelse(reportParameters$MeasurePopulation == "CONFIRMED", "confirmed ", pop)
    caption <- paste("Table 1. Distribution of ", pop, reportParameters$Label,
                     " cases, ", "EU/EEA, ", year - 4, "\U2013", year, sep = "")
    officer::cursor_bookmark(doc, id = "TABLE1_CAPTION")
    doc <- officer::body_add_par(doc, value = caption)

    return(doc)

  }

}





#' Shaping the final table (layout, title, color, font)
#'
#' Shaping the final table including titles, adding background color, specifying font name and size.
#'
#' @param ft flextable (see \code{'flextable'} package), table to shape into ECDC table layout
#' @param headers dataframe including the multiple headers to add to the flextable object.
#' Please note that the column \code{col_keys} should contain the names of the flextable object
#' (i.e. \code{col_key = names(x)}), accordingly to \code{\link{set_header_df}}.
#' @param fsize numeric, font to use (Default 7)
#' @param fname character, font name (Default \code{"Tahoma"})
#' @param maincolor character string, hexadecimal code for the header background
#' color (Default \code{EcdcColors(col_scale = "green", n=1)})
#'
#' @return flextable object (see \code{flextable} package)
#'
#' @seealso Global function: \code{\link{getTableByMS}} \cr
#' Required package \code{\link{flextable}}
#'
#' @export
#'
shapeECDCFlexTable <- function(ft, headers, fsize, fname, maincolor){

  ## ----
  ## Setting default arguments if missing
  ## ----

  if(missing(fsize)) {fsize <- 7}
  if(missing(fname)) {fname <- "Tahoma"}
  if(missing(maincolor)) {maincolor <- EcdcColors(col_scale = "green", n=1)}



  ## ----
  ## Shaping the table
  ## ----

  # --- Borders
  ft <- flextable::border_remove(ft)
  std_border <- officer::fp_border(color = EcdcColors(col_scale = "grey", grey_shade = "mediumlight"))
  ft <- flextable::hline(ft, border = std_border)
  # --- Headers
  ft <- flextable::set_header_df(ft, mapping = headers, key = "col_keys" )
  ft <- flextable::merge_h(ft, i = 1, part = "header")
  for(col in seq(2, ncol(ft$header$dataset), by=2)) {
    if(col+1 <= ncol(ft$header$dataset)) {
      if(ft$header$dataset[2, col] == ft$header$dataset[2, col+1]){
        ft <- flextable::merge_at(ft, i = 2, j = c(col, col+1), part = "header")
      }
    }
  }
  ft <- flextable::merge_v(ft, j = "Country", part = "header")
  # --- Headers Borders
  hd_border <- officer::fp_border(color = "white")
  ft <- flextable::border_inner_v(ft, border = hd_border, part = "header")
  ft <- flextable::border_inner_h(ft, border = hd_border, part = "header")
  # --- Colors
  ft <- flextable::bg(ft, bg = maincolor, part = "header")
  ft <- flextable::color(ft, color = "white", part = "header")
  ft <- flextable::bold(ft, part = "header")
  # --- Font
  ft <- flextable::fontsize(ft, size = fsize, part = "all")
  ft <- flextable::font(ft, fontname = fname, part = "all")
  # --- Alignement
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::align(ft, align = "left", j = 1)
  # --- EUEEA bold
  ft <- flextable::bold(ft, i = nrow(ft$body$dataset))
  # --- Autofit
  ft <- flextable::autofit(ft)

  return(ft)

}




#' Cleaning the final table
#'
#' Cleaning the final table: identifying missing reports with \code{'-'},
#' replacing the Member State codes with Member State names (see correspondence
#' table \code{\link{MSCode}}), identifying not reporting Member States with \code{'.'}
#'
#' @param x dataframe, dataset to clean
#' @param Country character vector, full names of the countries /
#' Member States (e.g. Austria, Belgium, etc.) that will replace the GeoCodes
#' included the x dataframe (Default \code{MSCode$Country})
#' @param GeoCode character vector, corresponding GeoCode of each Member State
#' (e.g. AT, BE, etc.) to replace with the country full names (Default \code{MSCode$GeoCode})
#'
#' @return cleaned ECDC dataframe
#'
#' @seealso Global function: \code{\link{getTableByMS}} \cr
#' Default dataset \code{\link{MSCode}}
#'
#' @export
#'
cleanECDCTable <- function(x,
                           Country = EpiReport::MSCode$Country,
                           GeoCode = EpiReport::MSCode$GeoCode){

  ## ----
  ## Setting default arguments if missing
  ## ----

  if(missing(Country)) {
    Country <- EpiReport::MSCode$Country
  }

  if(missing(GeoCode)) {
    GeoCode <- EpiReport::MSCode$GeoCode
  }



  ## ----
  ## Cleaning the table
  ## ----

  # --- Identifying missing reports
  x <- as.matrix(x)             # converting into matrix
  miss <- which(is.na(x) == TRUE)         # get index of NA values
  x[miss] <- "-"
  x <- as.data.frame(x, stringsAsFactors = FALSE)

  # --- Adding Country names
  corresp <- dplyr::bind_cols(Country = Country, GeoCode = GeoCode)
  x <- dplyr::full_join(corresp, x, by = "GeoCode")
  x <- dplyr::select(x, -"GeoCode")

  # --- Identifying missing countries
  x <- as.matrix(x)             # converting into matrix
  miss <- which(is.na(x) == TRUE)         # get index of NA values
  x[miss] <- "."
  x <- as.data.frame(x, stringsAsFactors = FALSE)

  return(x)
}


