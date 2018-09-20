#' Get the table
#'
#' Function returning the table
#'
#' @param x dataset including required data for AER
#' @param disease character string, disease name
#' @param year numeric, year to produce the report for
#' @param reportParameters dataset of parameters for the report
#' @param MSCode dataset containing the correspondence table of geo code and labels
#' @param index figure number
#' @param doc Word document (see \code{officer} package)
#' @return ?
#' @export
getTableByMS <- function(x,     #input to improve to have variables instead of dataframe
                         disease = "Salmonellosis", year = 2016,
                         reportParameters, MSCode, index = 1, doc){

  ## ----
  ## Setting default arguments if missing
  ## ----

  if(missing(x)) { x <- EpiReport::SALM2016 }
  if(missing(disease)) { disease <- "SALM" }    # disease <- "SALM"
  if(missing(year)) { year <- 2016 }
  if(missing(reportParameters)) { reportParameters <- EpiReport::AERparams }
  if(missing(MSCode)) { MSCode <- EpiReport::MSCode }
  if(missing(index)) { index <- 1 }


  ## ----
  ## Filtering
  ## ----

  # Filtering on the disease of interest
  x <- dplyr::filter(x, x$HealthTopic == disease)
  # Filtering on Yearly data only
  x <- dplyr::filter(x, x$TimeUnit == "Y")
  # Filtering on 5 year period
  x <- dplyr::filter(x, x$TimeCode %in% (year-4):year)
  # Filtering for analysis at country and EU-EEA level only, no EU level
  x <- dplyr::filter(x, x$GeoCode != "EU28")

  if(nrow(x) == 0) {
    stop(paste('The dataset does not include the selected disease "', disease, '".'))
  }


  reportParameters <- dplyr::filter(reportParameters, reportParameters$HealthTopic == disease)
  if( nrow(reportParameters) ==0 ) {
    stop(paste('The disease "', disease, '" is not described in the parameter table.
               The report cannot be produced.'))
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
                                                     c("COUNT", "RATE", "AGESTANDARDISED.RATE") , sep="."))
    # --- Filtering ASR only for the year of interest
    x <- dplyr::filter(x, !(x$TimeCode != year &
                              x$MeasureCode %in% paste(reportParameters$MeasurePopulation,
                                                         "AGESTANDARDISED.RATE" , sep=".")))
    # --- Rounding rates
    x$YValue <- round(x$YValue, reportParameters$TableRatesNoDecimals)

    # --- Building the table
    x <- dplyr::select(x, c("GeoCode", "TimeCode", "MeasureCode", "YValue"))
    x <- tidyr::unite(x, col = "Key", "TimeCode", "MeasureCode")
    x <- tidyr::spread(x, "Key", "YValue")

    # --- Reordering and rounding columns
    lastColumn <- paste(year, "_", reportParameters$MeasurePopulation,".AGESTANDARDISED.RATE", sep="")
    asrColumn <- dplyr::select(x, lastColumn)
    asrColumn <- round(asrColumn, reportParameters$TableASRNoDecimals)
    x <- dplyr::bind_cols(dplyr::select(x, -lastColumn),
                          asrColumn)

    # --- Cleaning table
    x <- cleaningECDCTable(x, MSCode)

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
                                                   c("COUNT", "RATE") , sep="."))
    # --- Rounding rates
    x$YValue <- round(x$YValue, reportParameters$TableRatesNoDecimals)

    # --- Building the table
    x <- dplyr::select(x, c("GeoCode", "TimeCode", "MeasureCode", "YValue"))
    x <- tidyr::unite(x, col = "Key", "TimeCode", "MeasureCode")
    x <- tidyr::spread(x, "Key", "YValue")

    # --- Cleaning table
    x <- cleaningECDCTable(x, MSCode)

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

    # --- Building the table
    x <- dplyr::select(x, c("GeoCode", "TimeCode", "MeasureCode", "YValue"))
    x <- tidyr::unite(x, col = "Key", "TimeCode", "MeasureCode")
    x <- tidyr::spread(x, "Key", "YValue")

    # --- Cleaning table
    x <- cleaningECDCTable(x, MSCode)

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
  # Specific Tables ??
  # ----

  if(reportParameters$TableUse == "SPECIFIC") {
    return(doc)
  }

  # ----
  # Table Layout
  # ----

  ft <- flextable::flextable(x)
  ft <- shapingECDCFlexTable(ft = ft, headers = headers)


  # ----
  # Final Output
  # ----

  if(missing(doc)) {
    # --- If no Word document, then just preview the map
    return(ft)
  } else {
    # --- If there is a Word document, then replace the corresponding bookmark
    officer::cursor_bookmark(doc, id = "TABLE1_BOOKMARK")
    doc <- flextable::body_add_flextable(doc, value = ft)


    # ----
    # Adding the caption
    # ----

    ## ------ Caption definition
    pop <- ifelse(reportParameters$MeasurePopulation == "ALL", "", "-")
    pop <- ifelse(reportParameters$MeasurePopulation == "CONFIRMED", "confirmed ", pop)
    caption <- paste("Table 1. Distribution of ", pop, reportParameters$Label,
                     " cases, ", "EU/EEA, ", year-4, "\U2013", year, sep = "")
    officer::cursor_bookmark(doc, id = "TABLE1_CAPTION")
    doc <- officer::body_add_par(doc, value = caption)

    return(doc)

  }

}




#' Shaping the final table
#'
#' Shaping the final table (layout)
#'
#' @param ft fletable to shape into ECDC table layout
#' @param headers dataframe including the multiple headers to add to the flextable
#' (n.b. the first column should contain the names of the flextable object)
#' @param fsize numeric, font to use (defaut = 7)
#' @param fname character, font name (defaut = "Tahoma")
#' @param maincolor character string, hexadecial code for the header background color
#' @return flextable object
#' @export
shapingECDCFlexTable <- function(ft, headers, fsize, fname, maincolor){

  ## ----
  ## Setting default arguments if missing
  ## ----

  if(missing(fsize)) {fsize <- 7}
  if(missing(fname)) {fname <- "Tahoma"}
  if(missing(maincolor)) {maincolor <- "#69AE23"}



  ## ----
  ## Shaping the table
  ## ----

  # --- Borders
  ft <- flextable::border_remove(ft)
  std_border <- officer::fp_border(color = "grey80")
  ft <- flextable::hline(ft, border = std_border)
  # --- Headers
  ft <- flextable::set_header_df(ft, mapping = headers, key = "col_keys" )
  ft <- flextable::merge_h(ft, i = 1, part = "header")
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
#' Cleaning the final table: identifying missing reports with '-',
#' replacing the Member State codes with Member State names,
#' identifying not reporting Member States with '.'
#'
#' @param x dataset including required data for AER
#' @param MSCode dataset containing the correspondence table of geo code and labels
#' @return cleaned ECDC table
#' @export
cleaningECDCTable <- function(x, MSCode){

  ## ----
  ## Setting default arguments if missing
  ## ----

  if(missing(MSCode)) {MSCode <- EpiReport::MSCode}



  ## ----
  ## Cleaning the table
  ## ----

  # --- Identifying missing reports
  x <- as.matrix(x)             # converting into matrix
  miss <- which(is.na(x) == TRUE)         # get index of NA values
  x[miss] <- "-"
  x <- as.data.frame(x, stringsAsFactors = FALSE)

  # --- Adding Country names
  x <- dplyr::full_join(MSCode, x, by = "GeoCode")
  x <- dplyr::select(x, -"GeoCode")

  # --- Identifying missing countries
  x <- as.matrix(x)             # converting into matrix
  miss <- which(is.na(x) == TRUE)         # get index of NA values
  x[miss] <- "."
  x <- as.data.frame(x, stringsAsFactors = FALSE)

  return(x)
}


