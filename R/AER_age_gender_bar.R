#' Get the age and gender bar graph
#'
#' Function returning the plot
#'
#' @param x dataset
#' @param disease character string, disease name
#' @param year numeric, year to produce the report for
#' @param reportParameters dataset of parameters for the report
#' @param geoCode character string, geoCode to run the analysis on (default: EU_EEA31)
#' @param index figure number
#' @param doc Word document
#' @return Word doc a ggplot2 preview
#' @export
getAgeGender <- function(x, #to improve with variables??
                         #HealthTopic MeasureCode TimeUnit TimeCode GeoCode N
                         disease = "SALM", year = 2016,
                         reportParameters, geoCode = "EU_EEA31", index = 1, doc){

  ## ----
  ## Setting default arguments if missing
  ## ----

  if(missing(x)) { x <- EpiReport::SALM2016 }
  if(missing(disease)) { disease <- "SALM" }
  if(missing(year)) { year <- 2016 }
  if(missing(reportParameters)) { reportParameters <- EpiReport::AERparams }
  if(missing(geoCode)) { geoCode <- "EU_EEA31" }
  if(missing(index)) { index <- 1 }


  ## ----
  ## Filtering parameter table
  ## ----

  reportParameters <- filterDisease(disease, reportParameters)


  ## ----
  ## Age Gender bar graph
  ## ----

  if(reportParameters$AgeGenderBarGraphUse %in% c("Y", "P")) {


    ## ----
    ## Filtering data
    ## ----

    # --- Filtering on the disease of interest
    x <- dplyr::select(x, c("HealthTopicCode", "MeasureCode", "TimeUnit", "TimeCode", "GeoCode",
                            "XValue", "XLabel", "YValue", "YLabel", "ZValue"))
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the necessary variables.'))
    }

    # --- Filtering on the disease of interest
    x <- dplyr::filter(x, x$HealthTopicCode == disease)
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the selected disease "', disease, '".'))
    }

    # --- Filtering on Yearly data only
    x <- dplyr::filter(x, x$TimeUnit == "Y")
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the required time unit \'Y\' for the selected disease "', disease, '".'))
    }

    # --- Filtering on the year of interest
    x <- dplyr::filter(x, x$TimeCode == year)
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the year of interest "', year
                 ,'" for the selected disease "', disease, '".'))
    }

    # --- Filtering on the GeoCode of interest
    x <- dplyr::filter(x, x$GeoCode == geoCode)
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the geoCode of interest "', geoCode
                 ,'" for the selected disease "', disease, '".'))
    }


    if(reportParameters$AgeGenderRatesUse == "Y") {

      # --- Filtering on MeasureCode indicators
      x <- dplyr::filter(x, x$MeasureCode %in%
                           paste(reportParameters$MeasurePopulation, "AGE_GENDER.RATE", sep="."))
      if(nrow(x) == 0) {
        stop(paste('The dataset does not include the required MeasureCode "',
                   paste(reportParameters$MeasurePopulation, "AGE_GENDER.RATE", sep=".")
                   ,'" for the selected disease "', disease, '".'))
      }
    }


    ## ------------
    ## Proportion Graph
    ## ------------

    if(reportParameters$AgeGenderBarGraphUse == "P") {

      # --- Filtering on MeasureCode indicators
      x <- dplyr::filter(x, x$MeasureCode %in%
                           paste(reportParameters$MeasurePopulation, "AGE_GENDER.PROPORTION", sep="."))
      if(nrow(x) == 0) {
        stop(paste('The dataset does not include the required MeasureCode "',
                   paste(reportParameters$MeasurePopulation, "AGE_GENDER.PROPORTION", sep=".")
                   ,'" for the selected disease "', disease, '".'))
      }

    }


    ## ------------
    ## WNV Graph
    ## ------------

    if(reportParameters$AgeBarGraphUse == "Y") {

      # --- Filtering on MeasureCode indicators
      x <- dplyr::filter(x, x$MeasureCode %in%
                           paste(reportParameters$MeasurePopulation, "DOMESTIC.AGE.RATE", sep="."))
      if(nrow(x) == 0) {
        stop(paste('The dataset does not include the required MeasureCode "',
                   paste(reportParameters$MeasurePopulation, "DOMESTIC.AGE.RATE", sep=".")
                   ,'" for the selected disease "', disease, '".'))
      }

    }


    ## ----
    ## Ordering the labels for gender variable
    ## ----

    x$YLabel = factor(x$YLabel, c("Male","Female"))
    x$XLabel = factor(x$XLabel, order_quasinum(unique(x$XLabel)))


    ## ----
    ## Plot
    ## ----

    p <- plotAgeGender(x,
                       xvar = "XLabel",
                       yvar = "ZValue",
                       group = "YLabel",
                       fill_color1 = "#65B32E",
                       fill_color2 = "#7CBDC4",
                       ytitle  = toCapTitle(tolower(reportParameters$AgeGenderBarGraphLabel)))

    if(missing(doc)) {
      return(p)
    } else {

      ## ------ Caption
      pop <- ifelse(reportParameters$MeasurePopulation == "ALL", "", "-")
      pop <- ifelse(reportParameters$MeasurePopulation == "CONFIRMED", "confirmed ", pop)
      caption <- paste("Figure ", index, ". Distribution of ", pop,
                       reportParameters$Label, " ",
                       tolower(reportParameters$AgeGenderBarGraphLabel),
                       ", by age and gender, EU/EEA, ",
                       year, sep = "")
      officer::cursor_bookmark(doc, id = "BARGPH_AGEGENDER_BOOKMARK")
      doc <- officer::body_add_par(doc,
                                   value = caption)

      ## ------ Plot
      doc <- officer::body_add_gg(doc,
                                  value = p,
                                  width = 6,
                                  height = 4)
    }
  }




  ## ----
  ## No AgeGender bar graph for this disease
  ## ----

  if(reportParameters$AgeGenderBarGraphUse == "N") {
    message(paste('According to the parameter table \'AERparams\', this disease "',
                  disease, '" does not include any age and gender bar graph in the AER report.', sep = ""))
    if(missing(doc)) {
      return()
    } else {
      return(doc)
    }
  }


  ## ----
  ## Final output
  ## ----

  if(missing(doc)) {
    return(p)
  }else{
    return(doc)
  }



}





#' AER age-gender bar
#'
#' This function draws a barchart by age group and gender(or possibly other grouping), AER style.
#' Expects aggregated data.
#' @param data Your data frame
#' @param xvar Variable on the x-axis in quotes; e.g. age group
#' @param yvar Variable on the y-axis in quotes with the rate or number of cases
#' @param fill_color1 Bar 1 colour; defaults to ECDC green "#65B32E"
#' @param fill_color2 Bar 2 colour; defaults to light blue "#7CBDC1"
#' @param group A grouping variable in quotes, e.g. gender as in the AER.
#' @param ytitle y-axis title; defaults to "Rate".
#' @keywords AER_age_gender_bar
#' @export
#' @examples
#' # --- Create dummy data
#' mydat <- data.frame(Gender=c("F", "F", "M", "M"),
#' AgeGroup = c("0-65", "65+", "0-65", "65+"),
#' NumberOfCases = c(54,43,32,41))
#'
#' # --- Plot the dummy data
#' plotAgeGender(mydat,
#'               xvar = "AgeGroup",
#'               yvar = "NumberOfCases",
#'               group = "Gender",
#'               ytitle = "Number of cases")
#' @export
plotAgeGender <- function(data,
                          xvar = "XLabel",
                          yvar = "ZValue",
                          group = "YLabel",
                          fill_color1 = "#65B32E",
                          fill_color2 = "#7CBDC4",
                          ytitle  = "Rate") {

  # xvar <-deparse(substitute(xvar))
  # yvar <-deparse(substitute(yvar))
  # group <-deparse(substitute(group))

  FIGBREAKS <- pretty(seq(0,
                          max(data[[yvar]]),
                          by = max(data[[yvar]])/5))

  p <- ggplot2::ggplot(data = data,
                       ggplot2::aes(x = data[[xvar]], y = data[[yvar]], fill = data[[group]])) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::scale_fill_manual(values = c(fill_color1, fill_color2)) +
    ggplot2::scale_y_continuous(expand = c(0,0),
                                limits = c(0, max(FIGBREAKS)),
                                breaks = FIGBREAKS) +
    ggplot2::labs(title = "", x = "Age", y = ytitle) +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 8),
                   axis.title = ggplot2::element_text(size = 9),
                   axis.line = ggplot2::element_line(colour = "black"),
                   axis.line.x = ggplot2::element_blank(),
                   # --- Setting the background
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   # --- Setting the legend
                   legend.position = "right",
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size=8),
                   legend.key.width = ggplot2::unit(0.8, "cm"),
                   legend.key.size = ggplot2::unit(0.4, "cm"))

  return(p)
}

