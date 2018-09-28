#' Get the trend of reported/confirmed cases graph
#'
#' Function returning the plot
#'
#' @param x dataset
#' @param disease character string, disease name
#' @param year numeric, year to produce the report for
#' @param reportParameters dataset of parameters for the report
#' @param MSCode dataset containing the correspondence table of geo code and labels
#' @param index figure number
#' @param doc Word document
#' @return Word doc a ggplot2 preview
#' @export
getTrend <- function(x, #to improve with variables??
                     #HealthTopic MeasureCode TimeUnit TimeCode GeoCode N
                      disease = "SALM", year = 2016,
                      reportParameters, MSCode, index = 1, doc){

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
  ## Filtering parameter table
  ## ----

  reportParameters <- filterDisease(disease, reportParameters)


  ## ----
  ## Trend plot
  ## ----

  if(reportParameters$TSTrendGraphUse == "Y") {

    ## ----
    ## Filtering data
    ## ----

    # --- Filtering on the disease of interest
    x <- dplyr::select(x, c("HealthTopicCode", "MeasureCode", "TimeUnit", "TimeCode", "GeoCode", "N"))
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the necessary variables.'))
    }

    # --- Filtering on the disease of interest
    x <- dplyr::filter(x, x$HealthTopicCode == disease)
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the selected disease "', disease, '".'))
    }

    # --- Filtering on Monthly data only
    x <- dplyr::filter(x, x$TimeUnit == "M")
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the required time unit \'M\' for the selected disease "', disease, '".'))
    }

    # --- Filtering on 5 year period monthly data
    studyPeriodYear <- (year-4):year
    studyPeriodMonth <- sub(" ", "0", format(1:12, width = 2))
    studyPeriod <- paste(rep(studyPeriodYear, each = 12),
                         rep(studyPeriodMonth, times = 5), sep="-")
    x <- dplyr::filter(x, x$TimeCode %in% studyPeriod)
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the required 5-year study period for the selected disease "', disease, '".'))
    }

    # --- Filtering for analysis at country and EU-EEA level only, no EU level
    MS <- MSCode$GeoCode[!is.na(MSCode$EUEEA)]
    x <- dplyr::filter(x, x$GeoCode %in% MS)
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include any \'GeoCode\' at EUEEA level (see MSCode dataset) for the selected disease "', disease, '".'))
    }

    # --- Filtering on MeasureCode indicators
    x <- dplyr::filter(x, x$MeasureCode %in%
                         paste(reportParameters$MeasurePopulation, "COUNT", sep="."))



    ## ----
    ## Building the time series for all countries with no gap
    ## ----

    tsFill <- expand.grid(TimeCode = studyPeriod, GeoCode = MS, stringsAsFactors = FALSE)

    # --- Merging to identify and exclude gaps in the data
    missingTimePoint <- dplyr::setdiff(tsFill, x[,names(tsFill)])
    x <- dplyr::filter(x, !(x$GeoCode %in% unique(missingTimePoint$GeoCode)))

    # --- Computing EUEEA level
    N <- TimeCode <- NULL
    eueea <- dplyr::group_by(x, TimeCode)
    eueea <- dplyr::summarise(eueea, "N" = sum(N, na.rm = TRUE))
    eueea <- dplyr::ungroup(eueea)

    # --- Computing EUEEA Moving average
    m.av <- zoo::rollmean(
      zoo::zoo(eueea$N, eueea$TimeCode),
      12 ,
      fill = list(NA, NA, NA))    #used to be 'fill = list(NA, NULL, NA))' !!!! TO DISCUSS!!

    # --- Building the result dataframe
    eueea <- dplyr::mutate(eueea, MAV = zoo::coredata(m.av))
    eueea <- as.data.frame(eueea)

    # --- Transforming into a complete date
    eueea$TimeCode <- as.Date(paste(eueea$TimeCode,"01", sep = "-"), "%Y-%m-%d")



    ## ----
    ## Plot
    ## ----

    p <- plotTS12MAvg(eueea,
                      xvar = "TimeCode",
                      yvar = "N",
                      movAverage = "MAV")


    if(missing(doc)) {
      return(p)
    } else {

      ## ------ Caption
      pop <- ifelse(reportParameters$MeasurePopulation == "ALL", "", "-")
      pop <- ifelse(reportParameters$MeasurePopulation == "CONFIRMED", "confirmed ", pop)
      caption <- paste("Figure ", index, ". Trend and number of ", pop,
                       reportParameters$Label, " cases, EU/EEA by month, ",
                       year-4, "\U2013", year, sep = "")
      officer::cursor_bookmark(doc, id = "TS_TREND_BOOKMARK")
      doc <- officer::body_add_par(doc,
                                   value = caption)

      ## ------ Plot
      doc <- officer::body_add_gg(doc,
                                  value = p,
                                  width = 6,
                                  height = 3)
    }
  }


  ## ----
  ## No trend plot for this disease
  ## ----

  if(reportParameters$TSTrendGraphUse == "N") {
    message(paste('According to the parameter table \'AERparams\', this disease "',
                  disease, '" does not include any trend graph in the AER report.', sep = ""))
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




#' AER time series with 12-month moving average
#'
#' This function draws a line graph with 12-month moving average, AER style.
#' Expects aggregated data and pre-calculated 12-month moving average.
#' @param data Your data frame.
#' @param xvar Time variable, given in quotes.
#' @param yvar A variable with the number of cases for each time unit, given in quotes.
#' @param movAverage A variable with the moving average per each time unit.
#' @keywords moving average
#' @export
plotTS12MAvg <- function(data,
                         xvar = "TimeCode",
                         yvar = "N",
                         movAverage = "MAV"){


  # xvar <- deparse(substitute(xvar))
  # yvar <- deparse(substitute(yvar))
  # movAverage <-deparse(substitute(movAverage))


  # --- Breaks for the Y axis
  FIGTSBREAKS <- pretty(seq(0,
                            max(data[[yvar]]),
                            by = max(data[[yvar]])/7))


  # --- Plotting

  p <- ggplot2::ggplot(data,
                       ggplot2::aes(data[[xvar]])) +
    ggplot2::geom_line(
      ggplot2::aes(y = data[[yvar]] , color = "Number of cases"), size = 0.6) +
    ggplot2::geom_line(
      ggplot2::aes(y = data[[movAverage]], color = "12-month moving average"), size = 1.1, na.rm = TRUE) +
    ggplot2::scale_x_date(
      date_label = "%b \n %Y",
      date_breaks = "6 months",
      expand = c(0, 0)) +
    ggplot2::scale_y_continuous(
      breaks = FIGTSBREAKS,
      limits = c(0, max(FIGTSBREAKS)),
      expand = c(0, 0)) +
    ggplot2::xlab("Month") +
    ggplot2::ylab("Number of cases") +
    ggplot2::scale_colour_manual(
      "lines",
      values=c("Number of cases" = "#767171", "12-month moving average"= "#69AE23")) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      axis.title = ggplot2::element_text(size = 9),
      axis.line = ggplot2::element_line(colour = "#767171"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.position = "right",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 8),
      legend.key = ggplot2::element_blank(),
      legend.key.width = ggplot2::unit(0.8, "cm")) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE),
                    colour = ggplot2::guide_legend(reverse = TRUE))

  return(p)
}





