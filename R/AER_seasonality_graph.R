#' Get the seasonality graph
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
getSeason <- function(x, #to improve with variables??
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
  ## Preparing the data
  ## ----
  x$MeasureCode <- cleanMeasureCode(x$MeasureCode)


  ## ----
  ## Filtering parameter table
  ## ----

  reportParameters <- filterDisease(disease, reportParameters)


  ## ----
  ## Seasonal plot
  ## ----

  if(reportParameters$TSSeasonalityGraphUse == "Y") {

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

    # --- Filtering on Yearly data only
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



    ## ----
    ## Building the result table
    ## ----

    # --- Transforming into a complete date
    x$TimeCode <- as.Date(paste(x$TimeCode,"01", sep = "-"), "%Y-%m-%d")

    # --- Creating the year and month variables for further stratification
    x <- dplyr::mutate(x, TimeYear = format(as.Date(x$TimeCode, "%Y-%m-%d"), "%Y"))
    x <- dplyr::mutate(x, TimeMonth = format(as.Date(x$TimeCode, "%Y-%m-%d"), "%m"))

    # --- Computing TS at EUEEA level
    N <- TimeCode <- TimeYear <- TimeMonth <- NULL
    eueea <- dplyr::group_by(x, TimeCode, TimeYear, TimeMonth)
    eueea <- dplyr::summarise(eueea, "N" = sum(N, na.rm = TRUE))
    eueea <- dplyr::ungroup(eueea)

    # --- Compute mean, min and max
    summ <- dplyr::filter(eueea, eueea$TimeYear != year)
    summ <- dplyr::group_by(summ, TimeMonth)
    summ <- dplyr::summarise(summ,
                             Mean4Years = mean(N, na.rm = TRUE),
                             Max4Years=max(N, na.rm = TRUE),
                             Min4Years=min(N, na.rm = TRUE))
    summ <- dplyr::ungroup(summ)

    # --- Finalysing the result table
    agg <- dplyr::filter(eueea, eueea$TimeYear == year)
    agg <- dplyr::inner_join(agg, summ, by = "TimeMonth")



    ## ----
    ## Plot
    ## ----

    p <- plotSeasonality(data = agg,
                                xvar = "TimeCode",
                                yvar = "N",
                                min4years = "Min4Years",
                                max4years = "Max4Years",
                                mean4years = "Mean4Years",
                                year = year)


    if(missing(doc)) {
      return(p)
    } else {

      ## ------ Caption
      pop <- ifelse(reportParameters$MeasurePopulation == "ALL", "", "-")
      pop <- ifelse(reportParameters$MeasurePopulation == "CONFIRMED", "confirmed ", pop)
      caption <- paste("Figure ", index, ". Distribution of ", pop,
                       reportParameters$Label, " cases by month, EU/EEA, ",
                       year, " and ", year-4, "\U2013", year-1, sep = "")
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
  ## No Seasonal plot for this disease
  ## ----

  if(reportParameters$TSSeasonalityGraphUse == "N") {
    message(paste('According to the parameter table \'AERparams\', this disease "',
                  disease, '" does not include any seasonal graph in the AER report.', sep = ""))
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


#' AER
#'
#' This function ...
#' @param data Your data frame
#' @param xvar Variable on the x-axis in quotes; e.g. age group
#' @param yvar Variable on the y-axis in quotes with the rate or number of cases
#' @param min4years var...
#' @param max4years var...
#' @param mean4years var..
#' @param year Var...
#' @keywords seasonality
#' @export
plotSeasonality <- function(data,
                            xvar = "TimeCode",
                            yvar = "N",
                            min4years = "Min4Years",
                            max4years = "Max4Years",
                            mean4years = "Mean4Years",
                            year = 2016){

  # --- Setting breaks for the time series to be nice
  FIGTSBREAKS <- pretty(seq(0,
                            max(data[[max4years]], data[[yvar]]),
                            by = max(data[[max4years]], data[[yvar]])/7))

  p <- ggplot2::ggplot(data, ggplot2::aes(data[[xvar]])) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = data[[min4years]] ,
                   ymax = data[[max4years]],
                   fill = paste("Min-max (", year - 4, "\U2013", year - 1, ")",sep = "")) , alpha = 0.5) +
    ggplot2::geom_line(
      ggplot2::aes(y = data[[mean4years]],
                   color = "Mean"),
      linetype = "longdash", size = 0.6) +
    ggplot2::geom_line(
      ggplot2::aes(y = data[[yvar]], color = "year"), size = 1.1) +
    ggplot2::scale_x_date(
      date_labels = "%b", date_breaks = "1 month", expand = c(0, 0)) +
    ggplot2::scale_y_continuous(
      breaks = FIGTSBREAKS, limits = c(0,max(FIGTSBREAKS)), expand = c(0, 0)) +
    ggplot2::xlab("Month") +
    ggplot2::ylab("Number of cases") +
    ggplot2::scale_colour_manual("lines",
                                 values = c("year" = "#69AE23", "Mean" = "#767171"),
                                 labels = c("year" = as.character(year),
                                            "Mean" = paste("Mean (", year - 4 , "\U2013", year - 1, ")", sep = ""))) +
    ggplot2::scale_fill_manual("", values = "grey80") +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      axis.title = ggplot2::element_text(size = 9),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "#767171"),
      legend.position = "right",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size=8),
      legend.key = ggplot2::element_blank(),
      legend.key.width = ggplot2::unit(0.8, "cm")) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(linetype = c("longdash", "solid"),
                            lwd = c(0.6, 1.1))))

  return(p)

}
