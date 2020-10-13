#' Get disease-specific trend plot: trend and number of cases by month
#'
#' Function returning the plot describing the trend of the disease over time
#' that will be included in the epidemiological report at the bookmark location
#' \code{'TS_TREND'} on the template report. \cr
#' \cr
#' The graph includes the number of cases at EU/EEA level, by month,
#' over the past five years, with:
#' \itemize{
#'    \item{}{The number of cases by month over the 5-year period (grey solid line)}
#'    \item{}{The 12-month moving average of the number of cases by month (green solid line)}
#' }
#' (see ECDC reports
#' \url{https://ecdc.europa.eu/en/annual-epidemiological-reports})
#'
#' @param x dataframe, raw disease-specific dataset (see specification of the
#' dataset in the package vignette with \code{browseVignettes(package = "EpiReport")})
#' (default \code{SALM2016})
#' @param disease character string, disease code (default \code{"SALM"}).
#' Please make sure the disease code is included in the disease-specific dataset x
#' in the \code{HealthTopicCode} variable.
#' @param year numeric, year to produce the graph for (default \code{2016}).
#' Please make sure the year is included in the disease-specific dataset x
#' in the \code{TimeCode} variable.
#' @param reportParameters dataframe, dataset including the required parameters
#' for the graph and report production (default \code{AERparams}) (see specification
#' of the dataset in the package vignette with \code{browseVignettes(package = "EpiReport")})
#' @param MSCode dataframe, correspondence table of GeoCode names and codes
#' (default \code{MSCode}) (see specification of the dataset in the
#' package vignette with \code{browseVignettes(package = "EpiReport")})
#' @param index integer, figure number
#' @param doc 'Word' document (see \code{officer} package) in which to add the graph
#' at the bookmark location.
#' If doc is missing, \code{getTrend} returns the \code{ggplot2} object.
#'
#' @return 'Word' doc or a ggplot2 preview
#'
#' @seealso Global function for the full epidemilogical report: \code{\link{getAER}}  \cr
#' Required Packages: \code{\link{ggplot2}} \code{\link{officer}} \cr
#' Internal functions: \code{\link{plotTS12MAvg}} \cr
#' Default datasets: \code{\link{AERparams}} \code{\link{MSCode}}
#'
#' @examples
#'
#' # --- Plot using the default dataset
#' getTrend()
#'
#' # --- Plot using external dataset
#' # --- Please see examples in the vignette
#' browseVignettes(package = "EpiReport")
#'
#' @export
#'
getTrend <- function(x = EpiReport::SALM2016,
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
  ## Trend plot
  ## ----

  if(reportParameters$TSTrendGraphUse == "Y") {

    ## ----
    ## Filtering data
    ## ----

    # --- Filtering on the required variables
    x <- dplyr::select(x, c("HealthTopicCode", "MeasureCode", "TimeUnit",
                            "TimeCode", "GeoCode", "N"))
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the necessary variables.'))
    }

    # --- Filtering on the disease of interest
    x <- dplyr::filter(x, x$HealthTopicCode == disease)
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the selected disease "',
                 disease, '".'))
    }

    # --- Filtering on Monthly data only
    x <- dplyr::filter(x, x$TimeUnit == "M")
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the required time unit \'M\'',
                 'for the selected disease "', disease, '".'))
    }

    # --- Filtering on 5 year period monthly data
    studyPeriodYear <- (year-4):year
    studyPeriodMonth <- sub(" ", "0", format(1:12, width = 2))
    studyPeriod <- paste(rep(studyPeriodYear, each = 12),
                         rep(studyPeriodMonth, times = 5), sep="-")
    x <- dplyr::filter(x, x$TimeCode %in% studyPeriod)
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the required 5-year',
                 'study period for the selected disease "', disease, '".'))
    }

    # --- Filtering for analysis at country and EU-EEA level only, no EU level
    MS <- MSCode$GeoCode[!is.na(MSCode$EUEEA)]
    x <- dplyr::filter(x, x$GeoCode %in% MS)
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include any \'GeoCode\' at EUEEA level',
                 '(see MSCode dataset) for the selected disease "', disease, '".'))
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
      fill = list(NA, NA, NA))

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
      # officer::cursor_bookmark(doc, id = "TS_TREND")
      # doc <- officer::body_add_par(doc,
      #                              value = caption)
      doc <- officer::body_replace_text_at_bkm(x = doc,
                                               bookmark = "TS_TREND_CAPTION",
                                               value = caption)

      ## ------ Plot
      temp <- tempdir()
      grDevices::png(paste(temp, "\\Trend.png", sep = ""), width = 6, height = 3, units = "in", res = 500)
      print(p)
      grDevices::dev.off()
      doc <- officer::body_replace_img_at_bkm(x = doc,
                                              bookmark = "TS_TREND",
                                              value = officer::external_img(src = paste(temp, "\\Trend.png", sep = ""),
                                                                            width = 6,
                                                                            height = 3))
      # doc <- officer::body_add_gg(doc,
      #                             value = p,
      #                             width = 6,
      #                             height = 3)
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




#' Time series with 12-month moving average
#'
#' This function draws a line graph describing the trend of the selected disease
#' over the past 5 years. \cr
#' The graph includes the trend and number of cases at EU/EEA level, by month,
#' over the past five years, with:
#' \itemize{
#'    \item{\code{yvar}: }{The number of cases by month over the 5-year period (grey solid line)}
#'    \item{\code{movAverage}: }{The 12-month moving average of the number of cases by month (green solid line)}
#' }
#' Expects aggregated data and pre-calculated 12-month moving average.
#'
#' @param .data dataframe containing the variables to plot
#' @param xvar character string, name of the time variable to plot on the x-axis
#' in quotes (default \code{"TimeCode"})
#' @param yvar character string, name of the variable to plot on the y-axis in quotes
#' (default \code{"N"}), number of cases by month over the 5-year period (grey solid line)
#' @param movAverage character string, name of the variable to plot in quotes including
#' the moving average per each time unit (default \code{"MAV"})
#'
#' @keywords trend
#'
#' @seealso Global function: \code{\link{getTrend}}  \cr
#' Required Packages: \code{\link{ggplot2}}
#'
#' @examples
#'
#' # --- Plot using external dataset
#'
#' # Create a dummy dataset
#' test <- data.frame(Time = as.Date(paste0("2019-",c(1:12), "-01")),
#'                    N = sample(c(4000:6000), 12),
#'                    mean = sample(c(4000:5000), 12))
#'
#' # Plot the dummy data
#' plotTS12MAvg(test,
#'              xvar = "Time",
#'              yvar = "N",
#'              movAverage = "mean")
#'
#' @export
#'
plotTS12MAvg <- function(.data,
                         xvar = "TimeCode",
                         yvar = "N",
                         movAverage = "MAV"){


  # --- Breaks for the Y axis

  FIGTSBREAKS <- pretty(seq(0,
                            max(.data[[yvar]]),
                            by = max(.data[[yvar]])/7))


  # --- Please Note: ECDC AER plots use the font "Tahoma"
  # --- The font is not available on Linux

  # if ("Tahoma" %in% extrafont::fonts()) {
  #   FONT <- "Tahoma"
  #   suppressMessages(extrafont::loadfonts(device = "win"))
  # } else if (Sys.info()["sysname"] == "Windows") {
  #   suppressMessages(extrafont::font_import(pattern = 'tahoma', prompt = FALSE))
  #   suppressMessages(extrafont::loadfonts(device = "win"))
  #   FONT <- "Tahoma"
  # } else {
  #   FONT <- NULL
  # }
  FONT <- NULL


  # --- Plotting

  p <- ggplot2::ggplot(.data,
                       ggplot2::aes(.data[[xvar]])) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data[[yvar]] , color = "Number of cases"), size = 0.6) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data[[movAverage]], color = "12-month moving average"), size = 1.1, na.rm = TRUE) +
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
      values=c("Number of cases" = EcdcColors(col_scale = "grey", grey_shade = "mediumdark"),
               "12-month moving average"= EcdcColors(col_scale = "green", n=1))) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8, family = FONT),
      axis.title = ggplot2::element_text(size = 9, family = FONT),
      axis.line = ggplot2::element_line(colour = EcdcColors(col_scale = "grey", grey_shade = "mediumdark")),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.position = "right",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 8, family = FONT),
      legend.key = ggplot2::element_blank(),
      legend.key.width = ggplot2::unit(0.8, "cm")) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE),
                    colour = ggplot2::guide_legend(reverse = TRUE))

  return(p)

}





