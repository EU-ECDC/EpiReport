#' Get disease-specific seasonality graph: distribution of cases by month
#'
#' Function returning the plot describing the seasonality of the disease
#' that will be included in the epidemiological report at the bookmark location
#' \code{'TS_SEASON'} of the template report. \cr
#' \cr
#' The graph includes the distribution of cases at EU/EEA level, by month,
#' over the past five years, with:
#' \itemize{
#'    \item{}{The number of cases by month in the reference year (green solid line)}
#'    \item{}{The mean number of cases by month in the four previous years (grey dashed line)}
#'    \item{}{The minimum number of cases by month in the four previous years (grey area)}
#'    \item{}{The maximum number of cases by month in the four previous years (grey area)}
#' }
#' (see ECDC reports
#' \url{https://www.ecdc.europa.eu/en/annual-epidemiological-reports})
#'
#' @param x dataframe, raw disease-specific dataset (see specification of the
#' dataset in the package vignette with \code{browseVignettes(package = "EpiReport")})
#' (default \code{DENGUE2019})
#' @param disease character string, disease code (default \code{"DENGUE"}).
#' Please make sure the disease code is included in the disease-specific dataset x
#' in the \code{HealthTopicCode} variable.
#' @param year numeric, year to produce the graph for (default \code{2019}).
#' Please make sure the year is included in the disease-specific dataset x
#' in the \code{TimeCode} variable.
#' @param reportParameters dataframe, dataset including the required parameters
#' for the graph and report production (default \code{AERparams}) (see specification
#' of the dataset in the package vignette with \code{browseVignettes(package = "EpiReport")})
#' @param MSCode dataframe, correspondence table of GeoCode names and codes
#' (default \code{MSCode}) (see specification of the dataset in the
#' package vignette with \code{browseVignettes(package = "EpiReport")})
#' @param index integer, figure number
#' @param doc 'Word' document (see \code{'officer'} package) in which to add the graph
#' at the bookmark location.
#' If doc is missing, \code{getSeason} returns the \code{ggplot2} object.
#'
#' @return 'Word' doc or a ggplot2 object
#'
#' @seealso Global function for the full epidemilogical report: \code{\link{getAER}}  \cr
#' Required Packages: \code{\link{ggplot2}} \code{\link{officer}} \cr
#' Internal functions: \code{\link{plotSeasonality}} \cr
#' Default datasets: \code{\link{AERparams}} \code{\link{MSCode}}
#'
#' @examples
#'
#' # --- Plot using the default dataset
#' getSeason()
#'
#' # --- Plot using external dataset
#' # --- Please see examples in the vignette
#' browseVignettes(package = "EpiReport")
#'
#' @export
#'
getSeason <- function(x = EpiReport::DENGUE2019,
                      disease = "DENGUE",
                      year = 2019,
                      reportParameters = EpiReport::AERparams,
                      MSCode = EpiReport::MSCode,
                      index = 1,
                      doc){

  ## ----
  ## Setting default arguments if missing
  ## ----

  if(missing(x)) { x <- EpiReport::DENGUE2019 }
  if(missing(disease)) { disease <- "DENGUE" }
  if(missing(year)) { year <- 2019 }
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

    # --- Filtering on the required variables
    x <- dplyr::select(x, c("HealthTopicCode", "MeasureCode", "TimeUnit",
                            "TimeCode", "GeoCode", "YValue"))
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the necessary variables.'))
    }

    # --- Filtering on the disease of interest
    x <- dplyr::filter(x, x$HealthTopicCode == disease)
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the selected disease "',
                 disease, '".'))
    }

    # --- Filtering on Yearly data only
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
    if(nrow(x) == 0 |
       sum(studyPeriod %in% x$TimeCode, na.rm = TRUE) != length(studyPeriod)) {
      stop(paste('The dataset does not include the required 5-year study period for the selected disease "',
                 disease, '".'))
    }

    # --- Excluding NA values as missing time points
    x <- dplyr::filter(x, !is.na(x$YValue))
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include the required 5-year',
                 'study period for the selected disease "', disease, '".'))
    }

    # --- Filtering for analysis at country and EU-EEA level only, no EU level
    MS <- MSCode$GeoCode[!is.na(MSCode$EUEEA)]
    x <- dplyr::filter(x, x$GeoCode %in% MS)
    if(nrow(x) == 0) {
      stop(paste('The dataset does not include any \'GeoCode\' at EUEEA level',
                 '(see MSCode dataset) for the selected disease "',
                 disease, '".'))
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
    N <- YValue <- TimeCode <- TimeYear <- TimeMonth <- NULL
    eueea <- dplyr::group_by(x, TimeCode, TimeYear, TimeMonth)
    eueea <- dplyr::summarise(eueea, "N" = sum(YValue, na.rm = TRUE), .groups = "drop")
    eueea <- dplyr::ungroup(eueea)

    # --- Compute mean, min and max
    summ <- dplyr::filter(eueea, eueea$TimeYear != year)
    summ <- dplyr::group_by(summ, TimeMonth)
    summ <- dplyr::summarise(summ,
                             Mean4Years = mean(N, na.rm = TRUE),
                             Max4Years=max(N, na.rm = TRUE),
                             Min4Years=min(N, na.rm = TRUE),
                             .groups = "drop")
    summ <- dplyr::ungroup(summ)

    # --- Finalysing the result table
    agg <- dplyr::filter(eueea, eueea$TimeYear == year)
    agg <- dplyr::inner_join(agg, summ, by = "TimeMonth")



    ## ----
    ## Plot
    ## ----

    p <- plotSeasonality(agg,
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
      doc <- officer::body_replace_text_at_bkm(x = doc,
                                               bookmark = "TS_SEASON_CAPTION",
                                               value = caption)

      ## ------ Plot
      doc <- EpiReport::body_replace_gg_at_bkm(doc = doc,
                                               gg = p,
                                               bookmark = "TS_SEASON",
                                               width = 6,
                                               height = 3)

      ## ------ List of countries reporting consistently
      countries <- EpiReport::MSCode$TheCountry[EpiReport::MSCode$GeoCode %in% x$GeoCode]
      countries <- paste(countries, collapse = ", ")
      countries <- paste("Source: Country reports from ", countries, ".", sep = "")
      doc <- officer::body_replace_text_at_bkm(x = doc,
                                               bookmark = "TS_SEASON_COUNTRIES",
                                               value = countries)
    }
  }


  ## ----
  ## No Seasonal plot for this disease
  ## ----

  if(reportParameters$TSSeasonalityGraphUse == "N") {
    message(paste('According to the parameter table \'AERparams\', this disease "',
                  disease, '" does not include any seasonal graph in the AER report.',
                  sep = ""))
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


#' Seasonality line graph
#'
#' This function draws a line graph describing the seasonality of the selected disease
#' over the past 5 years. \cr
#' The graph includes the distribution of cases, by month, over the past five years, with:
#' \itemize{
#'    \item{\code{yvar}: }{The number of cases by month in the reference year (green solid line)}
#'    \item{\code{mean4years}: }{The mean number of cases by month in the four previous years (grey dashed line)}
#'    \item{\code{min4years}: }{The minimum number of cases by month in the four previous years (grey area)}
#'    \item{\code{max4years}: }{The maximum number of cases by month in the four previous years (grey area)}
#' }
#' Expects aggregated data and pre-calculated min, max and mean figures.
#'
#' @param .data dataframe containing the variables to plot
#' @param xvar character string, name of the time variable on the x-axis in quotes
#' (default \code{"TimeCode"})
#' @param yvar character string, name of the variable to plot on the y-axis in quotes
#' (default \code{"N"}), number of cases by month in the reference year (green solid line)
#' @param min4years character string, name of the variable to plot in quotes including the minimum
#' number of cases by month over the past 4 years (default \code{"Min4Years"})
#' @param max4years character string, name of the variable to plot in quotes including the maximum
#' number of cases by month over the past 4 years (default \code{"Max4Years"})
#' @param mean4years character string, name of the variable to plot in quotes including the mean of the
#' number of cases by month over the past 4 years (default \code{"Mean4Years"})
#' @param year numeric, year to produce the graph for (default \code{2016}).
#'
#' @keywords seasonality
#'
#' @seealso Global function: \code{\link{getSeason}}  \cr
#' Required Packages: \code{\link{ggplot2}}
#' @examples
#'
#'
#' # --- Plot using external dataset
#'
#' # Create a dummy dataset
#' test <- data.frame(Time = as.Date(paste0("2019-",c(1:12), "-01")),
#'                    N = sample(c(5000:7000), 12),
#'                    mean = sample(c(4000:5000), 12),
#'                    low = sample(c(3000:4000), 12),
#'                    high = sample(c(5000:6000), 12))
#'
#' # Plot the dummy data
#' plotSeasonality(test,
#'                 xvar = "Time",
#'                 yvar = "N",
#'                 min4years = "low",
#'                 max4years = "high",
#'                 mean4years = "mean",
#'                 year = 2019)
#'
#' # --- Please see examples in the vignette
#' browseVignettes(package = "EpiReport")
#'
#' # --- Plot using the default dataset
#' getSeason()

#'
#' @export
#'
plotSeasonality <- function(.data,
                            xvar = "TimeCode",
                            yvar = "N",
                            min4years = "Min4Years",
                            max4years = "Max4Years",
                            mean4years = "Mean4Years",
                            year = 2016){


  # --- Setting breaks for the time series to be nice

  FIGTSBREAKS <- pretty(seq(0,
                            max(.data[[max4years]], .data[[yvar]], na.rm = TRUE),
                            by = max(.data[[max4years]], .data[[yvar]], na.rm = TRUE)/7))


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

  p <- ggplot2::ggplot(.data, ggplot2::aes(.data[[xvar]])) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data[[min4years]] ,
                   ymax = .data[[max4years]],
                   fill = paste("Min-max (", year - 4, "\U2013", year - 1, ")",sep = "")) , alpha = 0.5) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data[[mean4years]],
                   color = "Mean"),
      linetype = "longdash", size = 0.6) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data[[yvar]], color = "year"), size = 1.1) +
    ggplot2::scale_x_date(
      date_labels = "%b", date_breaks = "1 month", expand = c(0, 0)) +
    ggplot2::scale_y_continuous(
      breaks = FIGTSBREAKS, limits = c(0,max(FIGTSBREAKS, na.rm = TRUE)), expand = c(0, 0)) +
    ggplot2::xlab("Month") +
    ggplot2::ylab("Number of cases") +
    ggplot2::scale_colour_manual("lines",
                                 values = c("year" = EcdcColors(col_scale = "green", n=1),
                                            "Mean" = EcdcColors(col_scale = "grey", grey_shade = "mediumdark")),
                                 labels = c("year" = as.character(year),
                                            "Mean" = paste("Mean (", year - 4 , "\U2013", year - 1, ")", sep = ""))) +
    ggplot2::scale_fill_manual("", values = EcdcColors(col_scale = "grey", grey_shade = "mediumlight")) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8, family = FONT),
      axis.title = ggplot2::element_text(size = 9, family = FONT),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = EcdcColors(col_scale = "grey", grey_shade = "mediumdark")),
      legend.position = "right",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 8, family = FONT),
      legend.key = ggplot2::element_blank(),
      legend.key.width = ggplot2::unit(0.8, "cm"))+
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(linetype = c("longdash", "solid"),
                            lwd = c(0.6, 1.1))))

  return(p)

}
