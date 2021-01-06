#' Age and Gender bar graph
#'
#' (Discouraged function. Please use \code{plotBarGrouped()} instead.)
#'
#' This function draws a bar graph of the distribution of cases by age group
#' and gender (or possibly other grouping). \cr
#' The bar graph presents the distribution of cases at EU/EEA level using either:
#' \itemize{
#'    \item{\code{AG-COUNT}: }{The number of cases by age and gender}
#'    \item{\code{AG-RATE}: }{The rate per 100 000 cases by age and gender}
#'    \item{\code{AG-PROP}: }{The proportion of cases by age and gender}
#' }
#' Expects aggregated data.
#'
#' @param .data dataframe containing the variables to plot
#' @param xvar character string, name of the variable to plot on the x-axis in quotes
#' (default \code{"XLabel"})
#' @param yvar character string, name of the variable to plot on the y-axis in quotes
#' (default \code{"ZValue"})
#' @param fill_color1 character string, hexadecimal colour to use in the graph for bar 1;
#' (default to ECDC green \code{"#65B32E"}, see EcdcColors(col_scale = "qual", n = 2))
#' @param fill_color2 character string, hexadecimal colour to use in the graph for bar 2;
#' (default to ECDC blue \code{"#7CBDC4"}, see EcdcColors(col_scale = "qual", n = 2))
#' @param group character string, name of the grouping variable in quotes, e.g. gender.
#' (default \code{"YLabel"})
#' @param ytitle character string, y-axis title; (default \code{"Rate"}).
#'
#' @keywords age gender bargraph
#'
#' @seealso Global function: \code{\link{getAgeGender}}  \cr
#' Internal function: \code{\link{EcdcColors}} \cr
#' Required Packages: \code{\link{ggplot2}}
#'
#' @examples
#' # --- Create dummy data
#' mydat <- data.frame(Gender=c("F", "F", "M", "M"),
#'                     AgeGroup = c("0-65", "65+", "0-65", "65+"),
#'                     NumberOfCases = c(54,43,32,41))
#'
#' # --- Plot the dummy data
#' plotAgeGender(mydat,
#'               xvar = "AgeGroup",
#'               yvar = "NumberOfCases",
#'               group = "Gender",
#'               ytitle = "Number of cases")
#'
#' @export
#'
plotAgeGender <- function(.data,
                          xvar = "XLabel",
                          yvar = "ZValue",
                          group = "YLabel",
                          fill_color1 = "#65B32E",
                          fill_color2 = "#7CBDC4",
                          ytitle  = "Rate") {


  # --- Breaks for the Y axis

  FIGBREAKS <- pretty(seq(0,
                          max(.data[[yvar]]),
                          by = max(.data[[yvar]])/5))


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

  p <- ggplot2::ggplot(data = .data,
                       ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]], fill = .data[[group]])) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::scale_fill_manual(values = c(fill_color1, fill_color2)) +
    ggplot2::scale_y_continuous(expand = c(0,0),
                                limits = c(0, max(FIGBREAKS)),
                                breaks = FIGBREAKS) +
    ggplot2::labs(title = "", x = "Age", y = ytitle) +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 8, family = FONT),
                   axis.title = ggplot2::element_text(size = 9, family = FONT),
                   axis.line = ggplot2::element_line(colour = "black"),
                   axis.line.x = ggplot2::element_blank(),
                   # --- Setting the background
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   # --- Setting the legend
                   legend.position = "right",
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 8, family = FONT),
                   legend.key.width = ggplot2::unit(0.8, "cm"),
                   legend.key.size = ggplot2::unit(0.4, "cm"))

  return(p)
}




#' Age bar graph
#'
#' (Discouraged function. Please use \code{plotBarGrouped()} instead.)
#'
#' This function draws a bar graph by age group (or possibly other grouping). \cr
#' The bar graph presents the distribution of cases at EU/EEA level
#' using the rate per 100 000 cases by age. \cr
#' Expects aggregated data.
#'
#' @param .data dataframe containing the variables to plot
#' @param xvar character string, name of the variable to plot on the x-axis in quotes
#' (default \code{"XLabel"})
#' @param yvar character string, name of the variable to plot on the y-axis in quotes
#' (default \code{"YValue"})
#' @param fill_color1 character string, hexadecimal colour to use in the graph;
#' (default to ECDC green \code{"#65B32E"}, see EcdcColors(col_scale = "qual", n = 1))
#' @param ytitle character string, y-axis title; (default \code{"Rate"}).
#'
#' @keywords age bargraph
#'
#' @seealso Global function: \code{\link{getAgeGender}}  \cr
#' Internal function: \code{\link{EcdcColors}} \cr
#' Required Packages: \code{\link{ggplot2}}
#'
#' @examples
#'
#' # --- Create dummy data
#' mydat <- data.frame(AgeGroup = c("0-25", "26-65", "65+"),
#'                     NumberOfCases = c(54,32,41))
#'
#' # --- Plot the dummy data
#' plotAge(mydat,
#'         xvar = "AgeGroup",
#'         yvar = "NumberOfCases",
#'         ytitle = "Number of cases")
#'
#' @export
#'
plotAge <- function(.data,
                    xvar = "XLabel",
                    yvar = "YValue",
                    fill_color1 = "#65B32E",
                    ytitle  = "Rate") {


  # --- Breaks for the Y axis

  FIGBREAKS <- pretty(seq(0,
                          max(.data[[yvar]]),
                          by = max(.data[[yvar]])/5))

  # --- Plotting

  p <- ggplot2::ggplot(data = .data,
                       ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]])) +
    ggplot2::geom_bar(stat = "identity", fill = fill_color1) +
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
                   panel.background = ggplot2::element_blank())

  return(p)
}

