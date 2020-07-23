#' Multiple time series plot
#'
#' This function draws a time series of the values of variable 'Yvar'
#' with the time variable 'Xvar' on the x-axis.
#' The categorical variable that specify the group of the observations
#' for which there will be one time series each. \cr
#' Expects aggregated data.
#'
#' @param .data dataframe containing the variables to plot
#' @param xvar character string, name of the time variable (expects date format)
#' to plot on the x-axis in quotes
#' @param xlabel character string, label of the x axis
#' @param yvar character string, name of the numerical variable to plot on the y-axis in quotes
#' @param ylabel character string, label of the y axis
#' @param fill_color character string, hexadecimal colour to use in the graph;
#' (default to ECDC green \code{"#65B32E"},
#' see \code{EcdcColors(col_scale = "qual", n = length(unique(.data[[group]])))}
#' @param group character string, name of the grouping variable in quotes, e.g. gender.
#' @param log10_scale boolean, \code{TRUE} if y-axis should be log scale
#' (default \code{FALSE} ,see \code{ggplot2::scale_y_log10})
#' @param xvar_format character string, time format to use to plot the x-axis
#' (\code{"\%Y"} for yearly labels or \code{"\%b \%Y"} for monthly labels)
#' @param xvar_breaks character string, time unit to use to plot the x-axis between breaks
#' (\code{"1 year"} or \code{"1 month"}, see \code{ggplot2::scale_x_date(date_breaks = ... )})
#'
#' @keywords timeseries
#'
#' @seealso
#' Internal function: \code{\link{EcdcColors}} \cr
#' Required Packages: \code{\link{ggplot2}}
#'
#' @examples
#' # --- Create dummy data
#' mydat <- data.frame(TimeCode = seq(as.Date("2008/1/1"), as.Date("2017/1/1"), "years"),
#'                     YValue = sample(1:79/10, 20),
#'                     YLabel = rep(c("Acute", "Chronic"), each = 10))
#'
#' # --- Plot the dummy data
#' plotTSGrouped(mydat,
#'               xvar = "TimeCode",
#'               xlabel = "Year",
#'               yvar = "YValue",
#'               ylabel = "Rate per 100 000 population",
#'               group = "YLabel",
#'               log10_scale = TRUE,
#'               xvar_format = "%Y",
#'               xvar_breaks = "1 year")
#'
#' # --- Create dummy data
#' mydat <- data.frame(TimeCode = rep(seq(as.Date("2008/1/1"), as.Date("2017/1/1"), "years"), 5),
#'                     YValue = c(sample(1:50/10, 10),
#'                                sample(1:100/10, 10),
#'                                sample(1:300/10, 10),
#'                                sample(1:400/10, 10),
#'                                sample(1:500/10, 10)),
#'                     YLabel = rep(c("United Kingdom", "France", "Spain", "Netherlands", "Belgium"), each = 10))
#'
#' # --- Plot the dummy data
#' plotTSGrouped(mydat,
#'               xvar = "TimeCode",
#'               xlabel = "Year",
#'               yvar = "YValue",
#'               ylabel = "Rate per 100 000 population",
#'               group = "YLabel",
#'               log10_scale = FALSE,
#'               xvar_format = "%Y",
#'               xvar_breaks = "1 year")
#'
#' @export
#'
plotTSGrouped <- function(.data,
                     xvar = "",
                     xlabel = "",
                     yvar = "",
                     ylabel = "",
                     group = "",
                     fill_color = EcdcColors(col_scale = "qual",
                                             n = length(unique(.data[[group]]))),
                     log10_scale = FALSE,
                     xvar_format = "%Y",
                     xvar_breaks = "1 year") {

  ## ----
  ## Setting default arguments if missing
  ## ----

  if(missing(fill_color)) { fill_color <- EcdcColors(col_scale = "qual",
                                                     n = length(unique(.data[[group]]))) }
  if(missing(log10_scale)) { log10_scale <- FALSE }
  if(missing(xvar_format)) { xvar_format <- "%Y" }
  if(missing(xvar_breaks)) { xvar_breaks <- "1 year" }



  ## ----
  ## Setting breaks for Y axis
  ## ----

  if (log10_scale == TRUE) {
    MAX <- max(.data[[yvar]])
    if (floor(MAX) > 1) {
      BREAKS <- c( log10(10^((1:10)/10)), log10(10^(2:floor(MAX))) )
      LABELS <- rep("", length(BREAKS))
      if (floor(MAX) <= 10) {
        LABELS[c(1, 10)] <- BREAKS[c(1,10)]
      } else {
        LABELS[c(1, 10, seq(19, length(BREAKS), 10))] <- BREAKS[c(1,10,seq(19, length(BREAKS), 10))]
      }
    } else {
      BREAKS <- log10(10^((1:floor(MAX*10))/10))    ## if yvalues <0.1 breaks need to be adjusted
      LABELS <- BREAKS
    }
  } else {
    LABELS <- pretty(seq(0,
                         max(.data[[yvar]]),
                         by = max(.data[[yvar]])/5))
  }



  ## ----
  ## Plotting
  ## ----

  p <- ggplot2::ggplot(.data,
                       ggplot2::aes(x = .data[[xvar]],
                                    y = .data[[yvar]],
                                    group = .data[[group]],
                                    color = .data[[group]])) +
    ggplot2::geom_line(size = 1) +
    ggplot2::scale_colour_manual("lines", values=c(fill_color)) +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab(ylabel) +
    ggplot2::scale_x_date(
      date_label = xvar_format,
      date_breaks = xvar_breaks,
      expand = c(0, 0))+
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      axis.title = ggplot2::element_text(size = 9),
      axis.line = ggplot2::element_line(colour = EcdcColors(col_scale = "grey", grey_shade = "mediumdark")),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.position = "right",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 8),
      legend.key = ggplot2::element_blank(),
      legend.key.width = ggplot2::unit(0.8, "cm"))


  if (log10_scale == TRUE) {
    p <- p +
      ggplot2::scale_y_log10(breaks = BREAKS,
                             labels = LABELS)
  } else {
    p <- p +
      ggplot2::scale_y_continuous(expand = c(0,0),
                                  limits = c(0, max(LABELS)),
                                  breaks = LABELS)
  }

  return(p)

}
