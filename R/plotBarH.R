#' Horizontal bar graph
#'
#' This function draws an horizontal bar graph of the values of variable 'Yvar'
#' with the categorical variable 'Xvar' on the x-axis. \cr
#' Expects aggregated data.
#'
#' @param .data dataframe containing the variables to plot
#' @param xvar character string, name of the categorical variable to plot on the x-axis in quotes
#' @param xlabel character string, label of the x axis
#' @param yvar character string, name of the numerical variable to plot on the y-axis in quotes
#' @param ylabel character string, label of the y axis
#' @param fill_color character string, hexadecimal colour to use in the graph;
#' (default to ECDC green \code{"#65B32E"}, see EcdcColors(col_scale = "qual", n = 1))
#' @param log10_scale boolean, TRUE if y-axis should be log scale
#' (default \code{FALSE} ,see \code{ggplot2::scale_y_log10})
#' @param xlabel_black (optional) character string, value of the categorical variable
#' for which the bar should be black
#'
#' @keywords bargraph
#'
#' @seealso
#' Internal function: \code{\link{EcdcColors}} \cr
#' Required Packages: \code{\link{ggplot2}}
#'
#' @examples
#' # --- Create dummy data
#' mfratio <- data.frame( Country = sample(EpiReport::MSCode$Country, 28),
#'                        Ratio = runif(28, min = 0, max = 28))
#'
#' # --- Plot the dummy data
#' plotBarH(mfratio,
#'          xvar = "Country",
#'          xlabel = "",
#'          yvar = "Ratio",
#'          ylabel = "Male-to-Female ratio",
#'          log10_scale = FALSE)
#' plotBarH(mfratio,
#'          xvar = "Country",
#'          xlabel = "",
#'          yvar = "Ratio",
#'          ylabel = "Male-to-Female ratio",
#'          log10_scale = TRUE,
#'          xlabel_black = "EU-EEA")
#'
#' @export
#'
plotBarH <- function(.data,
                     xvar = "",
                     xlabel = "",
                     yvar = "",
                     ylabel = "",
                     fill_color = EcdcColors(col_scale = "qual", n = 1),
                     log10_scale = FALSE,
                     xlabel_black = "") {


  ## ----
  ## Setting default arguments if missing
  ## ----

  if(missing(fill_color)) { fill_color <- EcdcColors(col_scale = "qual", n = 1) }
  if(missing(log10_scale)) { log10_scale <- FALSE }


  # --- Breaks for the Y axis

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



  ###### Option not yet implemented
  FONT <- NULL


  # --- Plotting

  p <- ggplot2::ggplot(data = .data,
                       ggplot2::aes(x = stats::reorder(.data[[xvar]], .data[[yvar]]),
                                    y = .data[[yvar]])) +
    ggplot2::geom_bar(stat = "identity",
                      fill = ifelse(.data[[xvar]] == xlabel_black, "black", fill_color)) +
    ggplot2::coord_flip()  +
    ggplot2::labs(x = xlabel , y = ylabel) +
    ggplot2::theme(# --- Setting the background
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      # --- Setting text element
      axis.text = ggplot2::element_text(size = 8, family = FONT),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 9, family = FONT),
      axis.line.x = ggplot2::element_line(colour = "black")
    )

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
