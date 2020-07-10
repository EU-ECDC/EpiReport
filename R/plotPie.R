#' Pie chart
#'
#' This function draws a pie chart of the values of variable 'Xvar'
#' with the labels from the categorical variable 'Labels'. \cr
#' Expects aggregated data.
#'
#' @param .data dataframe containing the variables to plot
#' @param xvar character string, name of the numerical variable
#' to plot in quotes
#' @param labels character string, name of the character variable
#' including the corresponding labels
#' @param fill_colors vector of character strings, hexadecimal colours
#' to use for each labels in the piechart; the vector should contain
#' the exact number of categories defined in \code{"labels"} variable.
#' (default to ECDC colors, see
#' \code{EcdcColors(col_scale = "qual", n = nrow(.data))})
#'
#' @keywords pie
#'
#' @seealso
#' Internal function: \code{\link{EcdcColors}} \cr
#' Required Packages: \code{\link{ggplot2}}
#'
#' @examples
#' # --- Create dummy data
#' piechart <- data.frame(Labels = c("Heterosexual females,\n 1 633, 7.17%",
#'                                   "Heterosexual males,\n 2 937, 12.90%",
#'                                   "MSM,\n 15 342, 67.39%",
#'                                   "Unknow\n 2 854, 12.54%"),
#'                        Values = c(1633,2937,15342,2854))
#'
#' # --- Plot the dummy data
#' plotPie(piechart,
#'          xvar = "Values",
#'          labels = "Labels")
#'
#' @export
#'
plotPie <- function(.data,
                     xvar = "",
                     labels = "",
                     fill_colors = EcdcColors(col_scale = "qual",
                                              n = nrow(.data))) {


  ## ----
  ## Setting default arguments if missing
  ## ----

  if(missing(fill_colors)) { fill_colors <- EcdcColors(col_scale = "qual",
                                                     n = nrow(.data)) }
  if(nrow(.data) != length(fill_colors)) {
    warning(paste('fill_colors does not contain the proper number of colors
                  according to the dataset provided.'))
  }


  # --- Plotting

  graphics::pie(.data[[xvar]],
                labels = .data[[labels]],
                border = "white",
                col = fill_colors)

  # --- Test with ggplot2
  # --- but not really recommended apparently (https://www.r-graph-gallery.com/pie-plot.html)
  # p <- ggplot2::ggplot(data = .data,
  #                      ggplot2::aes(x = "",
  #                                   y = .data[[xvar]],
  #                                   fill = .data[[labels]])) +
  #   ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
  #   ggplot2::coord_polar("y", start = 0) +
  #   ggplot2::scale_fill_manual(values = fill_colors) +
  #   ggplot2::theme_void() +
  #   ggplot2::theme(
  #     # legend.position = "none",
  #     axis.text.x = ggplot2::element_text(colour = 'black')) +
  #   ggplot2::scale_y_continuous(breaks = cumsum(.data[[xvar]]) - (.data[[xvar]]*0.5), labels= .data[[labels]]) +
  #   ggplot2::geom_text( ggplot2::aes(y = cumsum(.data[[xvar]]) - (.data[[xvar]]*0.5),
  #     label = .data[[labels]]), color = "white" )
  #
  # --- Used to be a plotly output
  # --- However, not really compatible with Word output
  # p <- plotly::plot_ly(.data,
  #                      labels = .data[[labels]],
  #                      values = .data[[xvar]],
  #                      type = 'pie',
  #                      width = 1000,
  #                      height = 1000,
  #                      textposition = 'outside',
  #                      textinfo = 'label',
  #                      textfont = list(size = 20),
  #                      marker = list(colors = fill_colors,
  #                                    line = list(color = '#FFFFFF', width = 1)),
  #                      showlegend = FALSE,
  #                      sort = FALSE)
  # p <- plotly::layout(p, xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  #          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  #          autosize = FALSE,
  #          margin = list(l=300, r=300, b=100, t=100, pad=4))
  # plotly::export(p, file = "test/tranmission.png")
  # plotly::orca(p, file = "test/tranmission.png")
  # return(p)

}
