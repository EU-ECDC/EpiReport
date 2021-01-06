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
#' piechart <- data.frame(Labels = c("Heterosexual females",
#'                                   "Heterosexual males",
#'                                   "MSM",
#'                                   "Unknow"),
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


  # --- Ordering the pie slices
  .data <- .data[order(.data[[xvar]], decreasing = TRUE), ]
  .data[[labels]] <- factor(.data[[labels]], levels = unique(.data[[labels]]))


  # --- Plotting with ggplot2
  p <- ggplot2::ggplot(data = .data,
                       ggplot2::aes(x = "",
                                    y = .data[[xvar]],
                                    fill = .data[[labels]])) +
    ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
    ggplot2::coord_polar("y", start = 0, direction = -1) +
    ggplot2::scale_fill_manual(values = fill_colors) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.title = ggplot2::element_blank())


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

  # --- Plotting with basic R
  # graphics::pie(.data[[xvar]],
  #               labels = .data[[labels]],
  #               border = "white",
  #               col = fill_colors)


  return(p)

}
