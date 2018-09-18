#' AER time series with 12-month moving average
#'
#' This function draws a line graph with 12-month moving average, AER style.
#' Expects aggregated data and pre-calculated 12-month moving average.
#' @param data Your data frame.
#' @param xvar Time variable, given in quotes.
#' @param yvar A variable with the number of cases for each time unit, given in quotes.
#' @param mov_average A variable with the moving average per each time unit.

#' @keywords moving average
#' @export
#' @export
AER_ts_12_month_avg <- function(data, xvar,
                                yvar,
                                mov_average){
  xvar <-deparse(substitute(xvar))
  yvar <-deparse(substitute(yvar))
  mov_average <-deparse(substitute(mov_average))
  FIGTSBREAKS <- pretty(seq(0, max(data[[yvar]]),
                           by = max(data[[yvar]])/5))

p <- ggplot2::ggplot(data, ggplot2::aes_string(x = xvar)) +
  ggplot2::geom_line(ggplot2::aes_string(y = yvar, colour = '"Number of cases"'), size=0.6) +
  ggplot2::geom_line(ggplot2::aes_string(y = mov_average, colour = '"12-month moving average"'), size=1.1) +
  ggplot2::scale_x_date(date_labels = "  %b \n %Y", date_breaks= "6 months", expand = c(0, 0)) +
  ggplot2::scale_y_continuous(expand = c(0, 0), breaks = FIGTSBREAKS, limits = c(0, max(FIGTSBREAKS))) +
  ggplot2::xlab("\nMonth") +
  ggplot2::ylab("Number of cases") +
  ggplot2::scale_colour_manual("lines", values=c("Number of cases" = "#767171",
                                        "12-month moving average"= "#65B32E")) +
  ggplot2::theme(axis.text=ggplot2::element_text(size=8, family = "Tahoma"), axis.title=ggplot2::element_text(size=9,
                                                                                   family = "Tahoma")) +    #Axis text style
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank()) +  #Empty backgroud
  ggplot2::theme(axis.line = ggplot2::element_line(colour = "#767171") ) +
  ggplot2::theme(legend.position = "right", legend.title =ggplot2::element_blank(), legend.text =ggplot2::element_text(size=8, family = "Tahoma"),
        legend.key=ggplot2::element_blank(), legend.key.width = ggplot2::unit(0.8, "cm")) +
  ggplot2::guides(fill=ggplot2::guide_legend(reverse=TRUE), colour=ggplot2::guide_legend(reverse=TRUE))

return(p)
}
