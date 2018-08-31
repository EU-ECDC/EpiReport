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
#' @examples
#' # Load dummy data
#' load(system.file("extdata", "mav_example.rda", package = "ECDC.graphs"))
#' 
#' # Plot the example
#' AER_ts_12_month_avg(data = mav, xvar = timecode, 
#' yvar = n, mov_average = M.AV)
#' @export
AER_ts_12_month_avg <- function(data, xvar,
                                yvar,
                                mov_average){
  xvar <-deparse(substitute(xvar))
  yvar <-deparse(substitute(yvar))
  mov_average <-deparse(substitute(mov_average))
  FIGTSBREAKS <- pretty(seq(0, max(data[[yvar]]),
                           by = max(data[[yvar]])/5))
  
p <- ggplot(data, aes_string(x = xvar)) + 
  geom_line(aes_string(y = yvar, colour = '"Number of cases"'), size=0.6) +
  geom_line(aes_string(y = mov_average, colour = '"12-month moving average"'), size=1.1) +
  scale_x_date(date_labels = "  %b \n %Y", date_breaks= "6 months", expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), breaks = FIGTSBREAKS, limits = c(0, max(FIGTSBREAKS))) +
  xlab("\nMonth") + 
  ylab("Number of cases") +
  scale_colour_manual("lines", values=c("Number of cases" = "#767171", 
                                        "12-month moving average"= "#65B32E")) +
  theme(axis.text=element_text(size=8, family = "Tahoma"), axis.title=element_text(size=9, 
                                                                                   family = "Tahoma")) +    #Axis text style
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +  #Empty backgroud
  theme(axis.line = element_line(colour = "#767171") ) +
  theme(legend.position = "right", legend.title =element_blank(), legend.text =element_text(size=8, family = "Tahoma"),
        legend.key=element_blank(), legend.key.width = unit(0.8, "cm")) +
  guides(fill=guide_legend(reverse=TRUE), colour=guide_legend(reverse=TRUE))

return(p)
}
