#' AER age-gender bar
#'
#' This function draws a barchart by age group and gender(or possibly other grouping), AER style.
#' Expects aggregated data.
#' @param data Your data frame
#' @param xvar Variable on the x-axis in quotes; e.g. age group
#' @param yvar Variable on the y-axis in quotes with the rate or number of cases
#' @param fill_color1 Bar 1 colour; defaults to ECDC green "#65B32E"
#' @param fill_color2 Bar 2 colour; defaults to light blue "#7CBDC1"
#' @param group A grouping variable in quotes, e.g. gender as in the AER.
#' @param ytitle y-axis title; defaults to "Rate".
#' @keywords AER_age_gender_bar
#' @export
#' @examples
#' # Create dummy data
#' mydat <- data.frame(Gender=c("F", "F", "M", "M"),
#' AgeGroup = c("0-65", "65+", "0-65", "65+"),
#' NumberOfCases = c(54,43,32,41))
#'
#' # Plot the dummy data
#' # AER_age_gender_bar(mydat, xvar = AgeGroup,
#' # yvar = NumberOfCases,
#' # group = Gender,
#' # ytitle = "Number of cases")
#' @export
AER_age_gender_bar <- function(data, xvar,
                         yvar,
                         group,
                         fill_color1 = "#65B32E",
                         fill_color2 = "#7CBDC4",
                         ytitle  = "Rate") {

    if (missing(data)){
      print("You need to specify the data!")
      stop()}

  xvar <-deparse(substitute(xvar))
  yvar <-deparse(substitute(yvar))
  group <-deparse(substitute(group))

  FIG2BREAKS <- pretty(seq(0, max(data[[yvar]]),
                               by = max(data[[yvar]])/5))

  p1 <- ggplot2::ggplot(data=data, ggplot2::aes_string(x = xvar, y = yvar, fill = group)) +
    ggplot2::geom_bar(stat="identity", position = ggplot2::position_dodge()) +
    ggplot2::scale_fill_manual(values=c(fill_color1, fill_color2)) +
    ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0, max(FIG2BREAKS)),
                       breaks = FIG2BREAKS) +
    ggplot2::labs(title="", x="Age",y=ytitle) +
    ggplot2::theme(axis.text = ggplot2::element_text(size=8, family = "Tahoma"),
                   axis.title = ggplot2::element_text(size=9, family = "Tahoma")) +    #Axis text style
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank()) +  #Empty backgroud
    ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
          axis.line.x = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "right", legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size=8, family="Tahoma"),
           legend.key.width = ggplot2::unit(0.8, "cm"), legend.key.size = ggplot2::unit(0.4, "cm"))

  return(p1)
}

