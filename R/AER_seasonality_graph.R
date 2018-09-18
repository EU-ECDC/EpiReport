#' AER
#'
#' This function ...
#' @param data Your data frame
#' @param xvar Variable on the x-axis in quotes; e.g. age group
#' @param yvar Variable on the y-axis in quotes with the rate or number of cases
#' @param yearvar Var...
#' @param monthvar Var...
#' @param year Var...
#' @keywords AER_seasonality_graph
#' @export
AER_seasonality_graph <- function(data, xvar = "timecode",
                                  yvar = "yvalue",
                                  yearvar  = "timeyear",
                                  monthvar = "timemonth",
                                  year = 2015){
  #-----> Compute mean, min and max
  sums <- data[data[[yearvar]]!=year,]
  aggeuyear <- data[data[[yearvar]]==year,]
  # dt <- data.table::data.table(sums)            #!!!! Need to import data.table?
  dt <-dt[,list(Mean4Years = mean(get(yvar), na.rm = TRUE),Max4Years=max(get(yvar)),
                        Min4Years=min(get(yvar))),by=get(monthvar)]

  FIGTSBREAKS <- pretty(seq(0, max(data[[yvar]]),
  by = max(data[[yvar]], dt[["Max4Years"]])/5))

p1 <- ggplot2::ggplot(aggeuyear, ggplot2::aes_string(x = xvar)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin=dt$Min4Years, ymax=dt$Max4Years,
              fill = paste("Min-max (",min(data[[yearvar]]),"-",year-1, ")",sep="")),
              alpha=0.5) +
  ggplot2::geom_line(ggplot2::aes_string(y=yvar, colour = '"Year"'), size=1.1) +
  ggplot2::geom_line(ggplot2::aes(y=dt$Mean4Years, color = "Mean"), linetype="longdash", size=0.6) +
  ggplot2::scale_x_date(date_labels = "%b", date_breaks= "1 month", expand = c(0, 0)) +
  ggplot2::scale_y_continuous(expand = c(0, 0), breaks=FIGTSBREAKS, limits = c(0,max(FIGTSBREAKS))) +
  ggplot2::xlab("Month") +
  ggplot2::ylab("Number of cases") +
  ggplot2::scale_colour_manual("",
                      values=c("Year"= "#69AE23", "Mean"="#767171"),
                      labels=c("year"=as.character(year), "Mean"=paste("Mean (",min(data[[yearvar]]),"-",year-1, ")",sep="") ) ) +
  ggplot2::scale_fill_manual("",values="grey80") +
  ggplot2::theme(axis.text= ggplot2::element_text(size=8, family = "Tahoma"), axis.title=ggplot2::element_text(size=9)) +    #Axis text style
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank()) +  #Empty backgroud
  ggplot2::theme(axis.line = ggplot2::element_line(colour = "#767171") ) +
  ggplot2::theme(legend.position = "right", legend.title =ggplot2::element_blank(), legend.text =ggplot2::element_text(size=8, family = "Tahoma"),
        legend.key=ggplot2::element_blank(), legend.key.width = ggplot2::unit(0.8, "cm"))
return(p1)
  }
