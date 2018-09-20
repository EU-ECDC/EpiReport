#' Get the seasonal distribution graph
#'
#' Function returning the plot
#'
#' @param x dataset
#' @param disease character string, disease name
#' @param year numeric, year to produce the report for
#' @param reportParameters dataset of parameters for the report
#' @param index figure number
#' @param doc Word document
#' @return Word doc a ggplot2 preview
#' @export
getSeason <- function(x, #to improve with variables??
                      disease = "SALM", year = 2016,
                      reportParameters, index = 1, doc){

  ## ----
  ## Setting default arguments if missing
  ## ----

  if(missing(x)) { x <- EpiReport::SALM2016 }
  if(missing(disease)) { disease <- "SALM" }
  if(missing(year)) { year <- 2016 }
  if(missing(reportParameters)) { reportParameters <- EpiReport::AERparams }
  if(missing(index)) { index <- 1 }


  ## ----
  ## Filtering
  ## ----
  # x <- dplyr::filter(x, x$HealthTopic == disease)
  # if( nrow(x) == 0 ) {
  #   stop(paste('The dataset does not include the selected disease "', disease, '".'))
  #   }
  reportParameters <- dplyr::filter(reportParameters, reportParameters$HealthTopic == disease)
  if( nrow(reportParameters) ==0 ) {
    stop(paste('The disease "', disease, '" is not described in the parameter table.
               The report cannot be produced.'))
  }


  ## ----
  ## No Seasonal plot for this disease
  ## ----

  if(reportParameters$TSSeasonalityGraphUse == "Y") {
    p <- suppressWarnings(ggplot2::ggplot(x, ggplot2::aes(x$GeoCode, x$XValue))) +
      suppressWarnings(ggplot2::geom_bar(stat="identity"))

    if(missing(doc)) {
      p
    } else {
      officer::cursor_bookmark(doc, id = "TS_SEASON_BOOKMARK")
      doc <- officer::body_add_gg( doc,
                                   value = p,
                                   width = 6,
                                   height = 3)
    }

  }


  ## ----
  ## No Seasonal plot for this disease
  ## ----

  if(reportParameters$TSSeasonalityGraphUse == "N") {
    message(paste('According to the parameter table \'AERparams\', this disease "',
                  disease, '" does not include any seasonal graph in the AER report.', sep = ""))
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
