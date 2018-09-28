#' Capitalise the first letter
#'
#' Capitalise the first letter
#'
#' @param str character string to capitalise as a title
#' @return character string
#' @export
#'
toCapTitle <- function(str) {
  paste(toupper(substring(str, 1,1)), substring(str, 2),
        sep="", collapse=" ")
}



#' Filter disease parameters
#'
#' Filter disease parameters
#'
#' @param dis character string containing disease code
#' @param reportParameters dataset of parameters for the report
#' @return row corresponding to the parameters of the selected disease
#' @export
#'
filterDisease <- function(dis, reportParameters) {

  if(missing(reportParameters)) { reportParameters <- EpiReport::AERparams }

  reportParameters <- dplyr::filter(reportParameters, reportParameters$HealthTopic == dis)
  if( nrow(reportParameters) == 0 ) {
    stop(paste('The disease "', dis, '" is not described in the parameter table.
               The report cannot be produced.'))
  }
  return(reportParameters)
}



#' Clean the MeasureCode variable
#'
#' Clean the MeasureCode variable
#'
#' @param var variable to clean
#' @return cleaned variable
#' @export
#'
cleanMeasureCode <- function(var) {


}











