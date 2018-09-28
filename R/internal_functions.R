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
#' @examples
#' x <- EpiReport::SALM2016
#' x$MeasureCode <- cleanMeasureCode(x$MeasureCode)
#' @export
#'
cleanMeasureCode <- function(var) {

  # ---- Recoding CONFIRMED.AGE_GENDER.RATE
  valuesToReplace <- c("CONFIRMED.LABCONFIRMED.AGE_GENDER.RATE",
                      "TYPHOID.AGE_GENDER.RATE",
                      "ACCUTE.AGE_GENDER.RATE")
  var <- ifelse(var %in% valuesToReplace,
                "CONFIRMED.AGE_GENDER.RATE",
                var)


  # ---- Recoding CONFIRMED.AGESTANDARDISED.RATE
  valuesToReplace <- c("CONFIRMED.LABCONFIRMED.AGESTANDARDISED.RATE")
  var <- ifelse(var %in% valuesToReplace,
                "CONFIRMED.AGESTANDARDISED.RATE",
                var)


  # ---- Recoding CONFIRMED.RATE
  valuesToReplace = c("CONFIRMED.LABCONFIRMED.RATE",
                      "CONFIRMED.AGELT1.RATE",
                      "TYPHOID.RATE")
  var <- ifelse(var %in% valuesToReplace,
                "CONFIRMED.RATE",
                var)


  # ---- Recoding CONFIRMED.COUNT
  valuesToReplace = c("ALL.LABCONFIRMED.COUNT",
                      "CONFIRMED.LABCONFIRMED.COUNT",
                      "CONFIRMED.AGELT1.COUNT",
                      "TYPHOID.COUNT")
  var <- ifelse(var %in% valuesToReplace,
                "CONFIRMED.COUNT",
                var)


  # ---- Recoding ALL.COUNT
  valuesToReplace = c("ALL.DOMESTIC.COUNT",
                      "AGELT1.COUNT")
  var <- ifelse(var %in% valuesToReplace,
                "ALL.COUNT",
                var)


  # ---- Recoding ALL.RATE
  valuesToReplace = c("ALL.DOMESTIC.RATE")
  var <- ifelse(var %in% valuesToReplace,
                "ALL.RATE",
                var)



  # ---- Recoding AGESTANDARDISED.RATE
  valuesToReplace = c("ALL.DOMESTIC.AGESTANDARDISED.RATE")
  var <- ifelse(var %in% valuesToReplace,
                "AGESTANDARDISED.RATE",
                var)

  return(var)
}











