#' Capitalise first letter
#'
#' Capitalise the first letter of a character string in order to use it as title
#'
#' @param str character string to capitalise as a title
#' @return character string
#'
#' @examples
#' my_title <- "number of salmonellosis cases by age group"
#' toCapTitle(my_title)
#'
#' @export
#'
toCapTitle <- function(str) {
  paste(toupper(substring(str, 1,1)), substring(str, 2),
        sep="", collapse=" ")
}



#' Filter disease parameters
#'
#' Filter the table of parameters for the report on the given disease
#'
#' @param dis character string, disease code
#' @param reportParameters dataset of parameters for the report
#' (default \code{AERparams})
#'
#' @return dataframe with one row (from the \code{AERparams} dataframe)
#' corresponding to the parameters of the selected disease
#'
#' @examples
#' disease <- "SALM"
#' reportParameters <- EpiReport::AERparams
#' reportParameters <- filterDisease(disease, reportParameters)
#'
#' @seealso \code{\link{AERparams}}
#'
#' @export
#'
filterDisease <- function(dis, reportParameters) {

  if(missing(reportParameters)) { reportParameters <- EpiReport::AERparams }

  reportParameters <- dplyr::filter(reportParameters, reportParameters$HealthTopic == dis)
  if( nrow(reportParameters) == 0 ) {
    stop(paste('The disease "', dis,
               '" is not described in the parameter table. The report cannot be produced.'))
  }
  return(reportParameters)
}



#' Clean the MeasureCode variable
#'
#' Clean the MeasureCode variable and replace the specific codes with the generic ones \cr
#' (e.g. \code{ACCUTE.AGE_GENDER.RATE} will be replaced by \code{CONFIRMED.AGE_GENDER.RATE})
#'
#' \itemize{
#'     \item{\code{ALL.COUNT}} will replace the following codes:
#'     \itemize{
#'         \item \code{ALL.DOMESTIC.COUNT}
#'         \item \code{AGELT1.COUNT}
#'     }
#'     \item{\code{ALL.RATE}} will replace the following codes:
#'     \itemize{
#'         \item \code{ALL.DOMESTIC.AGE.RATE}
#'     }
#'     \item{\code{ALL.AGE.RATE}} will replace the following codes:
#'     \itemize{
#'         \item \code{ALL.DOMESTIC.AGE.RATE}
#'     }
#'     \item{\code{ALL.AGESTANDARDISED.RATE}} will replace the following codes:
#'     \itemize{
#'         \item \code{ALL.DOMESTIC.AGESTANDARDISED.RATE}
#'     }
#'     \item{\code{CONFIRMED.COUNT}} will replace the following codes:
#'     \itemize{
#'         \item \code{ALL.LABCONFIRMED.COUNT}
#'         \item \code{CONFIRMED.LABCONFIRMED.COUNT}
#'         \item \code{CONFIRMED.AGELT1.COUNT}
#'         \item \code{TYPHOID.COUNT}
#'     }
#'     \item{\code{CONFIRMED.RATE}} will replace the following codes:
#'     \itemize{
#'         \item \code{CONFIRMED.LABCONFIRMED.RATE}
#'         \item \code{CONFIRMED.AGELT1.RATE}
#'         \item \code{TYPHOID.RATE}
#'     }
#'     \item{\code{CONFIRMED.AGESTANDARDISED.RATE}} will replace the following codes:
#'     \itemize{
#'         \item \code{CONFIRMED.LABCONFIRMED.AGESTANDARDISED.RATE}
#'     }
#'     \item{\code{CONFIRMED.AGE_GENDER.RATE}} will replace the following codes:
#'     \itemize{
#'         \item \code{CONFIRMED.LABCONFIRMED.AGE_GENDER.RATE}
#'         \item \code{TYPHOID.AGE_GENDER.RATE}
#'         \item \code{ACCUTE.AGE_GENDER.RATE}
#'     }
#'
#'
#' }
#'
#' @param var character string vector variable, variable to clean
#'
#' @return cleaned vector variable
#'
#' @examples
#' x <- EpiReport::SALM2016
#' x$MeasureCode <- cleanMeasureCode(x$MeasureCode)
#'
#' @seealso \code{\link{SALM2016}}
#'
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

  # ---- Recoding ALL.AGE.RATE
  valuesToReplace = c("ALL.DOMESTIC.AGE.RATE")
  var <- ifelse(var %in% valuesToReplace,
                "ALL.AGE.RATE",
                var)

  # ---- Recoding AGESTANDARDISED.RATE
  valuesToReplace = c("ALL.DOMESTIC.AGESTANDARDISED.RATE")
  var <- ifelse(var %in% valuesToReplace,
                "ALL.AGESTANDARDISED.RATE",
                var)

  return(var)
}











