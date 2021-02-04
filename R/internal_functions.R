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



#' Replace a plot at a bookmark location
#'
#' Replace a plot at a bookmark location saving it as a PNG file in a temporary
#' folder. \cr
#' A bookmark will be considered as valid if enclosing words within a paragraph;
#' i.e., a bookmark along two or more paragraphs is invalid,
#' a bookmark set on a whole paragraph is also invalid,
#' but bookmarking few words inside a paragraph is valid.
#'
#' @param doc a docx device
#' @param gg a ggplot object or any object that can be printed in grDevices::png()
#' @param bookmark bookmark id
#' @param width the width of the device in inches
#' @param height the height of the device.
#'
#' @return doc
#'
#' @examples
#' doc <- officer::read_docx(path = file.path(system.file(package = "EpiReport"),
#'                                            "template/AER_template.docx" ))
#' p <- EpiReport::getTrend()
#' doc <- EpiReport::body_replace_gg_at_bkm(doc = doc,
#'                                          gg = p,
#'                                          bookmark = "TS_TREND",
#'                                          width = 6,
#'                                          height = 3)
#'
#' @export
#'
body_replace_gg_at_bkm <- function(doc, gg, bookmark,
                                   width = 6,
                                   height = 3) {
  temp <- tempdir()
  grDevices::png(paste(temp, "\\temp_ggplot.png", sep = ""), width = width, height = height, units = "in", res = 500)
  print(gg)
  grDevices::dev.off()
  doc <- officer::body_replace_img_at_bkm(x = doc,
                                          bookmark = bookmark,
                                          value = officer::external_img(src = paste(temp, "\\temp_ggplot.png", sep = ""),
                                                                        width = width,
                                                                        height = height))
  file.remove(paste(temp, "\\temp_ggplot.png", sep = ""))
  return(doc)
}










