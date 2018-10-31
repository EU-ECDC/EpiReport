#' Order 'quasinumerical' categorical vectors (increasing order)
#'
#' A function to order 'quasinumerical' (i.e. categorical with values
#' such as "15-30" or "<18") integer vectors into increasing order.
#' Currently handles away the following non-numerical characters
#' "-", ">", "<", ">=", "<=", "+".
#'
#' @param x character vector with 'quasinumerical' values
#' @author Tommi Karki
#' @keywords order
#' @export
#' @seealso Used in \code{\link{getAgeGender}} and \code{\link{plotAgeGender}} / \code{\link{plotAge}}
#' @examples
#' age1 <- c("<1", "1-15", "16-25", ">65", "26-65")
#' age2 <- c("0-4", "5-10", ">65", "25-64", "11-25")
#' age3 <- c("5-10", ">65", "25-64", "11-25", "<=4")
#' age4 <- c(">=65", "<18", "18-64")
#' age5 <- c("5-10", "+65", "25-64", "11-25", "0-4")
#'
#' age1
#' orderQuasinum(age1)
#' age2
#' orderQuasinum(age2)
#' age3
#' orderQuasinum(age3)
#' age4
#' orderQuasinum(age4)
#' age5
#' orderQuasinum(age5)
#'

orderQuasinum <- function(x){
  y <- strsplit(x, "-")
  y <- rapply(y, function(x) x[1], how = "unlist")
  y <- suppressWarnings(
    ifelse(grepl(">=", y), as.numeric(gsub(">=", "", y)),
           ifelse(grepl("<=", y), as.numeric(gsub("<=", "", y)),
                  ifelse((grepl(">", y) & !grepl("=", y)), as.numeric(gsub(">", "", y))+0.01,
                         ifelse((grepl("<", y) & !grepl("=", y)), as.numeric(gsub("<", "", y))-0.01,
                                ifelse(grepl("\\+", y), as.numeric(gsub("\\+", "", y))+0.01,  y)
                         )))))

  return(x[order(as.numeric(y))])
}


