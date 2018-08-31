#' A function to order 'quasinumerical' (i.e. categorical with values such as "15-30" or "<18") integer vectors into
#' increasing order. Currently handles away the following non-numerical characters "-", ">", "<", ">=", "<=".
#'
#' @param x A character vector with 'quasinumerical' values
#' @author Tommi Karki
#' @keywords order
#' @export
#' @examples
#' age1 <- c("<1", "1-15", "16-24", ">65", "25-65")

#' age2 <- c("0-4", "5-10", ">65", "25-64", "11-25")
#' age3 <- c("5-10", ">65", "25-64", "11-25", "<=4")
#' age4 <- c(">=65", "<18", "18-64")
#'
#' age1
#' order_quasinum(age1)

#' age2
#' order_quasinum(age2)

#' age3
#' order_quasinum(age3)

#' age4
#' order_quasinum(age4)


order_quasinum <- function(x){
  y <- str_split(x, "-")
  y <- rapply(y, function(x) x[1], how = "unlist")
  y <- suppressWarnings(ifelse(grepl(">=", y), as.numeric(gsub(">=", "", y)),
              ifelse(grepl("<=", y), as.numeric(gsub("<=", "", y)),
                     ifelse((grepl(">", y) & !grepl("=", y)), as.numeric(gsub(">", "", y))+0.01,
                            ifelse((grepl("<", y) & !grepl("=", y)), as.numeric(gsub("<", "", y))-0.01, y)
  ))))

return(x[order(as.numeric(y))])
}


