#' Colour palettes following the March 2018 ECDC guidelines for presentation of surveillance data
#'
#' Full document: European Centre for Disease Prevention and Control. Guidelines for presentation of surveillance data.
#' Stockholm: ECDC; 2018. Available from: \href{https://ecdc.europa.eu/en/publications-data/guidelines-presentation-surveillance-data}{Guidelines for presentation of surveillance data}
#'
#' @param col_scale Selected colour scale, defaults to 'green'. Select from 'green', 'blue', 'red', 'grey', 'qual(itative)' or 'hot(cold)'
#' @param n Number of colours from each colour scale, apart from grey, in order indicated in the guidelines. Defaults to one colour, apart from two colours for the hotcold scale, 
#' max 7-8 colours for each scale. To select grey shades, use the argument grey_shade; to select number of hot (warm) colours in the hotcold scale, use the argument hot_cols.
#' @param grey_shade Only used with grey colours, defaults to 'medium'. Selected shade(s) of grey in selected order; c('light', 'mediumlight','medium','mediumdark','dark'). Overrides given number of colours (n).
#' @param hot_cols Selected number of hot (warm) colours in the hotcold colour scale, use only for hotcold. Must be smaller than the total number of colours (n).
#' Defaults to floored half of total hotcold colours.
#' @author Tommi Karki
#' @keywords colourscales
#' @importFrom "grDevices" "rgb"
#' @export
#' @examples
#' # Select three first green colours
#' EcdcColors("green", n=3)
#' 
#' # Select two first qualitative colours
#' EcdcColors("qual", n=2)
#' 
#' # Select seven red colours
#' EcdcColors("red", n=7)
#' 
#' EcdcColors("grey", grey_shade = c("mediumlight", "dark"))
#' 
#' # Use in a graph
#' # Dummy data
#' mydat <- data.frame(ID = c(seq(1,10,1)),
#' Gender = c(rep(c("F", "M"),5)))
#' barplot(table(mydat$Gender), col = EcdcColors(col_scale = "qual", n=2))
#' 
#' # Hot-cold colour scale
#' barplot(c(1:4), col = EcdcColors(col_scale = "hotcold", n = 4, hot_cols = 2))
EcdcColors <- function(col_scale = "green", n = NULL, grey_shade = NULL,
                     hot_cols = NULL){
  
  return(SurvColors(col_scale = col_scale, n = n, grey_shade = grey_shade,
                    hot_cols = hot_cols))
}
